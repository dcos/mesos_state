%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2016 2:56 AM
%%%-------------------------------------------------------------------
-module(mesos_state_client).
-author("sdhillon").

-include("mesos_state.hrl").
-include("mesos_state_internal.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque mesos_agent_state() :: map().

-export_type([mesos_agent_state/0]).

%% API
-export([
  parse_response/1,
  flags/1, pid/1, tasks/1, id/1,
  slaves/1, frameworks/1
]).

-spec(parse_response(Body :: binary()) -> {ok, mesos_agent_state()}).
parse_response(Body) ->
  ParsedBody = jiffy:decode(Body, [return_maps]),
  %% The labels should be relatively tiny -- just let them be atoms
  ParsedBody0 = atom_labels(ParsedBody),
  parse_response2(ParsedBody0).

-spec atom_labels(jiffy:json_value()) -> jiffy:json_value().
atom_labels(Term) when is_map(Term) ->
  maps:fold(fun (Key, Value, Acc) ->
    Acc#{binary_to_atom(Key, latin1) => atom_labels(Value)}
  end, #{}, Term);
atom_labels(Term) when is_list(Term) ->
  lists:map(fun atom_labels/1, Term);
atom_labels(Term) ->
  Term.

parse_response2(ParsedBody) ->
  {ok, ParsedBody}.

flags(_ParsedBody = #{flags := Flags}) ->
  Flags.

pid(_ParsedBody = #{pid := Pid}) ->
  [NameBin, IPPort] = binary:split(Pid, <<"@">>),
  [IPBin, PortBin] = binary:split(IPPort, <<":">>),
  Port = binary_to_integer(PortBin),
  IPString = binary_to_list(IPBin),
  {ok, IP} = inet:parse_ipv4_address(IPString),
  #libprocess_pid{name = NameBin, port = Port, ip = IP}.

is_slave(ParsedBody) ->
  #libprocess_pid{name = NameBin} = pid(ParsedBody),
  case binary:match(NameBin, [<<"slave">>]) of
    nomatch ->
      false;
    _ ->
      true
  end.


slave(Slave) ->
  #slave{
    slave_id = maps:get(id, Slave),
    hostname = maps:get(hostname, Slave),
    pid = pid(Slave)
  }.

-spec(slaves(mesos_agent_state()) -> [slave()]).
slaves(ParsedBody) ->
  case is_slave(ParsedBody) of
    false ->
      Slaves = maps:get(slaves, ParsedBody),
      lists:map(fun slave/1, Slaves);
    true ->
      [slave(ParsedBody)]
  end.

find_slave(Id, Slaves) ->
  case lists:filter(fun(#slave{slave_id = SlaveId}) -> SlaveId == Id end, Slaves) of
    [Slave] ->
      {ok, Slave};
    _ ->
      error
  end.


%rg.listenerRecord(listener, ns)
%rg.masterRecord(domain, masters, sj.Leader)
-spec(frameworks(mesos_agent_state()) -> [framework()]).
frameworks(_ParsedBody = #{frameworks := Frameworks}) ->
  lists:map(fun framework/1, Frameworks).
-spec(framework(map()) -> framework()).
framework(Framework) ->
  Pid =
    case Framework of
      #{pid := _} ->
        pid(Framework);
      _ ->
        undefined
    end,
  #framework{
    id =  maps:get(id, Framework),
    name = maps:get(name, Framework),
    pid = Pid,
    hostname = maps:get(hostname, Framework),
    webui_url = maps:get(webui_url, Framework, undefined)
  }.


-spec(tasks(mesos_agent_state()) -> [task()]).
tasks(ParsedBody = #{frameworks := Frameworks, completed_frameworks := CompletedFrameworks}) ->
  Slaves = slaves(ParsedBody),
  Tasks1 = frameworks(Frameworks, Slaves, ParsedBody, []),
  frameworks(CompletedFrameworks, Slaves, ParsedBody, Tasks1).


frameworks([], _Slaves, _ParsedBody, TasksAcc) ->
  TasksAcc;

frameworks([Framework = #{executors := Executors, tasks := Tasks}|Frameworks], Slaves, ParsedBody, TasksAcc) ->
  TasksAcc1 = executors(Executors, Framework, Slaves, ParsedBody, TasksAcc),
  TasksAcc2 = tasks(Tasks, Framework, Slaves, ParsedBody, TasksAcc1),
  frameworks(Frameworks, Slaves, ParsedBody, TasksAcc2);
frameworks([Framework = #{executors := Executors}|Frameworks], Slaves, ParsedBody, TasksAcc) ->
  TasksAcc1 = executors(Executors, Framework, Slaves, ParsedBody, TasksAcc),
  frameworks(Frameworks, Slaves, ParsedBody, TasksAcc1);
frameworks([Framework = #{tasks := Tasks}|Frameworks], Slaves, ParsedBody, TasksAcc) ->
  TaskAcc1 = tasks(Tasks, Framework, Slaves, ParsedBody, TasksAcc),
  frameworks(Frameworks, Slaves, ParsedBody, TaskAcc1).


executors([], _Framework, _Slaves, _ParsedBody, Tasks) ->
  Tasks;
executors([_Executor = #{tasks := Tasks, completed_tasks := CompletedTasks}|Executors], Framework, Slaves, ParsedBody, TasksAcc) ->
  TasksAcc1 = tasks(Tasks, Framework, Slaves, ParsedBody, TasksAcc),
  TasksAcc2 = tasks(CompletedTasks, Framework, Slaves, ParsedBody, TasksAcc1),
  executors(Executors, Framework, Slaves, ParsedBody, TasksAcc2);
executors([_Executor = #{tasks := Tasks}|Executors], Framework, Slaves, ParsedBody, TasksAcc) ->
  TasksAcc1 = tasks(Tasks, Framework, Slaves, ParsedBody, TasksAcc),
  executors(Executors, Framework, Slaves, ParsedBody, TasksAcc1);
executors([_Executor|Executors], Framework, Slaves, ParsedBody, TasksAcc) ->
  executors(Executors, Framework, Slaves, ParsedBody, TasksAcc).

-ifdef(TEST).

tasks([], _Framework, _Slave, _ParsedBody, TasksAcc) ->
  TasksAcc;
tasks([Task | Tasks], Framework, Slave, ParsedBody, TasksAcc) ->
  TaskRecord = task(Task, Framework, Slave),
  tasks(Tasks, Framework, Slave, ParsedBody, [TaskRecord | TasksAcc]).
-else.
tasks([], _Framework, _Slave, _ParsedBody, TasksAcc) ->
  TasksAcc;
tasks([Task | Tasks], Framework, Slave, ParsedBody, TasksAcc) ->
  case catch task(Task, Framework, Slave) of
    %% Don't bail on failed tasks
    {'EXIT', Reason} ->
      error_logger:error_msg("Failed to parse task: ~p", [Reason]),
      tasks(Tasks, Framework, Slave, ParsedBody, TasksAcc);
    TaskRecord ->
      tasks(Tasks, Framework, Slave, ParsedBody, [TaskRecord | TasksAcc])
  end.
-endif.

task(Task, Framework, Slaves) ->
  SlaveID = maps:get(slave_id, Task),
  {ok, Slave} = find_slave(SlaveID, Slaves),
  #task{
    id = maps:get(id, Task),
    labels = task_labels(maps:get(labels, Task, [])),
    name = maps:get(name, Task),
    executor_id = maps:get(executor_id, Task),
    state = task_state(maps:get(state, Task)),
    statuses = task_statuses(maps:get(statuses, Task, []), []),
    resources = resources(maps:get(resources, Task)),
    container = container(maps:get(container, Task, undefined)),
    discovery = discovery(maps:get(discovery, Task, undefined)),
    slave = Slave,
    framework = framework(Framework)
  }.

task_labels(Labels) ->
  Proplist = [{Key, Value} || #{key := Key, value := Value} <- Labels],
  maps:from_list(Proplist).


task_state(<<"TASK_RUNNING">>) ->
  running;
task_state(<<"TASK_STAGING">>) ->
  staging;
task_state(<<"TASK_STARTING">>) ->
  starting;
task_state(<<"TASK_FINISHED">>) ->
  finished;
task_state(<<"TASK_FAILED">>) ->
  failed;
task_state(<<"TASK_KILLED">>) ->
  killed;
task_state(<<"TASK_KILLING">>) ->
  killing;
task_state(<<"TASK_LOST">>) ->
  lost;
task_state(<<"TASK_ERROR">>) ->
  error.

resources(Resources) ->
  maps:map(fun resource/2, Resources).

resource(cpus, Value) ->
  Value;
resource(gpus, Value) ->
  Value;
resource(mem, Value) ->
  Value;
resource(disk, Value) ->
  Value;
resource(ports, PortsBin) ->
  PortsStr = binary_to_list(PortsBin),
  range_to_resource(PortsStr);
resource(_, Value) when is_number(Value) ->
  Value.


range_to_resource(String) ->
  String1 = string:strip(String, left, $[),
  String2 = string:strip(String1, right, $]),
  Ranges = [range_to_resource2(Range) || Range <- string:tokens(String2, ", ")],
  lists:flatten(Ranges).

range_to_resource2(Range) ->
  [StartStr, EndStr] = string:tokens(Range, "-"),
  Start = list_to_integer(StartStr),
  End = list_to_integer(EndStr),
  lists:seq(Start, End).

container(undefined) ->
  undefined;
container(#{docker := Docker, type := <<"DOCKER">>}) ->
  #container{type = docker, docker = docker(Docker)};
container(#{type := <<"MESOS">>} = Mesos) ->
  #container{type = mesos, network_infos = network_infos(Mesos)}.

network_infos(#{network_infos := NetworkInfos}) -> 
  network_infos(NetworkInfos, []);
network_infos(_) -> [].

force_pull_image(#{force_pull_image := ForcePullImage}) -> ForcePullImage;
force_pull_image(_) -> false.

network_type(BinString) when is_binary(BinString) ->
    String = binary_to_list(BinString),
    case string:to_lower(String) of
        "bridge" -> 'bridge';
        "host" -> 'host';
        "user" -> 'user'
    end.

docker(#{image := Image, network := Network} = Docker) ->
    #docker{
       force_pull_image = force_pull_image(Docker),
       image = Image,
       network = network_type(Network),
       port_mappings = port_mappings(Docker)}.

task_statuses([], Acc) ->
  Acc1 = lists:keysort(#task_status.timestamp, Acc),
  lists:reverse(Acc1);
task_statuses([TaskStatus|Rest], Acc) ->
  task_statuses(Rest, [task_status(TaskStatus)|Acc]).

task_status(_TaskStatus = #{timestamp := Timestamp, state := State, container_status := ContainerStatus, healthy := Healthy}) ->
  #task_status{
    timestamp = Timestamp,
    container_status = container_status(ContainerStatus),
    state = task_state(State),
    healthy = Healthy
  };
task_status(_TaskStatus = #{timestamp := Timestamp, state := State, container_status := ContainerStatus}) ->
  #task_status{
    timestamp = Timestamp,
    container_status = container_status(ContainerStatus),
    state = task_state(State),
    healthy = undefined
  };
task_status(_TaskStatus = #{timestamp := Timestamp, state := State}) ->
  #task_status{
    timestamp = Timestamp,
    container_status = undefined,
    state = task_state(State),
    healthy = undefined
  }.


container_status(_ContainerStatus = #{cgroup_info := CGroupInfo, network_infos := NetworkInfos}) ->
  #container_status{
    cgroup_info = cgroup_info(CGroupInfo),
    network_infos = network_infos(NetworkInfos, [])
  };
container_status(_ContainerStatus = #{network_infos := NetworkInfos}) ->
  #container_status{
    cgroup_info = undefined,
    network_infos = network_infos(NetworkInfos, [])
  };
container_status(_) ->
  #container_status{
    cgroup_info = undefined,
    network_infos = []
  }.

cgroup_info(#{net_cls := #{classid := ClassID}}) ->
  #cgroup_info{net_cls = #net_cls{classid = ClassID}}.

network_infos([], Acc) ->
  Acc;
network_infos([#{ip_addresses := IPAddresses, port_mappings := PortMappings}|Rest], Acc) ->
  NetworkInfo1 = #network_info{ip_addresses = ip_addresses(IPAddresses, []),
                               port_mappings = port_mappings(PortMappings, [])},
  network_infos(Rest, [NetworkInfo1|Acc]);
network_infos([#{ip_addresses := IPAddresses}|Rest], Acc) ->
  NetworkInfo1 = #network_info{ip_addresses = ip_addresses(IPAddresses, [])},
  network_infos(Rest, [NetworkInfo1|Acc]);
network_infos([#{port_mappings := PortMappings}|Rest], Acc) ->
  NetworkInfo1 = #network_info{ip_addresses = [], port_mappings = port_mappings(PortMappings, [])},
  network_infos(Rest, [NetworkInfo1|Acc]);
network_infos([_|Rest], Acc) ->
  network_infos(Rest, Acc).

ip_addresses([], Acc) ->
  Acc;
ip_addresses([#{ip_address := IPAddressBin}|Rest], Acc) ->
  IPAddressStr = binary_to_list(IPAddressBin),
  case inet:parse_address(IPAddressStr) of
    {ok, IP} ->
      ip_addresses(Rest, [#ip_address{ip_address = IP}|Acc]);
    %% Raise this error somehow?
    _Else ->
      ip_addresses(Rest, Acc)
   end;
ip_addresses([_|Rest], Acc) ->
  ip_addresses(Rest, Acc).

port_mappings(#{port_mappings := PortMappings}) ->
  port_mappings(PortMappings, []);
port_mappings(_) -> [].

port_mappings([], Acc) ->
  Acc;
port_mappings([#{container_port := ContainerPort, host_port := HostPort, protocol := Protocol}|Rest], Acc) ->
  PortMapping1 = #port_mapping{protocol = protocol(Protocol), host_port = HostPort, container_port = ContainerPort},
  port_mappings(Rest, [PortMapping1|Acc]);
port_mappings([_|Rest], Acc) ->
  port_mappings(Rest, Acc). 


-spec(id(mesos_agent_state()) -> binary()).
id(_ParsedBody = #{id := ID}) ->
  ID.

discovery(undefined) ->
  undefined;
discovery(Discovery) ->
  #discovery{
    name = maps:get(name, Discovery),
    ports = discovery_ports(ports_from_discovery(Discovery), [])
  }.


discovery_ports([], Acc) ->
    Acc;
%% Skip "client" protocol re: https://github.com/mesosphere/etcd-mesos/blob/master/scheduler/scheduler.go#L977
discovery_ports([#{protocol := <<"client">>}|RestPorts], Acc) ->
    discovery_ports(RestPorts, Acc);
discovery_ports([Port = #{number := Number, protocol := Protocol}|RestPorts],
                Acc) ->
    PortRecord = #mesos_port{
       name = maps:get(name, Port, undefined),
       number = Number,
       protocol = protocol(Protocol),
       labels = port_labels(Port)
      },
    discovery_ports(RestPorts, [PortRecord|Acc]).

port_labels(_Port = #{labels := #{labels := Labels}}) ->
  Proplist = [{Key, Value} || #{key := Key, value := Value} <- Labels],
  maps:from_list(Proplist);
port_labels(_) ->
  #{}.

ports_from_discovery(#{ports := #{ports := Ports}}) ->
  Ports;
ports_from_discovery(_) ->
  [].

protocol(BinString) when is_binary(BinString) ->
    String = binary_to_list(BinString),
    case string:to_lower(String) of
        "tcp" -> tcp;
        "udp" -> udp
    end.

-ifdef(TEST).

-endif.
