%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2016 3:30 AM
%%%-------------------------------------------------------------------
-author("sdhillon").

-type task_state() :: staging | starting | running | finished | failed | killed | lost | error.
-type framework_id() :: binary().
-type framework_name() :: binary().
-type task_id() :: binary().
-type executor_id() :: binary().
-type task_name() :: binary().
-type labels() :: map(). %% Are labels unique?
-type resource() :: float() | list(non_neg_integer()).
-type resource_name() :: cpus | ports | mem | disk.
-type role() :: binary().
-type slave_id() :: binary().
-type hostname() :: binary().
-type protocol() :: tcp | udp.

-record(mesos_port, {
  number :: inet:port_number(),
  name :: binary() | undefined,
  protocol :: protocol(),
  labels :: labels()
}).

%% I made a mistake. port() is a built-in type.
-type mesos_port() :: #mesos_port{}.

-record(discovery, {
  name :: binary(),
  ports :: [mesos_port()]
}).
-type discovery() :: #discovery{}.

-record(ip_address, {
  ip_address :: inet:ip4_address()
}).
-type ip_address() :: #ip_address{}.

-record(network_info, {
  ip_addresses :: [ip_address()]
}).
-type network_infos() :: #network_info{}.

-record(net_cls, {
  classid :: non_neg_integer()
}).
-type net_cls() :: #net_cls{}.

-record(cgroup_info, {
  net_cls :: net_cls()
}).
-type cgroup_info() :: #cgroup_info{}.

-record(container_status, {
  network_infos :: [network_infos()],
  cgroup_info :: cgroup_info() | undefined
}).
-type container_status() :: container_status().

-record(task_status, {
  container_status :: container_status() | undefined,
  timestamp :: float(),
  state :: task_state(),
  healthy :: boolean() | undefined
}).
-type task_status() :: #task_status{}.

-record(port_mapping, {
  container_port :: inet:port_number(),
  host_port :: inet:port_number(),
  protocol :: protocol()
}).
-type port_mapping() :: #port_mapping{}.

-record(docker, {
  force_pull_image :: boolean(),
  image :: binary(),
  network :: 'bridge' | 'host' | 'user',
  port_mappings :: [port_mapping()]
}).
-type docker() :: #docker{}.

-record(container, {
  docker :: docker() | undefined,
  type :: 'docker' | 'mesos'
}).
-type container() :: #container{}.


-record(libprocess_pid, {
  name :: binary(),
  ip :: inet:ip4_address(),
  port :: non_neg_integer()
}).

-type libprocess_pid() :: #libprocess_pid{}.


-record(slave, {
  slave_id :: slave_id(),
  pid :: libprocess_pid(),
  hostname :: hostname()
}).

-type slave() :: #slave{}.


-record(task, {
  id :: task_id(),
  labels :: labels(),
  name :: task_name(),
  resources :: map(),
  executor_id :: executor_id(),
  state :: task_state(),
  %% Will be ordered with the newest timestamp first
  statuses :: [task_status()],
  discovery :: undefined | discovery(),
  container :: undefined | container(),
  slave :: slave(),
  framework :: framework()
  %% Should we add executor information
  %% -executor name
  %% -executor id? (which is different than the executor id on
}).

-type task() :: #task{}.

-record(framework, {
  id :: framework_id(),
  name :: framework_name(),
  pid :: libprocess_pid() | undefined,
  hostname :: hostname(),
  webui_url :: binary() | undefined
}).

-type framework() :: #framework{}.


