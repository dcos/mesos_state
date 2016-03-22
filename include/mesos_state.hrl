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

-record(port, {
  number :: inet:port_number(),
  name :: binary() | undefined,
  protocol :: protocol(),
  labels :: labels()
}).


-record(discovery, {
  name :: binary(),
  ports :: [port()]
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
  cgroup_info :: cgroup_info()
}).
-type container_status() :: container_status().

-record(task_status, {
  container_status :: container_status(),
  timestamp :: float(),
  state :: task_state(),
  healthy :: boolean() | undefined
}).
-type task_status() :: #task_status{}.

-record(docker, {
  force_pull_image :: boolean(),
  image :: binary(),
  network :: 'bridge' | 'host'
}).
-type docker() :: #docker{}.

-record(container, {
  docker :: docker(),
  type :: 'docker'
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
  framework_id :: framework_id(),
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
  framework_name :: framework_name(),
  slave :: slave()
  %% Should we add executor information
  %% -executor name
  %% -executor id? (which is different than the executor id on
}).



