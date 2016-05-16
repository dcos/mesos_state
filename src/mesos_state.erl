%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2016 4:51 PM
%%%-------------------------------------------------------------------
-module(mesos_state).
-author("sdhillon").

-include_lib("kernel/include/inet.hrl").

%% API
-export([ip/0]).

-spec(ip() -> inet:ip4_address()).
ip() ->
    case dcos_ip() of
        false ->
            infer_ip();
        IP ->
            IP
    end.

infer_ip() ->
    ForeignIP = foreign_ip(),
    {ok, Socket} = gen_udp:open(0),
    inet_udp:connect(Socket, ForeignIP, 4),
    {ok, {Address, _LocalPort}} = inet:sockname(Socket),
    gen_udp:close(Socket),
    Address.

foreign_ip() ->
    case inet:gethostbyname("leader.mesos") of
        {ok, Hostent} ->
            [Addr | _] = Hostent#hostent.h_addr_list,
            Addr;
        _ ->
            {192, 88, 99, 0}
    end.


%% Regex borrowed from:
%% http://stackoverflow.com/questions/12794358/how-to-strip-all-blank-characters-in-a-string-in-erlang
-spec(dcos_ip() -> false | inet:ip4_address()).
dcos_ip() ->
    String = os:cmd("/opt/mesosphere/bin/detect_ip"),
    String1 = re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]),
    case inet:parse_ipv4_address(String1) of
        {ok, IP} ->
            IP;
        {error, einval} ->
            false
    end.

