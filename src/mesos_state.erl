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
-include("mesos_state.hrl").


-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-type label_state() :: start | middle | terminate.
-spec(label(LabelState :: label_state(), RemainingChar :: string(), Acc :: string()) -> string()).
-define(ALLOWED_CHAR_GUARD(Char), (Char >= $a andalso Char =< $z) orelse (Char >= $0 andalso Char =< $9)).

%% API
-export([ip/0, domain_frag/1, label/1]).

-export_type([task_state/0, framework_id/0, framework_name/0, task_id/0, executor_id/0, task_name/0, labels/0,
    resource/0, resource_name/0, role/0, slave_id/0, hostname/0, protocol/0, mesos_port/0, ip_address/0,
    network_infos/0, net_cls/0, container_status/0, task_status/0, docker/0, container/0, libprocess_pid/0,
    slave/0, task/0
]).

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

-spec(domain_frag([binary()]) -> binary()).

%% DomainFrag mangles the given name in order to produce a valid domain fragment.
%% A valid domain fragment will consist of one or more host name labels
domain_frag(BinaryString) when is_binary(BinaryString) ->
    Fragments = binary:split(BinaryString, <<".">>, [global, trim_all]),
    domain_frag(Fragments);
domain_frag(Fragments0) ->
    Fragments1 =
        lists:filter(
            fun
                (<<>>) -> false;
                (_) -> true
            end,
            Fragments0
        ),
    StringFragments0 = lists:map(fun label/1, Fragments1),
    list_to_binary(string:join(StringFragments0, ".")).


label(Fragment) when is_binary(Fragment) ->
    label(binary_to_list(Fragment));
label(FragmentStr) when is_list(FragmentStr) ->
    lists:reverse(label(start, FragmentStr, [])).


%% When stripping from the accumulator left and right are reversed because it's backwards
label(_, [], Acc) ->
    string:strip(Acc, left, $-);
label(State, [Char0 | RestFragmentStr], Acc) when (Char0 >= $A andalso Char0 =< $Z) ->
    Char1 = Char0 - ($A - $a),
    label(State, [Char1 | RestFragmentStr], Acc);
label(start, FragmentStr = [Char | _RestFragmentStr], Acc) when ?ALLOWED_CHAR_GUARD(Char) ->
    label(middle, FragmentStr, Acc);
label(middle, FragmentStr, Acc0) when length(Acc0) > 62 ->
    Acc1 = string:strip(Acc0, left, $-),
    label(terminate, FragmentStr, Acc1);
label(middle, [Char | RestFragmentStr], Acc) when Char == $- orelse Char == $_ orelse Char == $. ->
    label(middle, RestFragmentStr, [$- | Acc]);
label(terminate, _Str, Acc) when length(Acc) == 63 ->
    label(terminate, [], Acc);
label(State, [Char | RestFragmentStr], Acc) when ?ALLOWED_CHAR_GUARD(Char) ->
    label(State, RestFragmentStr, [Char | Acc]);
label(State, [_Char | RestFragmentStr], Acc) ->
    label(State, RestFragmentStr, Acc).


-ifdef(TEST).
remap_test() ->
    ?assertEqual("fdgsf---gs7-fgs--d7fddg-123", label("fd%gsf---gs7-f$gs--d7fddg-123")),
    ?assertEqual("4abc123", label("4abc123")),
    ?assertEqual("89fdgsf---gs7-fgs--d7fddg-123", label("89fdgsf---gs7-fgs--d7fddg-123")),
    ?assertEqual("fdgsf---gs7-fgs--d7fddg123456789012345678901234567890123456789", label("##fdgsf---gs7-fgs--d7fddg123456789012345678901234567890123456789-")),
    ?assertEqual("fdgsf---gs7-fgs--d7fddg1234567890123456789012345678901234567891", label("fd%gsf---gs7-f$gs--d7fddg123456789012345678901234567890123456789-123")),
    ?assertEqual("89fdgsf---gs7-fgs--d7fddg---123", label("89fdgsf---gs7-fgs--d7fddg---123")),
    ?assertEqual("fdgsf---gs7-fgs--d7fddg1234567890123456789012345678901234567891", label("%%fdgsf---gs7-fgs--d7fddg123456789012345678901234567890123456789---123")),
    ?assertEqual("4abc123", label("-4abc123")),
    ?assertEqual("fdgsf---gs7-fgs--d7fddg1234567890123456789012345678901234567891", label("$$fdgsf---gs7-fgs--d7fddg123456789012345678901234567890123456789-123")),
    ?assertEqual("89fdgsf---gs7-fgs--d7fddg", label("89fdgsf---gs7-fgs--d7fddg-")),
    ?assertEqual("0-0", label("0-0")),
    ?assertEqual("a-----------------------------------------------b", "a-----------------------------------------------b").

all_prop_test_() ->
    {
        timeout,
        120,
        [fun() -> [] = proper:module(?MODULE, [{to_file, user}, {numtests, 100000}]) end]
    }.

any(Str) ->
    lists:all(fun
                  (Char) when ?ALLOWED_CHAR_GUARD(Char) orelse Char == $- ->
                      true;
                  (_) ->
                      false
              end,
    Str).

prop_allow_any_ascii() ->
    ?FORALL(Str, string(), any(label(Str))).

-endif.





