%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2016 12:12 AM
%%%-------------------------------------------------------------------
-module(mesos_state_overlay_SUITE).
-author("sdhillon").


-include("mesos_state_overlay_pb.hrl").

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, parse_overlay/1]).



all() ->
    [parse_overlay].


parse_overlay(Config) ->
    DataDir = ?config(data_dir, Config),
    OverlayJson = filename:join(DataDir, "overlay.json"),
    {ok, OverlayJsonData} = file:read_file(OverlayJson),
    DecodedOverlayJsonData = jsx:decode(OverlayJsonData, [{labels, atom}]),
    ct:pal("DecodedOverlayJsonData: ~p", [DecodedOverlayJsonData]),
    OD = mesos_state_agentinfo:from_json(DecodedOverlayJsonData),
    ct:pal("OverlayData: ~p", [OD]).