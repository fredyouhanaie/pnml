%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the pnml module.
%%%
%%% @end
%%% Created :  4 Feb 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Model_nofile, "test/no_file.pnml").  %% this file should never exist!
-define(Model_tiny,   "test/ptnet_1.pnml").
-define(Model_small,  "test/distributeur-01-unfolded-02.pnml").

%% Change `Log_level' if investigating failed tests
-define(Log_level, critical).

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

setup() ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, ?Log_level).

cleanup(_) ->
    ok.

%%--------------------------------------------------------------------

read_sample_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml:read(?Model_nofile, pnml_null, null))},
      {"tiny file",
       ?_assertMatch({ok, null}, pnml:read(?Model_tiny, pnml_null, null))},
      {"small file",
       ?_assertMatch({ok, null}, pnml:read(?Model_small, pnml_null, null))}
     ]}.

%%--------------------------------------------------------------------

-define(Attr_1_list, [{[], [], "id", "place_1"}]).
-define(Attr_1_map, #{id => "place_1"}).

-define(Attr_2_list, [{[],[],"type","http://www.pnml.org/version-2009/grammar/ptnet"},
                      {[],[],"id","net_1"}]).
-define(Attr_2_map, #{type => "http://www.pnml.org/version-2009/grammar/ptnet",
                      id => "net_1"}).

-define(Attr_3_list, [{[],[],"id","arc_1"},
                      {[],[],"source","place_1"},
                      {[],[],"target","transition_1"}]).
-define(Attr_3_map, #{id => "arc_1",
                      source => "place_1",
                      target => "transition_1"}).

map_attr_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"empty list",
       ?_assertEqual(#{}, pnml:attr_map([]))},
      {"list with 1 attribute",
       ?_assertEqual(?Attr_1_map, pnml:attr_map(?Attr_1_list))},
      {"list with 2 attributes",
       ?_assertEqual(?Attr_2_map, pnml:attr_map(?Attr_2_list))},
      {"list with 3 attributes",
       ?_assertEqual(?Attr_3_map, pnml:attr_map(?Attr_3_list))}
     ]}.

%%--------------------------------------------------------------------
