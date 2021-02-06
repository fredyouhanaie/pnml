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
       ?_assertMatch({error, _Reason}, pnml:read(?Model_nofile))},
      {"tiny file",
       ?_assertMatch({ok, null}, pnml:read(?Model_tiny))},
      {"small file",
       ?_assertMatch({ok, null}, pnml:read(?Model_small))}
     ]}.
