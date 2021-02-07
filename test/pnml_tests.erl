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

%%--------------------------------------------------------------------

-define(Counter, {fun pnml:h_count/2, #{}}).

-define(Counts_tiny, #{arc => 1, initialMarking => 1, inscription => 1,
                       name => 1, net => 1, page => 1, place => 1,
                       pnml => 1, text => 3, transition => 1} ).

-define(Counts_small, #{arc => 440, graphics => 204, initialMarking => 12,
                        inscription => 80, name => 98, net => 1, offset => 108,
                        page => 1, place => 24, pnml => 1, position => 96,
                        text => 190, transition => 72 } ).

read_counter_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml:read(?Model_nofile, ?Counter))},
      {"tiny file",
       ?_assertEqual({ok, ?Counts_tiny}, pnml:read(?Model_tiny, ?Counter))},
      {"small file",
       ?_assertEqual({ok, ?Counts_small}, pnml:read(?Model_small, ?Counter))}
     ]}.

%%--------------------------------------------------------------------

-define(Logger, {fun pnml:h_log/2, #{}}).

read_logger_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml:read(?Model_nofile, ?Logger))},
      {"tiny file",
       ?_assertEqual({ok, #{log_level=>notice}}, pnml:read(?Model_tiny, ?Logger))},
      {"small file",
       ?_assertEqual({ok, #{log_level=>notice}}, pnml:read(?Model_small, ?Logger))}
     ]}.
