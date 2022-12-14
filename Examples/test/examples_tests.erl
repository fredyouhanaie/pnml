%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021-2022, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the pnml module.
%%%
%%% @end
%%% Created :  4 Feb 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(examples_tests).

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

null_1_test_() ->
    {"null",
     setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml_null:start(?Model_nofile))},
      {"tiny file",
       ?_assertEqual({ok, null}, pnml_null:start(?Model_tiny))},
      {"small file",
       ?_assertEqual({ok, null}, pnml_null:start(?Model_small))}
     ]}.

%%--------------------------------------------------------------------

-define(Counts_tiny, #{arc => 1, initialMarking => 1, inscription => 1,
                       name => 1, net => 1, page => 1, place => 1,
                       pnml => 1, text => 3, transition => 1} ).

-define(Counts_small, #{arc => 440, graphics => 204, initialMarking => 12,
                        inscription => 80, name => 98, net => 1, offset => 108,
                        page => 1, place => 24, pnml => 1, position => 96,
                        text => 190, transition => 72 } ).

counter_1_test_() ->
    {"counter",
     setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml_counter:start(?Model_nofile))},
      {"tiny file",
       ?_assertEqual({ok, ?Counts_tiny}, pnml_counter:start(?Model_tiny))},
      {"small file",
       ?_assertEqual({ok, ?Counts_small}, pnml_counter:start(?Model_small))}
     ]}.

%%--------------------------------------------------------------------

logger_1_test_() ->
    {"logger",
     setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml_logger:start(?Model_nofile))},
      {"tiny file",
       ?_assertEqual({ok, #{log_level=>notice}}, pnml_logger:start(?Model_tiny))},
      {"small file",
       ?_assertEqual({ok, #{log_level=>notice}}, pnml_logger:start(?Model_small))}
     ]}.

%%--------------------------------------------------------------------

counter_cb_test_() ->
    {"counter via ets callback",
     setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, pnml_ets_counter:get_counts(?Model_nofile))},
      {"tiny file",
       ?_assertEqual([{arc,1},{net,1},{place,1},{transition,1}],
                     pnml_ets_counter:get_counts(?Model_tiny))},
      {"small file",
       ?_assertEqual([{arc,440},{net,1},{place,24},{transition,72}],
                     pnml_ets_counter:get_counts(?Model_small))}
     ]}.

%%--------------------------------------------------------------------
