%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the pnml_ets module.
%%%
%%% @end
%%% Created :  4 Feb 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_ets_tests).

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
    pnml_ets:cleanup(),
    ok.

%%--------------------------------------------------------------------

read_sample_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({{error, _Reason}, _Names_tid, _Net_tid}, pnml_ets:read_pt(?Model_nofile))},
      {"small file",
       ?_assertMatch({ok, _Names_tid, _Net_tid},
                     pnml_ets:read_pt(?Model_small)) }
     ]}.

%%--------------------------------------------------------------------

-define(ETS_names_tiny, [ {last_num, 4},
                          {<<"arc_1">>, 4},
                          {<<"net_1">>, 1},
                          {<<"place_1">>, 2},
                          {<<"transition_1">>, 3}
                        ]).

-define(ETS_table_tiny,
        [{{arc, 4},
          #{inscription := 2, net_num := 1, source := 2, target := 3}},
         {{net,1},
          #{type := <<"http://www.pnml.org/version-2009/grammar/ptnet">>}},
         {{place, 2},
          #{initial_marking := 2, net_num := 1}},
         {{transition, 3},
          #{net_num := 1}}
        ]).

read_ets_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"tiny file",
       ?_assertMatch({ok, _Names_tid, _Net_tabid},
                     pnml_ets:read_pt(?Model_tiny)) },

      {"tiny contents",
       ?_assertMatch({?ETS_names_tiny, ?ETS_table_tiny}, read_ets_tiny()) }

     ]}.

%%--------------------------------------------------------------------

% Process the tiny model and check the output, as well as the contents
% of the ETS table.
%
read_ets_tiny() ->
    {ok, Names_tid, Net_tid} = pnml_ets:read_pt(?Model_tiny),
    {lists:sort(ets:tab2list(Names_tid)), lists:sort(ets:tab2list(Net_tid))}.

%%--------------------------------------------------------------------

scan_elements_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"tiny file",
       ?_assertMatch({ok, _Names_tid, _Net_tabid},
                     pnml_ets:read_pt(?Model_tiny)) },

      {"tiny file counts",
       ?_assertEqual(4, scan_ets_count(?Model_tiny)) },

      {"small file counts",
       ?_assertEqual(537, scan_ets_count(?Model_small)) },

      {"tiny file contents",
       ?_assertMatch(?ETS_table_tiny, scan_ets_entries(?Model_tiny)) }

     ]}.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its elements via fold
% function.
%
scan_ets_count(File) ->
    {ok, _Names_tid, Net_tid} = pnml_ets:read_pt(File),
    Fun = fun (A, _E) -> A+1 end,
    Counts = pnml_ets:scan_elements(Fun, 0, Net_tid),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its elements via fold
% function.
%
scan_ets_entries(File) ->
    {ok, _Names_tid, Net_tid} = pnml_ets:read_pt(File),
    Fun = fun (A, [E_type, E_num, E_par]) -> 
                  [{{E_type, E_num}, E_par}|A]
          end,
    Entries = pnml_ets:scan_elements(Fun, [], Net_tid),
    pnml_ets:cleanup(),
    lists:sort(Entries).

%%--------------------------------------------------------------------
