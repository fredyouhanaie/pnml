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

-define(ETS_table, ets:new(pnml_tab, [])).
-define(H_ets(Tab_id), {fun pnml_ets:h_ets/2, {[], {#{}, 0}, Tab_id}}).

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
       ?_assertMatch({error, _Reason}, pnml:read(?Model_nofile, ?H_ets(?ETS_table)))},
      {"small file",
       ?_assertMatch({ok, {[], {_, 537}, _Tab_id}},
                     pnml:read(?Model_small, ?H_ets(?ETS_table))) }
     ]}.

%%--------------------------------------------------------------------

-define(ETS_names_tiny, {#{<<"arc_1">> := 3, <<"net_1">> := 0, <<"place_1">> := 1,
                           <<"transition_1">> := 2}, 4}).

-define(ETS_table_tiny,
        [{{arc,3},
          #{inscription := 2, net_num := 0, source := 1, target := 2}},
         {{net,0},
          #{type := <<"http://www.pnml.org/version-2009/grammar/ptnet">>}},
         {{place, 1},
          #{initial_marking := 2, net_num := 0}},
         {{transition, 2},
          #{net_num := 0}}
        ]).

read_ets_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"tiny file",
       ?_assertMatch({ok, {[], ?ETS_names_tiny, _Tab_id} },
                     pnml:read(?Model_tiny, ?H_ets(?ETS_table))) },

      {"tiny contents",
       ?_assertMatch({?ETS_names_tiny, ?ETS_table_tiny}, read_ets_tiny()) }

     ]}.

%%--------------------------------------------------------------------

% Process the tiny model and check the output, as well as the contents
% of the ETS table.
%
read_ets_tiny() ->
    Tab_id = ?ETS_table,
    {ok, {[], {Names, Count}, Tab_id}} = pnml:read(?Model_tiny, ?H_ets(Tab_id)),
    {{Names, Count}, lists:sort(ets:tab2list(Tab_id))}.

%%--------------------------------------------------------------------
