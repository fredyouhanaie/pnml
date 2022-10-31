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

tables_test_() ->
    {setup,
     fun() -> pnml_ets:create_table("xxx", []) end,
     fun(_) -> ok end,
     fun check_tables/1
    }.

check_tables(Tid) ->
    [{"create table", ?_assertEqual(pnml_ets_xxx, ets:info(Tid, name))},
     {"delete table", ?_assertEqual(ok, pnml_ets:delete_table(Tid))}
    ].

%%-------------------------------------------------------------------

insert_test_() ->
    {setup, local, %% local needed for insert access rights
     fun() -> pnml_ets:create_table("net_tid", []) end,
     fun(_Tid) -> pnml_ets:cleanup() end,
     fun check_insert/1
    }.

check_insert(Tid) ->
    Element_key = {abc, 123},
    Element_map = #{one => 1, two => 2},
    Element = {Element_key, Element_map},
    [{"insert", ?_assertEqual(ok, pnml_ets:insert_element(Element))},
     {"lookup", ?_assertEqual([Element], ets:lookup(Tid, Element_key))}
    ].

%%-------------------------------------------------------------------

process_test_() ->
    {setup, local, %% local needed for insert access rights
     fun() ->
             {pnml_ets:create_table("net_tid", []),
              pnml_ets:create_table("names_tid", [])}
     end,
     fun(_Tid) -> pnml_ets:cleanup() end,
     fun local_test_process/1
    }.

local_test_process({Net_tid, _Names_tid}) ->
    [{"add_net", ?_assertEqual({net, 1}, add_net())},
     {"chk_net", ?_assertEqual([{{net, 1}, #{type => <<"http://www.pnml.org/version-2009/grammar/ptnet">>}}],
                               ets:lookup(Net_tid, {net, 1}))},

     {"add_place", ?_assertEqual({place, 2}, pnml_ets:process_place(#{id=>"place_1"}, 1))},
     {"chk_place", ?_assertEqual([{{place,2}, #{initial_marking => 0, net_num => 1}}],
                                 ets:lookup(Net_tid, {place, 2}))},

     {"add_trans", ?_assertEqual({transition, 3}, pnml_ets:process_transition(#{id=>"trans_1"}, 1))},
     {"chk_trans", ?_assertEqual([{{transition, 3}, #{net_num => 1}}],
                                 ets:lookup(Net_tid, {transition, 3}))},

     {"add_arc", ?_assertEqual({arc, 4},
                               pnml_ets:process_arc(#{id=>"arc_1", source=>"2", target=>"3"}, 1))},
     {"chk_arc", ?_assertEqual([{{arc, 4}, #{inscription => 1,net_num => 1,source => 5,target => 6}}],
                               ets:lookup(Net_tid, {arc, 4}))}
    ].

add_net() ->
    Attr_map = #{id => "net_1", type => "http://www.pnml.org/version-2009/grammar/ptnet"},
    Id_num = pnml_ets:get_id_num(maps:get(id, Attr_map)),
    pnml_ets:process_net(Id_num, Attr_map).

%%-------------------------------------------------------------------
