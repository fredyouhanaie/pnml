%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2021-2022, Fred Youhanaie
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
-define(Model_empty,  "test/ptnet_0.pnml").
-define(Model_tiny,   "test/ptnet_1.pnml").
-define(Model_small,  "test/distributeur-01-unfolded-02.pnml").

-define(Init_mark_tiny, #{2 => 2}).
-define(Init_mark_small, #{2 => 1, 3 => 1,  4 => 1,  5 => 1,  6 => 1,  7 => 1,
                           8 => 1, 9 => 1, 10 => 1, 11 => 1, 12 => 1, 13 => 1}).

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
    {"file tests",
     setup, fun setup/0, fun cleanup/1,
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
    {"read tests",
     setup, fun setup/0, fun cleanup/1,
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
    {"scan tests",
     setup, fun setup/0, fun cleanup/1,

     [{"empty pnml counts",
       ?_assertEqual(0, scan_ets_count(?Model_empty)) },

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
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Fun = fun (_E, A) -> A+1 end,
    Counts = pnml_ets:scan_elements(Fun, 0),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its elements via fold
% function.
%
scan_ets_entries(File) ->
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Fun = fun (E, A) -> [E|A] end,
    Entries = pnml_ets:scan_elements(Fun, []),
    pnml_ets:cleanup(),
    lists:sort(Entries).

%%--------------------------------------------------------------------

tables_test_() ->
    {"ETS table tests",
     setup,
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
    {"insert tests",
     setup, local, %% local needed for insert access rights
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
    {"process_* tests",
     setup, local, %% local needed for insert access rights
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

init_mark_test_() ->
    {"Initial marking",
     [{ "tiny all",    ?_assertEqual(#{1 => ?Init_mark_tiny},  get_init_mark(?Model_tiny))},
      { "small all",   ?_assertEqual(#{1 => ?Init_mark_small}, get_init_mark(?Model_small))}
     ]}.

get_init_mark(File) ->
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Marking = pnml_ets:init_marking(),
    pnml_ets:cleanup(),
    Marking.

%%-------------------------------------------------------------------

scan_places_test_() ->
    {"scan places",
     setup, fun setup/0, fun cleanup/1,
     [{"empty pnml counts",
       ?_assertEqual(0, scan_places_count(?Model_empty)) },

      {"tiny file counts",
       ?_assertEqual(1, scan_places_count(?Model_tiny)) },

      {"small file counts",
       ?_assertEqual(24, scan_places_count(?Model_small)) }

     ]}.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its places.
%
scan_places_count(File) ->
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Counts = length(pnml_ets:scan_places()),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------

scan_transitions_test_() ->
    {"scan transitions",
     setup, fun setup/0, fun cleanup/1,
     [{"empty pnml counts",
       ?_assertEqual(0, scan_transitions_count(?Model_empty)) },

      {"tiny file counts",
       ?_assertEqual(1, scan_transitions_count(?Model_tiny)) },

      {"small file counts",
       ?_assertEqual(72, scan_transitions_count(?Model_small)) }

     ]}.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its transitions.
%
scan_transitions_count(File) ->
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Counts = length(pnml_ets:scan_transitions()),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------

scan_arcs_test_() ->
    {"scan arcs",
     setup, fun setup/0, fun cleanup/1,
     [{"empty pnml counts",
       ?_assertEqual(0, scan_arcs_count(?Model_empty)) },

      {"tiny file counts",
       ?_assertEqual(1, scan_arcs_count(?Model_tiny)) },

      {"small file counts",
       ?_assertEqual(440, scan_arcs_count(?Model_small)) }

     ]}.

%%--------------------------------------------------------------------

% Read a model into ETS and return a count of its arcs.
%
scan_arcs_count(File) ->
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File),
    Counts = length(pnml_ets:scan_arcs()),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------

-define(CB_table, pnml_ets_cb).
-define(CB_func, fun (Tag, Id_num) ->
                         ets:update_counter(?CB_table, Tag, 1, {Tag,0})
                 end).

read_cb_test_() ->
    {"read_pt with callback",
     setup, fun setup/0, fun cleanup/1,
     [{"empty pnml counts",
       ?_assertEqual([], counts_via_read_cb(?Model_empty)) },
      {"tiny pnml counts",
       ?_assertEqual([{arc,1},{net,1},{place,1},{transition,1}],
                     counts_via_read_cb(?Model_tiny)) },
      {"small pnml counts",
       ?_assertEqual([{arc,440},{net,1},{place,24},{transition,72}],
                     counts_via_read_cb(?Model_small)) }
     ]}.

counts_via_read_cb(File) ->
    ?CB_table = ets:new(?CB_table, [named_table]),
    {ok, _Names_tid, _Net_tid} = pnml_ets:read_pt(File, ?CB_func),
    Counts = lists:sort(ets:tab2list(?CB_table)),
    ets:delete(?CB_table),
    pnml_ets:cleanup(),
    Counts.

%%--------------------------------------------------------------------
