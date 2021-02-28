%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% A handler that saves the net elements in an ETS table.
%%%
%%% Two ETS tables will be created, one for the Petri Net elements,
%%% and the second for the net element names.
%%%
%%% The table for the net elements will contain one record for each of
%%% PNML elements in the document, i.e. `net', `place', `transition'
%%% and `arc'.
%%%
%%% The records stored in the ETS table are of one of the following forms:
%%%
%%% <ul>
%%%
%%% <li>`{ {net, Num_id}, #{type => Net_type} }'</li>
%%%
%%% <li>`{ {place, Num_id}, #{net_num => Net_num, initial_marking => Initial_Marking} }'</li>
%%%
%%% <li>`{ {transition, Num_id}, #{net_num => Net_num, } }'</li>
%%%
%%% <li>`{ {arc, Num_id}, #{net_num => Net_num, source => Source, target => Target, inscription => Inscription} }'</li>
%%%
%%% </ul>
%%%
%%% Where:
%%%
%%% <ul>
%%%
%%% <li>`Num_id' is the unique numeric id assigned to each
%%% element.</li>
%%%
%%% <li>`InitMarking' and `Inscription' are integers that are either
%%% taken from the PNML document, or if missing are assigned default
%%% values of `0' and `1' respectively.</li>
%%%
%%% <li>`Source' and `Target' are the unique numbers assigned to their
%%% corresponding `place' or `transition' string identifiers.</li>
%%%
%%% </ul>
%%%
%%% Each of the Net elements has a unique string identifier, however,
%%% in the interest of efficiency, we maintain a symbol table,
%%% `Names', that maps the symbolic names (as binary strings) to
%%% unique numeric ids. The `Names' table will contain one tuple for
%%% each net element name/number pair, `{Id::binary(),
%%% Id_num::integer()}'.
%%%
%%% The two tables are created by the `read_pt/1' function, and the
%%% table ids returned as part of the result, if the function is
%%% successful.
%%%
%%% @end
%%% Created :  7 Feb 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_ets).

-export([read_pt/1, cleanup/0]).
-export([get_id_num/1, add_id_ref/2]).

-include_lib("kernel/include/logger.hrl").

%%-------------------------------------------------------------------

%% We are only intersted in the following element tags:
%%
-define(PT_elements, [pnml, net, place, arc, transition, text,
                      initialMarking, inscription, referencePlace,
                      referenceTransition]).

%%-------------------------------------------------------------------

-type h_ets_state() :: {Parents::list(), Net_num::integer(), Place_num::integer(), Arc_num::integer()}.

-type ref_type() :: referencePlace | referenceTransition.

%%--------------------------------------------------------------------
%% @doc Read a PT net and store the details in ETS tables.
%%
%% We create two tables one for the net elements, and the other for
%% the element names and their corresponding unique numbers.
%%
%% Whether succussful, otherwise, the ETS table identifiers are
%% returned to the caller.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_pt(string()) -> {ok, ets:tid(), ets:tid()} | {{error, term()}, ets:tid(), ets:tid()}.
read_pt(File) ->
    Base_name = atom_to_list(?MODULE),

    Net_table = list_to_atom(Base_name ++ "_net"),
    Net_tabid = ets:new(Net_table, []),
    persistent_term:put({?MODULE, net_tid}, Net_tabid),

    Names_table = list_to_atom(Base_name ++ "_names"),
    Names_tabid = ets:new(Names_table, []),
    persistent_term:put({?MODULE, names_tid}, Names_tabid),

    State0 = {[], 0, 0, 0},
    case pnml:read(File, {fun h_ets/2, State0}) of
        {ok, State0} ->
            {ok, Names_tabid, Net_tabid};
        Other ->
            {Other, Names_tabid, Net_tabid}
    end.


%%--------------------------------------------------------------------
%% @doc The main handler function.
%%
%% The handler state is a tuple `{Parents, Net_num, Place_num, Arc_num}',
%% where, `Parents' is a list, initially empty, that keeps
%% track of the nested elements.
%%
%% `Net_num' is the integer corresponding to the current net. This
%% number will be recorded with each of the other elements, since a
%% PNML document may contain more than one net.
%%
%% `Place_num' and `Arc_num' will either be zero, or they will hold
%% the current place/arc number. This is used for any nested elements
%% with paths `place'/`initialMarking'/`text' or
%% `arc'/`inscription'/`text' that may be encountered. They are used
%% by the `process_initialMarking/2' and `process_inscription/2'
%% functions to update the corresponding fields in the `place' or
%% `arc' records.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets(pnml:handler_arg(), h_ets_state()) -> h_ets_state().
h_ets({el_begin, Tag, Attrs}, State) ->
    ?LOG_DEBUG("h_ets: el_begin Tag=~p, Attrs=~p, State=~p.",
               [Tag, Attrs, State]),
    case lists:member(Tag, ?PT_elements) of
        true ->
            Attr_map = pnml:attr_map(Attrs),
            h_ets_begin(Tag, Attr_map, State);
        false ->
            State
    end;

h_ets({el_end, Tag}, State) ->
    ?LOG_DEBUG("h_ets: el_end Tag=~p, State=~p.",
               [Tag, State]),
    case lists:member(Tag, ?PT_elements) of
        true ->
            h_ets_end(Tag, State);
        false ->
            State
    end;

h_ets({el_text, Text}, State) ->
    ?LOG_DEBUG("h_ets: el_text Text=~p, State=~p.",
               [Text, State]),
    h_ets_text(Text, State).


%%--------------------------------------------------------------------
%% @doc Handle an `el_begin' request.
%%
%% The first parameter is the PNML element tag. Each tag is inserted
%% at the head of the `Parents' list. Which will later be removed in
%% the corresponding `h_ets_end' function.
%%
%% For the main elements we create an entry in the ETS net table.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_begin(atom(), map(), h_ets_state()) -> h_ets_state().
h_ets_begin(pnml, _Attr_map, {[], 0, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: pnml, Parents=[]."),
    {[pnml], 0, 0, 0};

h_ets_begin(net, Attr_map, {[pnml], 0, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: net, Parents=[pnml]."),
    Id_num = get_id_num(maps:get(id, Attr_map)),
    process_net(Id_num, Attr_map),
    {[net, pnml], Id_num, 0, 0};

h_ets_begin(place, Attr_map, {Parents=[net, pnml], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: place, Parents=~p].", [Parents]),
    {place, Place_num} = process_place(Attr_map, Net_num),
    {[place|Parents], Net_num, Place_num, 0};

h_ets_begin(transition, Attr_map, {Parents=[net, pnml], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: transition, Parents=~p].", [Parents]),
    {transition, _Id_num} = process_transition(Attr_map, Net_num),
    {[transition|Parents], Net_num, 0, 0};

h_ets_begin(arc, Attr_map, State={Parents=[net, pnml], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: arc, State=~p].", [State]),
    {arc, Arc_num} = process_arc(Attr_map, Net_num),
    {[arc|Parents], Net_num, 0, Arc_num};

h_ets_begin(initialMarking, _Attr_map, State={Parents=[place|_], Net_num, Place_num, 0}) ->
    ?LOG_DEBUG("h_ets_begin: initialMarking, State=~p].", [State]),
    {[initialMarking|Parents], Net_num, Place_num, 0};

h_ets_begin(inscription, _Attr_map, State={Parents=[arc|_], Net_num, 0, Arc_num}) ->
    ?LOG_DEBUG("h_ets_begin: inscription, State=~p].", [State]),
    {[inscription|Parents], Net_num, 0, Arc_num};

h_ets_begin(referencePlace, Attr_map, State={[net, pnml], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: referencePlace, State=~p].", [State]),
    referencePlace = process_reference(referencePlace, Attr_map),
    {[referencePlace, net, pnml], Net_num, 0, 0};

h_ets_begin(referenceTransition, Attr_map, State={[net, pnml], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_begin: referenceTransition, State=~p].", [State]),
    referenceTransition = process_reference(referenceTransition, Attr_map),
    {[referenceTransition, net, pnml], Net_num, 0, 0};

h_ets_begin(text, _Attr_map, State={Parents=[initialMarking|_], Net_num, Place_num, 0}) ->
    ?LOG_DEBUG("h_ets_begin: initialMarking, State=~p].", [State]),
    {[text|Parents], Net_num, Place_num, 0};

h_ets_begin(text, _Attr_map, State={Parents=[inscription|_], Net_num, 0, Arc_num}) ->
    ?LOG_DEBUG("h_ets_begin: inscription, State=~p].", [State]),
    {[text|Parents], Net_num, 0, Arc_num};

h_ets_begin(Tag, _Attr_map, State) ->
    ?LOG_DEBUG("h_ets_begin: tag ignored Tag=~p, State=~p.", [Tag, State]),
    State.


%%--------------------------------------------------------------------
%% @doc Handle an `el_end' request.
%%
%% We expect `Tag' to match the first tag in the `Parents' list. If
%% there is a match, then the head of the parents list is
%% removed. Otherwise, an error is logged.
%%
%% The `Parents' list may contain two types of elements, the tuple
%% `{net, Net_num}', or an `atom', such as `place', `initialMarking',
%% etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_end(atom(), h_ets_state()) -> h_ets_state().
h_ets_end(pnml, State={[pnml], 0, 0, 0}) ->
    ?LOG_DEBUG("h_ets_end: pnml, State=~p.", [State]),
    {[], 0, 0, 0};

h_ets_end(net, State={[net|Rest], _Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_end: net, State=~p.", [State]),
    {Rest, 0, 0, 0};

h_ets_end(place, State={[place|Rest], Net_num, _Place_num, 0}) ->
    ?LOG_DEBUG("h_ets_end: place, State=~p.", [State]),
    {Rest, Net_num, 0, 0};

h_ets_end(transition, State={[transition|Rest], Net_num, 0, 0}) ->
    ?LOG_DEBUG("h_ets_end: transition, State=~p.", [State]),
    {Rest, Net_num, 0, 0};

h_ets_end(arc, State={[arc|Rest], Net_num, 0, _Arc_num}) ->
    ?LOG_DEBUG("h_ets_end: arc, State=~p.", [State]),
    {Rest, Net_num, 0, 0};

h_ets_end(initialMarking, State={[initialMarking|Rest], Net_num, Place_num, 0}) ->
    ?LOG_DEBUG("h_ets_end: initialMarking, State=~p.", [State]),
    {Rest, Net_num, Place_num, 0};

h_ets_end(inscription, State={[inscription|Rest], Net_num, 0, Arc_num}) ->
    ?LOG_DEBUG("h_ets_end: inscription, State=~p.", [State]),
    {Rest, Net_num, 0, Arc_num};

h_ets_end(text, State={[text,initialMarking|Rest], Net_num, Place_num, 0}) ->
    ?LOG_DEBUG("h_ets_end: text, State=~p.", [State]),
    {[initialMarking|Rest], Net_num, Place_num, 0};

h_ets_end(text, State={[text,inscription|Rest], Net_num, 0, Arc_num}) ->
    ?LOG_DEBUG("h_ets_end: text, State=~p.", [State]),
    {[inscription|Rest], Net_num, 0, Arc_num};

h_ets_end(text, State) ->
    ?LOG_DEBUG("h_ets_end: text ignored, State=~p.", [State]),
    State;

h_ets_end(Tag, State) ->
    ?LOG_WARNING("h_ets_end: unexpected tag - ignored, Tag=~p, State=~p.",
               [Tag, State]),
    State.


%%--------------------------------------------------------------------
%% @doc Handle an `el_text' request.
%%
%% We are only interested in the text within the two paths
%% `pnml/net/place/initialMarking/text' and
%% `pnml/net/arc/inscription/text'. All other text content is ignored.
%%
%% The elements of the `Parents' list are matched against the reverse
%% of the above paths.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_text(string(), h_ets_state()) -> h_ets_state().
h_ets_text(Text, State={[text, initialMarking | _ ], _Net_num, Place_num, 0}) ->
    ok = process_initialMarking(Text, Place_num),
    State;

h_ets_text(Text, State={[text, inscription | _ ], _Net_num, 0, Arc_num}) ->
    ok = process_inscription(Text, Arc_num),
    State;

h_ets_text(Text, State) ->
    ?LOG_DEBUG("h_ets_text: text ignored Text=~p, State=~p.", [Text, State]),
    State.


%%--------------------------------------------------------------------
%% @doc Process a `net' element.
%%
%% A `net' record will be added to the ETS `net' table.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_net(integer(), map()) -> {net, integer()}.
process_net(Id_num, Attr_map) ->
    Type = list_to_binary(maps:get(type, Attr_map)),
    insert_element({ {net, Id_num}, #{type => Type} }),
    {net, Id_num}.


%%--------------------------------------------------------------------
%% @doc Process `place' element.
%%
%% A `place' record will be added to the ETS `net' table with default
%% `initialMarking' of `0'.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_place(map(), integer()) -> {place, integer()}.
process_place(Attr_map, Net_num) ->
    Id_num = get_id_num(maps:get(id, Attr_map)),
    insert_element({ {place, Id_num},
                     #{net_num => Net_num,
                       initial_marking => 0}
                   }),
    {place, Id_num}.


%%--------------------------------------------------------------------
%% @doc process a `transition' element.
%%
%% A `transition' record will be added to the ETS `net' table.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_transition(map(), integer()) -> {transition, integer()}.
process_transition(Attr_map, Net_num) ->
    Id_num = get_id_num(maps:get(id, Attr_map)),
    insert_element({ {transition, Id_num},
                     #{net_num=>Net_num}
                   }),
    {transition, Id_num}.


%%--------------------------------------------------------------------
%% @doc Process an `arc' element.
%%
%% An `arc' record will be added to the ETS `net' table, along with
%% the id numbers for the `source' and `target' elements, and default
%% `inscription' of `1'.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_arc(map(), integer()) -> {arc, integer()}.
process_arc(Attr_map, Net_num) ->
    Id_num = get_id_num(maps:get(id, Attr_map)),
    Source_num = get_id_num(maps:get(source, Attr_map)),
    Target_num = get_id_num(maps:get(target, Attr_map)),
    insert_element({ {arc, Id_num},
                     #{net_num => Net_num,
                       source => Source_num,
                       target => Target_num,
                       inscription => 1}
                   }),
    {arc, Id_num}.


%%--------------------------------------------------------------------
%% @doc Process a `referencePlace' or `referenceTransition' element.
%%
%% A reference item will be added to the names table.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_reference(ref_type(), map()) -> ref_type().
process_reference(Ref_type, Attr_map) ->
    ok = add_id_ref(maps:get(id, Attr_map), maps:get(ref, Attr_map)),
    Ref_type.


%%--------------------------------------------------------------------
%% @doc Process an `initialMarking' text field.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_initialMarking(string(), integer()) -> ok.
process_initialMarking(Text, Place_num) ->
    Initial_marking = list_to_integer(Text),
    Tab_id = get_net_tid(),
    [{Place, Place_map}] = ets:lookup(Tab_id, {place, Place_num}),
    Place_map2 = maps:update(initial_marking, Initial_marking, Place_map),
    true = ets:update_element(Tab_id, Place, {2, Place_map2}),
    ok.


%%--------------------------------------------------------------------
%% @doc Process an `inscription' text body.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_inscription(string(), integer()) -> ok.
process_inscription(Text, Arc_num) ->
    Inscription = list_to_integer(Text),
    Tab_id = get_net_tid(),
    [{Arc, Arc_map}] = ets:lookup(Tab_id, {arc, Arc_num}),
    Arc_map2 = maps:update(inscription, Inscription, Arc_map),
    true = ets:update_element(Tab_id, Arc, {2,Arc_map2}),
    ok.


%%--------------------------------------------------------------------
%% @doc Add a new reference name to the Names table for `referencePlace'
%% and `referenceTransition' PNML elements.
%%
%% `Id' and `Ref' are respectively the `id' and `ref' attributes of
%% the reference element. Both are of `string()' type as provided by
%% `xmerl', however, they are converted to `binary' strings when saved
%% in the table.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_id_ref(string(), string()) -> ok.
add_id_ref(Id, Ref) ->
    Id_bin  = list_to_binary(Id),
    Ref_bin = list_to_binary(Ref),
    Tab_id  = get_names_tid(),
    true = ets:insert(Tab_id, {Id_bin, Ref_bin}),
    ok.


%%--------------------------------------------------------------------
%% @doc Return the id number for a given PNML element id name.
%%
%% If the name does not exist, a new entry with a new unique number is
%% created, and inserted in the ETS table for the names.
%%
%% If the `Id' exists it may refer to a number or another name, for
%% `referencePlace' and `referenceTransition'. In the latter case the
%% lookup is repeated once more.
%%
%% The unique integer corresponding to the id name is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_id_num(string()) -> integer().
get_id_num(Id) ->
    Names_tid = get_names_tid(),
    Id_bin = list_to_binary(Id),
    case ets:lookup(Names_tid, Id_bin) of
        [] -> %% entry not found
            %% create a num entry for the Id
            Id_num = next_num(Names_tid),
            ets:insert(Names_tid, {Id_bin, Id_num}),
            Id_num;

        [{Id_bin, Id_num}] when is_integer(Id_num) -> %% we have an integer entry
            %% We found a number entry
            Id_num;

        [{Id_bin, Ref}] -> %% we have a reference entry
            %% we found a ref entry
            %% look up the new ref
            case ets:lookup(Names_tid, Ref) of
                [] -> %% entry not found
                    %% create a num entry for the Id
                    Id_num = next_num(Names_tid),
                    ets:insert(Names_tid, {Ref, Id_num}),
                    Id_num;

                [{Id_bin, Id_num}] -> %% We found a number entry
                    Id_num
            end
    end.


%%--------------------------------------------------------------------
%% @doc Increment the name index tuple, create it if missing.
%%
%% We maintain a `{last_num, integer()}' tuple in the ETS table for
%% names/numbers. `last_num' is set to `1' during the first call, and
%% is incremented during each subsequent call.
%%
%% @end
%%--------------------------------------------------------------------
-spec next_num(ets:tid()) -> integer().
next_num(Tab_id) ->
    ets:update_counter(Tab_id, last_num, 1, {last_num, 0}).


%%--------------------------------------------------------------------
%% @doc Return the ETS table id for the net records.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_net_tid() -> ets:tid()|none.
get_net_tid() ->
    persistent_term:get({?MODULE, net_tid}, none).


%%--------------------------------------------------------------------
%% @doc Return the ETS table id for the names table.
%%
%% @end
%%-------------------------------------------------------------------
-spec get_names_tid() -> ets:tid()|none.
get_names_tid() ->
    persistent_term:get({?MODULE, names_tid}, none).


%%--------------------------------------------------------------------
%% @doc Insert a net element in the net table.
%%
%% @end
%%--------------------------------------------------------------------
-spec insert_element(tuple()) -> ok.
insert_element(Element) ->
    Tab_id = get_net_tid(),
    true = ets:insert(Tab_id, Element),
    ok.


%%--------------------------------------------------------------------
%% @doc Clean up all data created during the parsing.
%%
%% We delete the two ETS tables pointed to by the persistent terms, as
%% well as the persistent terms.
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    delete_table(get_net_tid()),
    persistent_term:erase({?MODULE, net_tid}),

    delete_table(get_names_tid()),
    persistent_term:erase({?MODULE, names_tid}),
    ok.


%%--------------------------------------------------------------------
%% @doc Delete an ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_table(ets:tid()|none) -> ok.
delete_table(none) ->
    ok;
delete_table(Tab_id) ->
    try ets:delete(Tab_id) of
        true ->
            ok
    catch Type:Exception ->
            ?LOG_WARNING("delete_table: got exception ~p:~p, Tab_id=~p.", [Type, Exception, Tab_id]),
            ok
    end.

%%--------------------------------------------------------------------
