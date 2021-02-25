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
%%% The terms stored in the ETS table are of the following types:
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

-export([read_pt/1, get_id_num/2, add_id_ref/3]).

-include_lib("kernel/include/logger.hrl").

%%-------------------------------------------------------------------

%% We are only intersted in the following element tags:
%%
-define(PT_elements, [pnml, net, place, arc, transition, text,
                      initialMarking, inscription, referencePlace,
                      referenceTransition]).

%%-------------------------------------------------------------------

-type h_ets_state() :: {Parents::list(), Names_tabid::ets:tid(), Net_tabid::ets:tid()}.

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

    Names_table = list_to_atom(Base_name ++ "_names"),
    Names_tabid = ets:new(Names_table, []),

    State0 = {fun h_ets/2, {[], Names_tabid, Net_tabid}},
    case pnml:read(File, State0) of
        {ok, {[], Names_tabid, Net_tabid}} ->
            {ok, Names_tabid, Net_tabid};
        Other ->
            {Other, Names_tabid, Net_tabid}
    end.


%%--------------------------------------------------------------------
%% @doc The main handler function.
%%
%% The handler state is a tuple `{Parents, Names, Tab_id}', where,
%% `Parents' is a list, initially empty, that keeps track of the
%% nested elements; `Names' is the ETS table id of the name/num pairs;
%% and `Tab_id' is the ETS table id of the net elements.
%%
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
%% @doc Handle an `el_begin' requests.
%%
%% The first parameter is the PNML element tag. Each tag is inserted
%% at the head of the `Parents' list. Which will later be removed in
%% the corresponding `h_ets_end' function.
%%
%% For the main elements we create an entry in the ETS table. You can
%% find further details in the docs for `h_ets/2'.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_begin(atom(), map(), h_ets_state()) -> h_ets_state().
h_ets_begin(pnml, _Attr_map, {[], Names, Tab_id}) ->
    {[pnml], Names, Tab_id};

h_ets_begin(net, Attr_map, {[pnml], Names, Tab_id}) ->
    Id_num = get_id_num(maps:get(id, Attr_map), Names),
    Type = list_to_binary(maps:get(type, Attr_map)),
    true = ets:insert(Tab_id, {{net, Id_num}, #{type => Type} }),
    {[{net, Id_num}, pnml], Names, Tab_id};

h_ets_begin(place, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    Id_num = get_id_num(maps:get(id, Attr_map), Names),
    true = ets:insert(Tab_id, {{place, Id_num},
                               #{net_num=>Net_num,
                                 initial_marking=>0}
                              }),
    {[{place, Id_num}|Parents], Names, Tab_id};

h_ets_begin(transition, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    Id_num = get_id_num(maps:get(id, Attr_map), Names),
    true = ets:insert(Tab_id, {{transition, Id_num},
                               #{net_num=>Net_num}
                              }),
    {[{transition, Id_num}|Parents], Names, Tab_id};

h_ets_begin(arc, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    Id_num     = get_id_num(maps:get(id, Attr_map), Names),
    Source_num = get_id_num(maps:get(source, Attr_map), Names),
    Target_num = get_id_num(maps:get(target, Attr_map), Names),

    true = ets:insert(Tab_id, {{arc, Id_num},
                               #{net_num => Net_num,
                                 source => Source_num,
                                 target => Target_num,
                                 inscription => 1}
                              }),
    {[{arc, Id_num}|Parents], Names, Tab_id};

h_ets_begin(referencePlace, Attr_map, {Parents=[{net, _Net_num}, pnml], Names, Tab_id}) ->
    add_id_ref(maps:get(id, Attr_map),
               maps:get(ref, Attr_map),
               Names),
    {[referencePlace|Parents], Names, Tab_id};

h_ets_begin(referenceTransition, Attr_map, {Parents=[{net, _Net_num}, pnml], Names, Tab_id}) ->
    add_id_ref(maps:get(id, Attr_map),
               maps:get(ref, Attr_map),
               Names),
    {[referenceTransition|Parents], Names, Tab_id};

h_ets_begin(Tag, _Attr_map, {Parents, Names, Tab_id}) ->
    ?LOG_DEBUG("h_ets_begin: tag ignored Tag=~p, Parents=~p.", [Tag, Parents]),
    {[Tag|Parents], Names, Tab_id}.


%%--------------------------------------------------------------------
%% @doc Handle an `el_end' element.
%%
%% We expect `Tag' to match the first tag in the Parents list. If
%% there is a match, then the head of the parents list is
%% removed. Otherwise, an error is logged.
%%
%% The `Parents' list may contain two types of elements, a tuple, such
%% as `{place, Place_num}', and an atom, such as `initialMarking'. We
%% cater for both patterns, and nothing else.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_end(atom(), h_ets_state()) -> h_ets_state().
h_ets_end(Tag, {[Tag|Parents], Names, Tab_id}) ->
    {Parents, Names, Tab_id};

h_ets_end(Tag, {Parents=[Tag2|_], Names, Tab_id}) when is_atom(Tag2) ->
    ?LOG_ERROR("h_ets_end: unexpected end tag, Tag=~p, Parents=~p.",
               [Tag, Parents]),
    {Parents, Names, Tab_id};

h_ets_end(Tag, {[{Tag, _Id_num}|Parents], Names, Tab_id}) ->
    {Parents, Names, Tab_id};

h_ets_end(Tag, {Parents=[{Tag2, _Id_num}|_], Names, Tab_id}) when is_atom(Tag2) ->
    ?LOG_ERROR("h_ets_end: unexpected end tag, Tag=~p, Parents=~p.",
               [Tag, Parents]),
    {Parents, Names, Tab_id}.


%%--------------------------------------------------------------------
%% @doc Handle an `el_text' element.
%%
%% We are only interested in the within two paths
%% `pnml/net/place/initialMarking/text' and
%% `pnml/net/arc/inscription/text'. All other text content is ignored.
%%
%% The elements of the `Parents' list are matched against the reverse
%% of the above paths.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_ets_text(string(), h_ets_state()) -> h_ets_state().
h_ets_text(Text, {Parents=[text, initialMarking, Place={place,_Place_Num}|_],
                  Names, Tab_id}) ->
    Initial_marking = list_to_integer(Text),
    [{Place, Place_map}] = ets:lookup(Tab_id, Place),
    Place_map2 = maps:update(initial_marking, Initial_marking, Place_map),
    true = ets:update_element(Tab_id, Place, {2, Place_map2}),
    {Parents, Names, Tab_id};

h_ets_text(Text, {Parents=[text, inscription, Arc={arc, _Arc_Num}|_],
                  Names, Tab_id}) ->
    Inscription = list_to_integer(Text),
    [{Arc, Arc_map}] = ets:lookup(Tab_id, Arc),
    Arc_map2 = maps:update(inscription, Inscription, Arc_map),
    true = ets:update_element(Tab_id, Arc, {2,Arc_map2}),
    {Parents, Names, Tab_id};

h_ets_text(Text, {Parents, Names, Tab_id}) ->
    ?LOG_DEBUG("h_ets_text: text ignored Text=~p, Parents=~p.", [Text, Parents]),
    {Parents, Names, Tab_id}.


%%--------------------------------------------------------------------
%% @doc Add a new reference name to the Names map for `referencePlace'
%% and `referenceTransition' PNML elements.
%%
%% `Id' and `Ref' are respectively the `id' and `ref' attributes of
%% the reference element. Both are of `string()' type as provided by
%% `xmerl', however, they are converted to `binary' strings when saved
%% in the map.
%%
%% `Map' is the table of symbols that is produced and maintained
%% during parsing.
%%
%% `Next_num' is the number to be assigned to the next symbol. This
%% number is not used for references.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_id_ref(string(), string(), {map(), integer()}) -> {map(), integer()}.
add_id_ref(Id, Ref, {Map, Next_num}) ->
    Id_bin  = list_to_binary(Id),
    Ref_bin = list_to_binary(Ref),
    {maps:put(Id_bin, Ref_bin, Map), Next_num}.


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
-spec get_id_num(string(), ets:tid()) -> integer().
get_id_num(Id, Names_tid) ->
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
%% names/numbers. `last_num' is initialized to zero during the first
%% attempt at incrementing it, and is incremented during each call.
%%
%% @end
%%--------------------------------------------------------------------
-spec next_num(ets:tid()) -> integer().
next_num(Tab_id) ->
    ets:update_counter(Tab_id, last_num, 1, {last_num, 0}).
