%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% A handler that saves the net elements in an ETS table.
%%%
%%% The ETS table will contain one record for each of PNML elements in
%%% the document, i.e. `net', `place', `transition' and `arc'.
%%%
%%% Each of the Net elements have a unique string identifier, however,
%%% in the interest of efficiency, we maintain a symbol table,
%%% `Names', that maps the symbolic names (as binary strings) to
%%% unique numeric ids. The `Names' map is part of the handler state,
%%% and is returned when `pnml:read/2' completes successfully. The
%%% records in the ETS table will contain the numeric ids.
%%%
%%% This handler can be used as follows:
%%%
%%% <code>
%%% Tab_id = ets:new(Tab_name, Tab_opts),<br/>
%%% State0 = {fun pnml_ets:h_ets/2, {[], {#{}, 0}, Tab_id}},<br/>
%%% {ok, {[], {Names, Count}, Tab_id} = pnml:read(File, State0)
%%% </code>
%%%
%%% Where `Names' is a map of binary strings to unique numeric
%%% identifiers and `Count' is the number of entries in `Names'.
%%%
%%% If successful, the ETS table will contain one record for each Net
%%% element.
%%%
%%% @end
%%% Created :  7 Feb 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_ets).

-export([h_ets/2]).

-include_lib("kernel/include/logger.hrl").

%%-------------------------------------------------------------------

%% We are only intersted in the following element tags:
%%
-define(PT_elements, [pnml, net, place, arc, transition, text,
                      initialMarking, inscription, referencePlace,
                      referenceTransition]).

%%-------------------------------------------------------------------

-type h_ets_state() :: {list(), {map(), integer()}, ets:tid()}.

%%--------------------------------------------------------------------
%% @doc The main handler function.
%%
%% The handler state is a tuple `{Parents, Names, Tab_id}', where,
%% `Parents' is a list, initially empty, that keeps track of the
%% nested elements; `Names' is a tuple, `{Map, Next_num}', initially
%% `{#{}, 0}', is a symbol table for the element ids; and `Tab_id' is
%% the ETS table id.
%%
%% The terms stored in the ETS table are of the following types:
%%
%% <ul>
%%
%% <li>`{ {net, Num_id}, #{type => Net_type} }'</li>
%%
%% <li>`{ {place, Num_id}, #{net_num => Net_num, initial_marking => Initial_Marking} }'</li>
%%
%% <li>`{ {transition, Num_id}, #{net_num => Net_num, } }'</li>
%%
%% <li>`{ {arc, Num_id}, #{net_num => Net_num, source => Source, target => Target, inscription => Inscription} }'</li>
%%
%% </ul>
%%
%% Where:
%%
%% <ul>
%%
%% <li>`Num_id' is the unique numeric id assigned to each
%% element.</li>
%%
%% <li>`InitMarking' and `Inscription' are integers that are either
%% taken from the PNML document, or if missing are assigned default
%% values of `0' and `1' respectively.</li>
%%
%% <li>`Source' and `Target' are the unique numbers assigned to their
%% corresponding `place' or `transition' string identifiers.</li>
%%
%% </ul>
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
    {Id_num, Names2} = get_id_num(maps:get(id, Attr_map), Names),
    Type = list_to_binary(maps:get(type, Attr_map)),
    true = ets:insert(Tab_id, {{net, Id_num}, #{type => Type} }),
    {[{net, Id_num}, pnml], Names2, Tab_id};

h_ets_begin(place, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    {Id_num, Names2} = get_id_num(maps:get(id, Attr_map), Names),
    true = ets:insert(Tab_id, {{place, Id_num},
                               #{net_num=>Net_num,
                                 initial_marking=>0}
                              }),
    {[{place, Id_num}|Parents], Names2, Tab_id};

h_ets_begin(transition, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    {Id_num, Names2} = get_id_num(maps:get(id, Attr_map), Names),
    true = ets:insert(Tab_id, {{transition, Id_num},
                               #{net_num=>Net_num}
                              }),
    {[{transition, Id_num}|Parents], Names2, Tab_id};

h_ets_begin(arc, Attr_map, {Parents=[{net, Net_num}, pnml], Names, Tab_id}) ->
    {Id_num, Names2}     = get_id_num(maps:get(id, Attr_map), Names),
    {Source_num, Names3} = get_id_num(maps:get(source, Attr_map), Names2),
    {Target_num, Names4} = get_id_num(maps:get(target, Attr_map), Names3),

    true = ets:insert(Tab_id, {{arc, Id_num},
                               #{net_num => Net_num,
                                 source => Source_num,
                                 target => Target_num,
                                 inscription => 1}
                              }),
    {[{arc, Id_num}|Parents], Names4, Tab_id};

h_ets_begin(referencePlace, Attr_map, {Parents=[{net, _Net_num}, pnml], Names, Tab_id}) ->
    Names2 = add_id_ref(maps:get(id, Attr_map),
                                 maps:get(ref, Attr_map),
                                 Names),
    {[referencePlace|Parents], Names2, Tab_id};

h_ets_begin(referenceTransition, Attr_map, {Parents=[{net, _Net_num}, pnml], Names, Tab_id}) ->
    Names2 = add_id_ref(maps:get(id, Attr_map),
                                 maps:get(ref, Attr_map),
                                 Names),
    {[referenceTransition|Parents], Names2, Tab_id};

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
%% created. The number and the updated map is then returned.
%%
%% If the `Id' exists it may refer to a number or another name, for
%% `referencePlace' and `referenceTransition'. In the latter case the
%% lookup is repeated once more.
%%
%% The unique integer corresponding to the id name, along with the
%% existing map is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_id_num(string(), {map(), integer()}) -> {integer(), {map(), integer()}}.
get_id_num(Id, {Map, Next_num}) ->
    Id_bin = list_to_binary(Id),
    case maps:find(Id_bin, Map) of
        {ok, Num} when is_integer(Num) -> %% we have an integer entry
            %% We found a number entry
            {Num, {Map, Next_num}};

        {ok, Ref} -> %% we have a reference entry
            %% we found a ref entry
            %% look up the new ref
            case maps:find(Ref, Map) of
                {ok, Num} -> %% the second entry exists
                    %% we found an entry (we expect integer!)
                    {Num, {Map, Next_num}};
                error -> %% the second entry does not exist
                    %% create a num entry for the second Id
                    {Next_num,
                     {maps:put(Id_bin, Next_num, Map), Next_num+1}
                    }
            end;

        error -> %% entry not found
            %% create a num entry for the Id
            {Next_num,
             {maps:put(Id_bin, Next_num, Map), Next_num+1}
            }
    end.
