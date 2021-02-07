%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% Tools to process `PNML' files - Petri Net Markup Language. See the
%%% overview docs for further details.
%%%
%%% @end
%%% Created : 16 Jan 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(pnml).

-export([read/1, read/2]).
-export([h_null/2, h_count/2, h_log/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/logger.hrl").

-define(Default_handler, {fun h_null/2, null}).

%% record type used for the SAX `user_state'
-record(state, {handler_fun, handler_state}).

%%--------------------------------------------------------------------
%% @doc read in an XML file with the default handler.
%%
%% See `read/2' for further details.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(string()) -> term().
read(Filename) ->
    read(Filename, ?Default_handler).


%%--------------------------------------------------------------------
%% @doc Read in a valid XML file and process its elements using the
%% supplied handler.
%%
%% It should be noted that `read/2' will read and process any valid
%% XML file, it is up to the supplied handler to process the PNML
%% related aspects of the file.
%%
%% We expect the file to be a valid XML document, no validation is
%% performed. However, the supplied handler can perform validation
%% during the scan.
%%
%% We return success/failure result. In case of success, the handler's
%% final state is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(string(),
              {fun((tuple(), term())->term()), term()}) ->
          ok | {error, term()}.
read(Filename, {Handler_fun, Handler_state}) ->
    ?LOG_INFO("read: scan started File=~p.", [Filename]),

    Scan_opts = [{event_fun, fun event_cb/3},
                 {event_state, #state{handler_fun=Handler_fun,
                                      handler_state=Handler_state}}
                ],

    Result = case xmerl_sax_parser:file(Filename, Scan_opts) of
                 {ok, State, _Rest} ->
                     {ok, State#state.handler_state};
                 Other_result ->
                     ?LOG_ERROR("read: could not read/parse file (~p).", [Other_result]),
                     Other_result
             end,
    %% collect the statistics and return the results
    ?LOG_INFO("read: Result=~p).", [Result]),
    Result.


%%--------------------------------------------------------------------
%% @doc event handler for the SAX parser.
%%
%% We basically handle all the tag starts, ends and character
%% contents.
%%
%% The SAX user state is passed to the `handler_caller' as is, and the
%% result is returned to the SAX parser.
%%
%% It should be noted that `event_cb/3' does not understand, or care
%% about, PNML files. At this level, this is very much plain XML
%% processing. All the PNML related processing is carried out in the
%% handler function.
%%
%% @end
%%--------------------------------------------------------------------
-spec event_cb(atom(), term(), tuple()) -> tuple().
event_cb({startElement, _Uri, LocalName, _QualName, Attr}, _Loc, State) ->
    Tag = list_to_atom(LocalName),
    call_handler({el_begin, Tag, Attr}, State);

event_cb({endElement, _Uri, LocalName, _QualName}, _Loc, State) ->
    Tag = list_to_atom(LocalName),
    call_handler({el_end, Tag}, State);

event_cb({characters, Text}, _Loc, State) ->
    call_handler({el_text, Text}, State);

event_cb(Event, _Loc, State) ->
    %% we ignore all the other events
    ?LOG_DEBUG("event_cb: IGNORED Ev=~p, St=~p.", [Event, State]),
    State.


%%--------------------------------------------------------------------
%% @doc Call the handler with the supplied `Arg' and SAX user `State'.
%%
%% The SAX user state, `State' should contain the handler function and
%% the current state.
%%
%% The return value is the updated handler state, as returned by the
%% handler function.
%%
%% @end
%%--------------------------------------------------------------------
-spec call_handler(term(), term()) -> term().
call_handler(Arg, State) ->
    H_fun = State#state.handler_fun,
    S_cur = State#state.handler_state,
    S_new = H_fun(Arg, S_cur),
    State#state{handler_state=S_new}.


%%--------------------------------------------------------------------
%% @doc The null handler. Does nothing, just behaves as a "compliant
%% handler".
%%
%% This can be used for testing or benchmarking purposes.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_null(tuple(), term()) -> term().
h_null(_, State) ->
    State.


%%--------------------------------------------------------------------
%% @doc Handler that counts the tags.
%%
%% Each `el_begin' call increments the count for the corresponding
%% `Tag'. Everything else, such as attributes, end tags and text are
%% ignored.
%%
%% The state variable for this handler should be a `map'. When calling
%% `read/2', it is recommended to supply an empty map as the initial
%% value, i.e. `#{}', although a map with preset values will not be
%% rejected. If a key for the `Tag' does not exist a new entry will be
%% created. If a non-numeric entry exists for the `Tag', the increment
%% operation will cause an exception!
%%
%% @end
%%--------------------------------------------------------------------
-spec h_count({el_begin, atom(), [tuple()]} | {el_end, atom()} | {el_text, string()},
              map()) -> map().
h_count({el_begin, Tag, _Attr}, Counts) ->
    ?LOG_INFO("Counts=~p.", [Counts]),
    C = maps:get(Tag, Counts, 0),
    maps:put(Tag, C+1, Counts);

h_count({el_end, _Tag}, Counts) ->
    Counts;

h_count({el_text, _Text}, Counts) ->
    Counts.


%%--------------------------------------------------------------------
%% @doc Handler that logs the elements.
%%
%% The details of the element will be sent to the logger.
%%
%% The handler state variable should be a map. If it contains the key
%% `log_level', then that key will be used for the logging. Otherwise,
%% the logging will be at `notice' level.
%%
%% @end
%%--------------------------------------------------------------------
-spec h_log({el_begin, atom(), [tuple()]} | {el_end, atom()} | {el_text, string()},
              map()) -> map().
h_log({el_begin, Tag, Attr}, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "h_log:S: Tag=~p, Attr=~p.", [Tag, attr_map(Attr)]),
    maps:put(log_level, Level, State);

h_log({el_end, Tag}, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "h_log:E: Tag=~p.", [Tag]),
    maps:put(log_level, Level, State);

h_log({el_text, Text}, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "h_log:T: Text=~p.", [Text]),
    maps:put(log_level, Level, State).
