%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2021-2022, Fred Youhanaie
%%% @doc
%%%
%%% A set of functions to process `PNML' files - Petri Net Markup Language. See
%%% the overview doc for further details.
%%%
%%% This is a behaviour module to be used by other modules, such as pnml_ets and
%%% pnml_counter, via callbacks.
%%%
%%% To start the process of parsing the pnml file, the callback module
%%% should call the `read/3' function. While `read/3' is scanning the
%%% PNML file it will call the appropriate handler functions.
%%%
%%% The callback module should provide three callback functions:
%%%
%%% <ul>
%%%
%%% <li>`handle_begin/3' will be called whenever a start tag is
%%% encountered,</li>
%%%
%%% <li>`handle_end/2' will be called whenever an end tag is encountered.</li>
%%%
%%% <li>`handle_text/2' is called whenever character data is seen.</li>
%%%
%%% </ul>
%%%
%%% All three callback handlers are passed the behaviour state, and should
%%% return the state, optionally updated.
%%%
%%% @end
%%% Created : 16 Jan 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(pnml).

-export([read/3, attr_map/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/logger.hrl").

%% record type used for the SAX `user_state'
-record(state, {cb_module, cb_state}).

%%--------------------------------------------------------------------

-type read_ret() :: {ok, State::term()} | {error, Reason::term()}.

%%--------------------------------------------------------------------

-callback handle_begin(atom(), term(), term()) -> term().
-callback handle_end(atom(), term()) -> term().
-callback handle_text(string(), term()) -> term().

%%--------------------------------------------------------------------
%% @doc Read in a valid XML file and process its elements using the supplied
%% callback module.
%%
%% It should be noted that `read/3' will read and process any valid XML file, it
%% is up to the supplied handler to process the PNML related aspects of the
%% file.
%%
%% We expect the file to be a valid XML document, no validation is performed
%% here. However, the supplied callback module can perform its own validation
%% during the scan.
%%
%% We return success/failure result. In case of success, the handler's final
%% state is returned. The failure reason may originate from the
%% `xmerl_sax_parser' module.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(string(), atom(), term()) -> read_ret().
read(Filename, CB_module, CB_state) ->
    ?LOG_INFO("read: scan started File=~p.", [Filename]),

    Scan_opts = [{event_fun, fun event_cb/3},
                 {event_state, #state{cb_module=CB_module,
                                      cb_state=CB_state}}
                ],

    Result = case xmerl_sax_parser:file(Filename, Scan_opts) of
                 {ok, State, _Rest} ->
                     {ok, State#state.cb_state};
                 Other_result ->
                     ?LOG_ERROR("read: could not read/parse file (~p).", [Other_result]),
                     Other_result
             end,
    %% collect the statistics and return the results
    ?LOG_INFO("read: Result=~p).", [Result]),
    Result.


%%--------------------------------------------------------------------
%% @doc The event handler for the SAX parser.
%%
%% We basically handle all the tag starts, ends and character contents, and
%% ignore the rest.
%%
%% The SAX user state is passed to the `handler_caller' as is, and the result is
%% returned to the SAX parser.
%%
%% It should be noted that `event_cb/3' does not understand, or care about, PNML
%% files. At this level, this is very much plain XML processing. All the PNML
%% related processing is carried out in the callback function.
%%
%% @end
%%--------------------------------------------------------------------
-spec event_cb({atom(), string(), string(), string(), list()}, term(), term()) -> term().
event_cb({startElement, _Uri, LocalName, _QualName, Attr}, _Loc,
         State=#state{cb_module=CB_module, cb_state=CB_state0}) ->
    Tag = list_to_atom(LocalName),
    CB_state1 = CB_module:handle_begin(Tag, Attr, CB_state0),
    State#state{cb_state=CB_state1};

event_cb({endElement, _Uri, LocalName, _QualName}, _Loc,
         State=#state{cb_module=CB_module, cb_state=CB_state0}) ->
    Tag = list_to_atom(LocalName),
    CB_state1 = CB_module:handle_end(Tag, CB_state0),
    State#state{cb_state=CB_state1};

event_cb({characters, Text}, _Loc,
         State=#state{cb_module=CB_module, cb_state=CB_state0}) ->
    CB_state1 = CB_module:handle_text(Text, CB_state0),
    State#state{cb_state=CB_state1};

event_cb(Event, _Loc, State) ->
    %% we ignore all the other events
    ?LOG_DEBUG("event_cb: IGNORED Ev=~p, St=~p.", [Event, State]),
    State.

%%--------------------------------------------------------------------
%% @doc Return a map corresponding to the list of attributes from xmerl.
%%
%% We extract the attribute names and values, and ignore the rest.
%%
%% The attribute names are returned as atoms and the values as strings.
%%
%% @end
%%--------------------------------------------------------------------
-spec attr_map(list()) -> map().
attr_map(Attr) ->
    Attr_to_pair = fun ({_, _, Name, Val}) -> {list_to_atom(Name), Val} end,
    Attr_list = lists:map(Attr_to_pair, Attr),
    maps:from_list(Attr_list).

%%--------------------------------------------------------------------
