%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang:anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% The pnml `logger' callback module
%%%
%%% The details of each callback invocation are sent to the logger.
%%%
%%% The handler state variable should be a map. If it contains the key
%%% `log_level', then that level will be used for the logging. Otherwise, the
%%% logging will be at `notice' level.
%%%
%%% @end
%%% Created : 15 Jan 2022 by Fred Youhanaie <fyrlang:anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_logger).

-behaviour(pnml).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc helper function to scan a document.
%%
%% We use a minimal logger format to produce one line per callback invocation.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(string()) -> pnml:read_ret().
start(File) ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    pnml:read(File, ?MODULE, #{}).

%%--------------------------------------------------------------------
%% @doc The callback handler for the begin tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), map()) -> map().
handle_begin(Tag, Attr, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:S: Tag=~p, Attr=~p.", [Tag, pnml:attr_map(Attr)]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------
%% @doc The callback handler for the end tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), map()) -> map().
handle_end(Tag, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:E: Tag=~p.", [Tag]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------
%% @doc The callback handler for the text elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), map()) -> map().
handle_text(Text, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:T: Text=~p.", [Text]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------
