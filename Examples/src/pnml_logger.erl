%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang:anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% The pnml `logger' callback module
%%%
%%% Details of each element is sent to the logger
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

start(File) ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    pnml:read(File, ?MODULE, #{}).

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
-spec handle_begin(atom(), list(), map()) -> map().
handle_begin(Tag, Attr, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:S: Tag=~p, Attr=~p.", [Tag, pnml:attr_map(Attr)]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------

-spec handle_end(atom(), map()) -> map().
handle_end(Tag, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:E: Tag=~p.", [Tag]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------

-spec handle_text(atom(), map()) -> map().
handle_text(Text, State) ->
    Level = maps:get(log_level, State, notice),
    ?LOG(Level, "pnml:T: Text=~p.", [Text]),
    maps:put(log_level, Level, State).

%%--------------------------------------------------------------------
