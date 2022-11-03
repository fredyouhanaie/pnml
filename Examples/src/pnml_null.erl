%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fy@localhost>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% The pnml `null' callback module.
%%%
%%% As the name implies, no processing is performed on the PNML/XML elements.
%%% This callback module is purely for testing and performance benchmarking.
%%%
%%% @end
%%% Created : 15 Jan 2022 by Fred Youhanaie <fy@localhost>
%%%-------------------------------------------------------------------
-module(pnml_null).

-behaviour(pnml).

-export([start/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc Helper function to scan a PNML document.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(string()) -> {ok, State::term()} | {error, Reason::term()}.
start(File) ->
    pnml:read(File, ?MODULE, null).

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), term(), term()) -> term().
handle_begin(_Tag, _Attr, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), term()) -> term().
handle_end(_Tag, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), term()) -> term().
handle_text(_Text, State) ->
    State.

%%--------------------------------------------------------------------
