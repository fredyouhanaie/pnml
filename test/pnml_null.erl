%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2022, Fred Youhanaie
%%% @doc
%%%
%%% The pnml `null' callback module
%%%
%%% As the name implies, no processing performed on the PNML/XML
%%% element. This callback module is purely for testing and
%%% performance benchmarking.
%%%
%%% @end
%%% Created : 15 Jan 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_null).

-behaviour(pnml).

-export([start/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------

start(File) ->
    pnml_cb:read(File, ?MODULE, null).

%%--------------------------------------------------------------------
%% @doc The null callbacks. These do nothing, just behaves as a
%% "compliant callback handler".
%%
%% This can be used for testing or benchmarking purposes.
%%
%% @end
%%--------------------------------------------------------------------

-spec handle_begin(atom(), term(), term()) -> term().
handle_begin(_Tag, _Attr, State) ->
    State.

%%--------------------------------------------------------------------

-spec handle_end(atom(), term()) -> term().
handle_end(_Tag, State) ->
    State.

%%--------------------------------------------------------------------

-spec handle_text(string(), term()) -> term().
handle_text(_Text, State) ->
    State.

%%--------------------------------------------------------------------
