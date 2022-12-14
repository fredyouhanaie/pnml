%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyborg@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% Example of using the `pnml_ets' callbacks.
%%%
%%% @end
%%% Created : 14 Dec 2022 by Fred Youhanaie <fyborg@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_ets_counter).

%%--------------------------------------------------------------------

-export([get_counts/1]).

%%--------------------------------------------------------------------

-define(CB_table_name, pnml_ets_counter).

%%--------------------------------------------------------------------

-spec get_counts(string()) -> list() | {error, term()}.
get_counts(File) ->
    Handle_new = fun (Tag, _Id_num) ->
                         ets:update_counter(?CB_table_name, Tag, 1, {Tag,0}),
                         ok
                 end,
    ?CB_table_name = ets:new(?CB_table_name, [named_table]),
    Result = case pnml_ets:read_pt(File, Handle_new) of
                 {ok, _Names_tid, _Net_tid} ->
                     lists:sort(ets:tab2list(?CB_table_name));
                 {Error, _Names_tid, _Net_tid} ->
                     Error
             end,
    pnml_ets:cleanup(),
    ets:delete(?CB_table_name),
    Result.

%%--------------------------------------------------------------------
