%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2022, Fred Youhanaie
%%% @doc
%%%
%%% Functions to handle P/T net markings.
%%%
%%% A marking is represented as a `map' with the places as keys and the number
%%% of tokens as the corresponding value.
%%%
%%% Only places with non-zero tokens are maintained. All functions that generate
%%% or compute new markings will remove the zero-token places before returning a
%%% marking.
%%%
%%% @end
%%% Created : 1 Mar 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(pnml_ptmark).

-export([add/2, sub/2, greater_equal/2, remove_zeros/1]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc add two markings and return the result.
%%
%% Each input marking is a map of places to the number of tokens in that place.
%%
%% The resulting map will contain the union of the keys of the input maps. The
%% keys that are in the intersection of the two maps will have their values
%% (token counts) added together.
%%
%% The addition of the markings will not generate any places with zero tokens,
%% however, places with zero tokens in the result will be checked for and
%% removed. This can only occur if such elements exist in the input maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec add(map(), map()) -> map().
add(M1, M2) ->
    %% initially Acc is M1, we add/insert the M2 elements into Acc
    MM = maps:fold(fun (K, V, Acc) ->
                           Acc#{K => maps:get(K, Acc, 0)+V}
                   end, M1, M2),
    remove_zeros(MM).

%%--------------------------------------------------------------------
%% @doc The marking `M2' is subtracted from the marking `M1'.
%%
%% We do not check if the set of places (keys) in `M2' is a subset of those in
%% `M1', as this is expected to have been ensured by the caller, see
%% `greater_equal/2', before the subtraction. It is therefore possible to end up
%% with negative number of tokens!
%%
%% Any places not in a marking (map), either `M1' or `M2' are assumed to have
%% zero tokens.
%%
%% Any places in the resulting marking with zero tokens will be removed from the
%% result.
%%
%% @end
%%--------------------------------------------------------------------
-spec sub(map(), map()) -> map().
sub(M1, M2) ->
    MM = maps:fold(fun (K, V, Acc) ->
                           Acc#{K => maps:get(K, Acc, 0)-V}
                   end, M1, M2),
    % remove places with zero tokens
    remove_zeros(MM).

%%--------------------------------------------------------------------
%% @doc returns `true' if marking `M1' >= `M2'.
%%
%% We compare the set of places which is the union of the set of places in both
%% markings. Any places in one but not the other marking is considered to have
%% zero tokens in the latter.
%%
%% This function can be used to check if a transition marking, `M2', is enabled
%% by its input places marking, `M1'.
%%
%% @end
%%--------------------------------------------------------------------
-spec greater_equal(map(), map()) -> boolean().
greater_equal(M1, M2) ->
    MM_places = lists:usort(maps:keys(M1) ++ maps:keys(M2)),
    F = fun (Place, Greater_Eq) ->
                Greater_Eq and (maps:get(Place, M1, 0) >= maps:get(Place, M2, 0))
        end,
    lists:foldl(F, true, MM_places).

%%--------------------------------------------------------------------
%% @doc Remove the places with zero tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_zeros(map()) -> map().
remove_zeros(M) ->
    Non_zero = fun (_Place, Tokens) -> Tokens /= 0 end,
    maps:filter(Non_zero, M).

%%--------------------------------------------------------------------
