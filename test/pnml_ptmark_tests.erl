%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2022, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the PNML PT marking module.
%%%
%%% @end
%%% Created : 1 Mar 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(pnml_ptmark_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

add_test_() ->
    {"addition of two markings",
     [{"empty markings",
       ?_assert(pnml_ptmark:add(#{}, #{}) == #{})},

      {"normal markings",
       ?_assert(pnml_ptmark:add(#{a=>1, b=>2},
                                #{b=>1, c=>3})
                == #{a=>1, b=>3, c=>3})},

      {"M1 is empty",
       ?_assert(pnml_ptmark:add(#{},
                                #{b=>1, c=>3})
                == #{b=>1, c=>3})},

      {"M2 is empty",
       ?_assert(pnml_ptmark:add(#{b=>1, c=>3},
                                #{})
                == #{b=>1, c=>3})},

      {"zero value in input marking",
       ?_assert(pnml_ptmark:add(#{a=>0, b=>2},
                                #{b=>1, c=>3})
                == #{b=>3, c=>3})}
     ]}.

%%--------------------------------------------------------------------

sub_test_() ->
    {"subtraction of two markings",
     [{"two empty maps, i.e. zero tokens in both",
       ?_assert(pnml_ptmark:sub(#{}, #{}) == #{})},

      {"M2 is strict subset of M1",
       ?_assert(pnml_ptmark:sub(#{a=>1, b=>2}, #{b=>1})
                == #{a=>1, b=>1})},

      {"M1 and M2 have identical keys",
       ?_assert(pnml_ptmark:sub(#{a=>3, b=>2}, #{a=>1, b=>1})
                == #{a=>2, b=>1})},

      {"M1 is less than M2",
       ?_assert(pnml_ptmark:sub(#{b=>2}, #{a=>1, b=>1})
                == #{a=>-1, b=>1})},

      {"zero elements in result are removed",
       ?_assert(pnml_ptmark:sub(#{a=>1, b=>2}, #{a=>1, b=>1})
                == #{b=>1})},

      {"zero elements in input are removed",
       ?_assert(pnml_ptmark:sub(#{a=>1, b=>2, c=>0}, #{a=>1, b=>1})
                == #{b=>1})}
      ]}.

%%--------------------------------------------------------------------

greq_test_() ->
    {"greater or equal",
     [{"two empty maps, i.e. zero tokens",
       ?_assert(pnml_ptmark:greater_equal(#{},
                                          #{}))},

      {"two equal markings",
       ?_assert(pnml_ptmark:greater_equal(#{a=>1},
                                          #{a=>1}))},

      {"M1 > M2",
       ?_assert(pnml_ptmark:greater_equal(#{a=>1, b=>2},
                                          #{b=>1}))},

      {"M1 < M2",
       ?_assert(not pnml_ptmark:greater_equal(#{b=>1},
                                              #{a=>1, b=>2}))},

      {"non-zero places of M1 subset of non-zero places of M2",
       ?_assert(not pnml_ptmark:greater_equal(#{b=>1},
                                              #{a=>1, b=>2}))},

      {"neither of the non-zero places of M1/M2 subset of the other.",
       ?_assert(not pnml_ptmark:greater_equal(#{a=>1, b=>2},
                                              #{b=>1, c=>3}))}
     ]}.

%%--------------------------------------------------------------------
