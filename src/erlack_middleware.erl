-module(erlack_middleware).

-export([wrap/2]).

wrap({M,F,A}, Handler) ->
    {M,F,[Handler|A]};
wrap({F,A}, Handler) ->
    {F, [Handler|A]};
wrap(Fun, Handler) when is_function(Fun, 1) ->
    {erlang, apply, [Fun, [Handler]]}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
wrap_test_() ->
    Test =
        fun(Middleware) ->
                ?assert(ok =:= ecgi:apply_handler(wrap(Middleware, fun() -> ok end)))
        end,

    [?_test(Test({ecgi, apply_handler, []})),
     ?_test(Test({fun ecgi:apply_handler/1, []})),
     ?_test(Test(fun ecgi:apply_handler/1))].
-endif.
