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
                ?_assertEqual(ok, ecgi:apply_handler(wrap(Middleware, fun() -> ok end)))
        end,

    [Test({ecgi, apply_handler, []}),
     Test({fun ecgi:apply_handler/1, []}),
     Test(fun ecgi:apply_handler/1)].
-endif.
