-module(erlack_reason_phrase).

-export([middleware/1]).

middleware(Handler) ->
    {response, Status, Headers, Body} = ecgi:apply_handler(Handler),
    Status1 =
        case Status of
            {_,_} ->
                Status;
            Code when is_integer(Code) ->
                {Code, httpd_util:reason_phrase(Code)}
        end,
    {response, Status1, Headers, Body}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

middleware_test_() ->
    Test =
        fun(Handler, Status) ->
                {response, Status1, _, _} = middleware(Handler),
                ?assert(Status =:= Status1)
        end,

    Handler = fun(Status) -> fun() -> {response, Status, #{}, <<"">>} end end,

    [?_test(Test(Handler(200), {200, httpd_util:reason_phrase(200)})),
     ?_test(Test(Handler({200, <<"FINE">>}), {200, <<"FINE">>}))
    ].
-endif.
