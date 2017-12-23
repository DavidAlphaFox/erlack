-module(erlack_content_length).

-export([middleware/1]).


middleware(Handler) ->
    {response, Status, Headers, Body} = ecgi:apply_handler(Handler),
    case Body of
        {chunked, BodyHandler} ->
            {response, Status,
             Headers#{<<"Transfer-Encoding">> => <<"chunked">>},
             {handler, {ecgi, chunked_output, BodyHandler}}};
        {handler, _} ->
            {response, Status, Headers, Body};
        _ ->
            {response, Status,
             Headers#{<<"Content-Length">> => integer_to_binary(iolist_size(Body))},
             Body}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

middleware_test_() ->
    Test =
        fun(Handler, Headers, Body) ->
                {response, _, Headers1, Body1} = middleware(Handler),
                ?assertEqual(Headers, Headers1),
                ?assertEqual(Body, Body1)
        end,
    Handler = fun(Body) -> fun() -> {response, 200, #{}, Body} end end,

    [?_test(Test(Handler(<<"">>), #{<<"Content-Length">> => <<"0">>}, <<"">>)),
     ?_test(Test(Handler({chunked, none}), #{<<"Transfer-Encoding">> => <<"chunked">>}, {handler, {ecgi, chunked_output, none}})),
     ?_test(Test(Handler({handler, none}), #{}, {handler, none}))].

-endif.
