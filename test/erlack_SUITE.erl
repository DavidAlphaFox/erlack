-module(erlack_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [eunit, {group, debug_server}].

groups() ->
    [{debug_server, [], [echo]}].


init_per_testcase(_, Config) ->
    case ?config(tc_group_properties, Config) of
        [{name, debug_server},{suite,?MODULE}] ->
            Handler = ct:get_config(handler),
            Port = ct:get_config(port, 8000),
            Options = ct:get_config(options, []),
            Pid = erlack_debug_server:start(Port, Options, Handler),
            [{port, Port}, {pid, Pid}|Config];
        _ ->
            Config
    end.

end_per_testcase(_, Config) ->
    case ?config(tc_group_properties, Config) of
        [{name, debug_server}] ->
            Pid = ?config(pid, Config),
            exit(Pid, kill),
            ok;
        _ ->
            ok
    end.


eunit(_Config) ->
    ok = eunit:test({application, erlack}, [verbose]).


recv_headers(Conn, Headers) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            recv_headers(Conn, Headers#{Field => Value});
        {ok, http_eoh} ->
            Headers
    end.


request(Port, Request) ->
    {ok, Conn} =
        gen_tcp:connect(
          {127,0,0,1},
          Port,
          [binary, {packet,http_bin}, {active, false}]),

    ok = gen_tcp:send(Conn, Request),

    {ok, {http_response, {1,1}, Code, Reason}} =
        gen_tcp:recv(Conn, 0),

    Headers = recv_headers(Conn, #{}),
    ok = inet:setopts(Conn, [{packet, raw}]),

    {ok, Body} =
        gen_tcp:recv(Conn, binary_to_integer(maps:get('Content-Length', Headers))),
    {response, {Code, Reason}, Headers, Body}.


echo() ->
    [{require, handler},
     {default_config, handler,
      fun () ->
              Length = binary_to_integer(get(<<"HTTP_CONTENT_LENGTH">>)),
              {ok, Body} = ecgi:recv(Length),
              {response,
               {200, <<"OK">>},
               #{<<"Content-Length">> => integer_to_binary(iolist_size(Body))},
               Body}
      end
     }
    ].

echo(Config) ->
    Body = <<"OK">>,
    Response =
        {response,
         {200, <<"OK">>},
         #{'Content-Length' => integer_to_binary(iolist_size(Body))},
         Body},

    Response =
        request(
          ?config(port, Config),
          [<<"PUT / HTTP/1.1\r\n">>,
            <<"Content-Length: ">>, integer_to_binary(iolist_size(Body)), <<"\r\n">>,
            <<"\r\n">>,
            Body
          ]).
