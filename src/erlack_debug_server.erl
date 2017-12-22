-module(erlack_debug_server).

-export([start/3]).

-export([init/3, loop/2, handle_connection/2]).


start(Port, Options, Handler) ->
    spawn(?MODULE, init, [Port, Options, Handler]).


init(Port, Options, Handler) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http_bin},
           {active, false},
           {reuseaddr, true}|Options]),
    ?MODULE:loop(Socket, Handler).


loop(Socket, Handler) ->
    case gen_tcp:accept(Socket, 200) of
        {ok, Conn} ->
            Pid = spawn(?MODULE, handle_connection, [Conn, Handler]),
            ok = gen_tcp:controlling_process(Conn, Pid),
            Pid ! {socket_ownership_transferred, Conn};
        {error, timeout} ->
            ok
    end,
    ?MODULE:loop(Socket, Handler).


handle_connection(Conn, Handler) ->
    receive
        {socket_ownership_transferred, Conn} ->
            ok
    after 1000 ->
            throw(timeout)
    end,

    {ok, {http_request, Method, {abs_path, Path}, Version}} =
        gen_tcp:recv(Conn, 0),

    put(<<"REQUEST_METHOD">>, ecgi_server:normalize_method(Method)),
    put(<<"REQUEST_URI">>, Path),
    put(<<"SERVER_PROTOCOL">>, http_version(Version)),

    ok = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),

    put(ecgi_input, {gen_tcp, recv, Conn}),
    put(ecgi_output, {gen_tcp, send, Conn}),

    try
        ecgi_server:handle(Handler, <<"HTTP/1.1">>)
    after
        ok = gen_tcp:close(Conn)
    end.


http_version({X,Y}) ->
    unicode:characters_to_binary(io_lib:format("HTTP/~B.~B", [X,Y])).

recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            ecgi_server:set_header(Field, Value),
            recv_headers(Conn);
        {ok, http_eoh} ->
            ok
    end.
