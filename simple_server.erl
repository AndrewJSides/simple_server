-module(simple_server).
-export([start/0, stop/0, init/2]).

start() ->
    % odbc:start(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/erl/hello", ?MODULE, hello},
            {"/erl/submit", ?MODULE, submit},
            {"/erl/fetch", ?MODULE, fetch},
            {"/erl/delete", ?MODULE, delete}
        ]}
    ]),
    Port = list_to_integer(os:getenv("PORT", "8080")),
    Ip = {0, 0, 0, 0},
    case cowboy:start_clear(http_listener,
                            [{port, Port}, {ip, Ip}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _Pid} ->
            io:format("Cowboy server started on ~p:~p~n", [Ip, Port]),
            % Keep the process alive indefinitely
            timer:sleep(infinity);
        {error, Reason} ->
            io:format("Failed to start Cowboy: ~p~n", [Reason]),
            error(Reason)
    end.
    % {ok, _} = cowboy:start_clear(http_listener,
    %     [{port, Port}, {ip, Ip}],
    %     #{env => #{dispatch => Dispatch}}
    % ),
    % io:format("Cowboy server started on ~p:~p~n", [Ip, Port]),
    % ok.

stop() ->
    cowboy:stop_listener(http_listener),
    odbc:stop(),
    io:format("Cowboy server stopped~n"),
    ok.

common_headers() ->
    #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"Content-Type">>,
        <<"content-type">> => <<"application/json">>
    }.

init(Req0, hello) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            Body = jsx:encode(#{message => <<"Hello from Erlang!">>, time => list_to_binary(io_lib:format("~p", [erlang:universaltime()]))}),
            Req = cowboy_req:reply(200, common_headers(), Body, Req0),
            {ok, Req, hello};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(204, common_headers(), <<>>, Req0),
            {ok, Req, hello};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"text/html">>}, <<"<h1>405 Method Not Allowed</h1>">>, Req0),
            {ok, Req, hello}
    end;

init(Req0, submit) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req0),
            Type = proplists:get_value(<<"TYPE">>, Body),
            case Type of
                <<"NOTE">> ->
                    Title = proplists:get_value(<<"title">>, Body),
                    Text = proplists:get_value(<<"text">>, Body),
                    User = proplists:get_value(<<"user">>, Body),
                    Ts = erlang:timestamp(),
                    Date_formatted = calendar:now_to_datetime(Ts),
                    io:format("Date: ~p~n", [Date_formatted]),
                    ok = process_note(Title, Text, User),
                    RespBody = jsx:encode(#{status => <<"success">>, title => Title, text => Text, user => User, created => Date_formatted, modified => Date_formatted}),
                    Req = cowboy_req:reply(200, common_headers(), RespBody, Req1),
                    {ok, Req, submit};
                _ ->
                    Req = cowboy_req:reply(400, common_headers(), jsx:encode(#{error => <<"Unsupported Type">>}), Req1),
                    {ok, Req, submit}
            end;
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(204, common_headers(), <<>>, Req0),
            {ok, Req, submit};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"text/html">>}, <<"<h1>405 Method Not Allowed</h1>">>, Req0),
            {ok, Req, submit}
    end;

init(Req0, fetch) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            Notes = fetch_notes(),
            Body = jsx:encode(Notes),
            Req = cowboy_req:reply(200, common_headers(), Body, Req0),
            {ok, Req, fetch};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(204, common_headers(), <<>>, Req0),
            {ok, Req, fetch};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"text/html">>}, <<"<h1>405 Method Not Allowed</h1>">>, Req0),
            {ok, Req, fetch}
    end;

init(Req0, delete) ->
    io:format("Received delete request~n"),
    Method = cowboy_req:method(Req0),
    io:format("Method: ~p~n", [Method]),

    case Method of
        <<"DELETE">> ->
            io:format("Parsing the delete request...~n"),
            {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req0),
            Note_id = proplists:get_value(<<"note_id">>, Body),
            io:format("Note id: ~p~n",[Note_id]),
            delete_note(Note_id),
            io:format("UPDATED v2"),
            ResponseBody = jsx:encode(#{status => <<"success">>, note_id => Note_id}),
            Req = cowboy_req:reply(200, common_headers(), ResponseBody, Req1),
            {ok, Req, delete};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(204, common_headers(), <<>>, Req0),
            {ok, Req, submit};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"text/html">>}, <<"<h1>405 Method Not Allowed</h1>">>, Req0),
            {ok, Req, hello}    
    end.

process_note(Title, Content, User) ->
    io:format("Processing note - Title: ~p, Content: ~p, User: ~p~n", [Title, Content, User]),
    {ok, Ref} = odbc:connect("DSN=mysql_home;user=root;password=MnQOxl3Q0Z", []),
    odbc:sql_query(Ref, "USE home"),
    QueryText = io_lib:format("INSERT INTO notes (title, content, owner) VALUES ('~s', '~s', '~s')", 
                              [binary_to_list(Title), binary_to_list(Content), binary_to_list(User)]),
    odbc:sql_query(Ref, QueryText),
    io:format("sql_query Complete~n"),
    odbc:disconnect(Ref),
    io:format("disconnect Complete~n"),
    ok.

fetch_notes() ->
    io:format("Fetching notes~n"),
    {ok, Ref} = odbc:connect("DSN=mysql_home;user=root;password=MnQOxl3Q0Z", []),
    io:format("Connected to db~n"),

    odbc:sql_query(Ref, "USE home"),
    io:format("Using home...~n"),

    QueryText = "SELECT id, title, content, created, modified, owner FROM notes",
    {selected, _Cols, Data} = odbc:sql_query(Ref, QueryText),

    Notes = lists:map(fun({Id, Title, Content, CreatedAt, UpdatedAt, Owner}) ->
        #{
            id => Id,
            title => list_to_binary(Title),
            content => list_to_binary(Content),
            created_at => list_to_binary(format_datetime(CreatedAt)),
            updated_at => list_to_binary(format_datetime(UpdatedAt)),
            owner => list_to_binary(Owner)
        }
    end, Data),
    odbc:disconnect(Ref),
    Notes.   

delete_note(Note_id) ->
    io:format("Deleting note id: ~p~n", [Note_id]),
    {ok, Ref} = odbc:connect("DSN=mysql_home;user=root;password=MnQOxl3Q0Z", []),
    io:format("Connected to db~n"),
    QueryText = io_lib:format("DELETE FROM notes WHERE id=~s", [Note_id]),
    odbc:sql_query(Ref, QueryText),
    io:format("Delete Complete~n"),
    odbc:disconnect(Ref),
    io:format("disconnect Complete~n"),
    ok.    

format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                  [Year, Month, Day, Hour, Minute, Second]).

