%    -*- Erlang -*-
%    File:      idrott.erl  (~jb/work/idrott/src/idrott.erl)
%    Author:    Johan Bevemyr
%    Created:   Tue Aug  5 14:29:44 2014
%    Purpose:

-module('idrott').
-author('jb@bevemyr.com').

-compile(export_all).
%%-export([Function/Arity, ...]).

-include("/usr/lib/yaws/include/yaws_api.hrl").

-behaviour(gen_server).

%% External exports
-export([start/0, stop/0, reset/0, hard_reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% appmod callback
-export([out/1]).

%%

-define(SERVER, ?MODULE).
-define(i2l(X), integer_to_list(X)).
-define(l2i(X), list_to_integer(X)).
-define(l2b(X), list_to_binary(X)).
-define(l2f(X), list_to_float(X)).
-define(b2l(X), binary_to_list(X)).
-define(a2l(X), atom_to_list(X)).
-define(l2a(X), list_to_atom(X)).

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).


-define(EMAIL_SENDER, "info@idrott.se").
-define(MAILSERVER, "mail.bevemyr.com").

-define(USER_DB, "/home/share/katrin/www/idrott/user.db.json").
-define(USER_DB_TMP, "/home/share/katrin/www/idrott/user.db.json.tmp").

-define(EVENTS_DB, "/home/share/katrin/www/idrott/events.db.json").
-define(EVENTS_DB_TMP, "/home/share/katrin/www/idrott/events.db.json.tmp").

-define(DATAFILE, "/home/share/katrin/www/idrott/key_value.db").

%%

-record(post_state, {count=0, acc=[]}).

out(#arg{req = #http_request{method = 'POST'},
         headers = #headers{
           content_type = "multipart/form-data"++_}} = A) ->
    io:format("here\n", []),
    case yaws_api:parse_multipart_post(A) of
        [] ->
            Res = {struct, [{status, "error"},{reason, "broken post"}]},
            rpcreply(Res);
        {cont, _, _}
          when is_record(A#arg.state, post_state),
               A#arg.state#post_state.count == 10 ->
            Res = {struct, [{status, "error"},{reason, "to big post"}]},
            rpcreply(Res);
        {cont, Cont, Res}  ->
            PState = A#arg.state,
            Count = PState#post_state.count,
            Acc = PState#post_state.acc,
            {get_more, Cont, PState#post_state{count=Count+1,acc=[Acc|Res]}};
        {result, Res} ->
            PState = A#arg.state,
            Acc = PState#post_state.acc,
            PostStr = skip_ws(dequote(?b2l(?l2b([Acc|Res])))),
            case json2:decode_string(PostStr) of
                {ok, Json} ->
                    L = yaws_api:parse_query(A),
                    do_op(A#arg.appmoddata, L, Json, A);
                _Reason ->
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid json"}]},
                    rpcreply(Res)
            end
    end;
out(#arg{req = #http_request{method = 'POST'},
         headers = #headers{
           content_type = "application/x-www-form-urlencoded"++_}} = A) ->
    Clidata = ?b2l(A#arg.clidata),
    DecodedClidata = yaws_api:url_decode(Clidata),
    L = yaws_api:parse_query(A),
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    case json2:decode_string(dequote(DecodedClidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json, A);
        _Reason ->
            io:format("json=~p\n", [DecodedClidata]),
            io:format("got error ~p\n", [_Reason]),
            Res = {struct, [{status, "error"},
                            {reason, "invalid json"}]},
            rpcreply(Res)
    end;
out(#arg{req = #http_request{method = 'POST'}} = A) ->
    Clidata = ?b2l(A#arg.clidata),
    L = yaws_api:parse_query(A),
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    case json2:decode_string(dequote(Clidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json, A);
        _Reason ->
            io:format("json=~p\n", [Clidata]),
            io:format("got error ~p\n", [_Reason]),
            Res = {struct, [{status, "error"},
                            {reason, "invalid json"}]},
            rpcreply(Res)
    end;
out(A) ->
    QueryL = yaws_api:parse_query(A),
    L = case (A#arg.req)#http_request.method of
            'GET' ->
                QueryL;
            'POST' ->
                io:format("post=~p\n", [yaws_api:parse_post(A)]),
                QueryL ++ yaws_api:parse_post(A)
        end,
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    do_op(A#arg.appmoddata, L, [], A).

do_op(Cmd, L, Json, A) ->
    case gen_server:call(?SERVER, {Cmd, L, Json, A}, infinity) of
        Content = {content, _Type, _Data} ->
            Content;
        JsonAndHeaders ->
            rpcreply(JsonAndHeaders)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server

-record(user, {
          username = [],          %% string() - username (email addess)
          password = [],          %% string() - password MD5
          sid = [],               %% string() - session id
          confirmed = false,      %% boolean() - confirmed
          role = user,            %% admin | user - privilege group
          passwd_reset_id = [],   %% string() - password reset id
          passwd_reset_send_time=0, %% date_time() - send time time of last
                                    %% password reset email
          data=[]                 %% [{string(), string()}]
         }).

-record(event, {
          id,                  %% integer() - unique event id
          name=[],             %% string() - event name
          date="2014-10-01",   %% string() - event date
          data=[]              %% [{string(), string{}}]
         }).

-record(state, {
          users=[],            %% list of #user{}
          events=[],
          event_id=0,
          data=[]              %% [{Key,Value}]
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

reset() ->
    gen_server:call(?SERVER, reset, infinity).

hard_reset() ->
    gen_server:call(?SERVER, hard_reset, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {X,Y,Z} = erlang:now(),
    random:seed(X, Y, Z),
    Users = read_users(),
    Events = read_events(),
    Data = read_data(),
    {ok, #state{users=Users, events=Events, data=Data}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, _From, S) ->
    {stop, normal, S};

handle_call(reset, _From, S) ->
    Users = read_users(),
    Events = read_events(),
    {reply, ok, S#state{users=Users, events=Events}};

handle_call(hard_reset, _From, S) ->
    file:copy(?USER_DB++".start", ?USER_DB),
    file:copy(?EVENTS_DB++".start", ?EVENTS_DB),
    Users = read_users(),
    Events = read_events(),
    {reply, ok, S#state{users=Users, events=Events}};

handle_call({Cmd, L, Json, A}, _From, S) ->
    try
        {Res, NewS} = do_cmd(Cmd, L, Json, A, S),
        {reply, Res, NewS}
    catch
        X:Y ->
            error_logger:format("error during exection of cmd ~p: ~p ~p\n",
                                [{Cmd, L, Json}, {X,Y}, erlang:get_stacktrace()]),
            {reply, {struct, [{status, "error"}, {reason, "internal"}]}, S}
    end;

handle_call(_Request, _From, S) ->
    Res = {struct, [{status, "error"},
                    {reason, "unknown request"}]},
    {reply, Res, S}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_cmd(Spec="pdf/"++_, _L, _Json, _A, S) ->
    case string:tokens(Spec, "/") of
        ["pdf", Key|_] ->
            case lists:keysearch(Key, 1, S#state.data) of
                {value, {Key, Json}} ->
                    PdfContentData = gen_pdf(Json),
                    Res = {content, "application/pdf", PdfContentData};
                false ->
                    Res = {{struct, [{status, "error"}, {reason, "unknown key"}]}, []}
            end;
        _ ->
            Res = {{struct, [{status, "error"},
                             {reason, "malformed request"}]}, []}
    end,
    {Res, S};
do_cmd("put", L, Json, _A, S) ->
    Key = get_val("key", L, ""),
    case lists:keymember(Key, 1, S#state.data) of
        true ->
            NewData = lists:keyreplace(Key, 1, S#state.data, {Key, Json});
        false ->
            NewData = [{Key,Json}|S#state.data]
    end,
    Res = {struct, [{status, "ok"}]},
    save_data(NewData),
    {{Res,[]}, S#state{data=NewData}};
do_cmd("delete", L, _Json, _A, S) ->
    Key = get_val("key", L, ""),
    case lists:keymember(Key, 1, S#state.data) of
        true ->
            NewData = lists:keydelete(Key, 1, S#state.data);
        false ->
            NewData = S#state.data
    end,
    Res = {struct, [{status, "ok"}]},
    save_data(NewData),
    {{Res,[]}, S#state{data=NewData}};
do_cmd("get", L, _Json, _A, S) ->
    Key = get_val("key", L, ""),
    case lists:keysearch(Key, 1, S#state.data) of
        {value, {Key, Json}} ->
            Res = Json;
        false ->
            Res = {struct, [{status, "error"}, {reason, "unknown key"}]}
    end,
    {{Res,[]}, S};
%% http://idrott/idrott/login?user=johan&password=test
do_cmd("login", L, _Json, _A, S) ->
    User = get_val("user", L, ""),
    Password = get_val("password", L, ""),
    Remember = get_val("remember", L, "false"),
    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
    case get_user_by_name(User, S) of
        U=#user{password = Md5Pass} when Remember == "true" ->
            %% login successful
            {Y,M,D} = date(),
            Expires= format_expires_date({{Y+1,M,D},time()}),
            Headers=[yaws_api:setcookie("sid", U#user.sid, "/", Expires)],
            Res = {struct, [{status, "ok"}, {"sid", U#user.sid},
                            {group, ?a2l(U#user.role)}]};
        U=#user{password = Md5Pass} ->
            %% login successful
            Headers=[yaws_api:setcookie("sid", U#user.sid, "/")],
            Res = {struct, [{status, "ok"}, {"sid", U#user.sid},
                            {group, ?a2l(U#user.role)}]};
        #user{} ->
            Headers=[],
            Res = {struct, [{status, "error"}, {reason, "invalid password"}]};
        _ ->
            Headers=[],
            Res = {struct, [{status, "error"}, {reason, "unknown user"}]}
    end,
    {{Res,Headers}, S};
%% http://idrott/idrott/send_reset_password?user=johan
do_cmd("send_reset_password", L, _Json, _A, S) ->
    User = get_val("user", L, ""),
    case get_user_by_name(User, S) of
        U=#user{} ->
            RPid = mk_rpid(S),
            smtp:send(?MAILSERVER, ?EMAIL_SENDER, [User],
                      "Password reset request",
                      "You have requested a password reset. Follow the link\n"
                      "below to reset the password. Ignore this email if you haven't\n"
                      "requested a reset.",
                      "http://www.idrott.org/reset_password.html?rpid="++RPid, []),
            %% Note that the reset_password.html page should make a AJAX call to
            %% the reset_password?rpdi=<rpid from post to .html page>&password=<new password>
            Res = {struct, [{state, "ok"}]},
            NewUser = U#user{passwd_reset_id=RPid, passwd_reset_send_time=gnow()},
            NewUsers = update_user(NewUser, S#state.users),
            NewS = S#state{users=NewUsers};
        _ ->
            Res = {struct, [{state, "error"}, {reason, "unknown user"}]},
            NewS = S
    end,
    {{Res,[]}, NewS};
%% http://idrott/idrott/reset_password?rpid=1234567890&password=test
do_cmd("reset_password", L, _Json, _A, S) ->
    Rpid = get_val("rpid", L, ""),
    Password = get_val("password", L, ""),
    case get_user_by_rpid(Rpid, S) of
        U=#user{} ->
            Age = days_diff(U#user.passwd_reset_send_time, gnow()),
            if Age < 5 ->
                    %% login successful
                    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
                    NewU = U#user{password=Md5Pass},
                    Res = {struct, [{status, "ok"}, {user, user2object(U)}]},
                    {{Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
               true ->
                    Res = {struct, [{status, "error"},
                                    {reason, "password reset link has expired"}]},
                    {{Res, []}, S}
            end;
        _ ->
            Res = {struct, [{status, "error"}, {reason, "unknown sid"}]},
            {{Res, []}, S}
    end;
%% http://idrott/idrott/set_password?sid=1234567890&old_password=test&new_password=test2
do_cmd("set_password", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    OldPass = get_val("old_password", L, ""),
    NewPass = get_val("new_password", L, ""),
    Md5OldPass = ?b2l(base64:encode(crypto:hash(md5, OldPass))),
    Md5NewPass = ?b2l(base64:encode(crypto:hash(md5, NewPass))),
    case get_user_by_id(Sid, S) of
        U=#user{} when Md5OldPass == U#user.password ->
            %% successful auth of old pass
            NewU = U#user{password=Md5NewPass},
            Res = {struct, [{status, "ok"}]},
            {{Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{status, "error"}, {reason, "unknown sid"}]},
            {{Res, []}, S}
    end;
%% http://idrott/idrott/set_user_password?sid=1234567890&username=jb&password=test
do_cmd("set_user_password", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    Username = get_val("username", L, ""),
    NewPass = get_val("new_password", L, ""),
    Md5NewPass = ?b2l(base64:encode(crypto:hash(md5, NewPass))),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    NewU = OU#user{password=Md5NewPass},
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=update_user(NewU, S#state.users)},
                    {{Res, []}, NewS};
                false ->
                    Res = {struct, [{status, "error"},
                                    {reason, "unknown user"}]},
                    {{Res, []}, S}
            end;
        #user{} ->
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]},
            {{Res,[]}, S};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]},
            {{Res,[]}, S}
    end;
%% http://idrott/idrott/get_all_users?sid=1234567890
do_cmd("get_all_users", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        #user{} ->
            %% login successful
            Res = {struct, [{status, "ok"},
                            {users, {array, [user2object(U) ||
                                                U <- S#state.users]}}]};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res,[]}, S};
%% http://idrott/idrott/get_user?sid=1234567890
do_cmd("get_user", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            Res = {struct, [{status, "ok"}, {user, user2object(U)}]};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, S};
%% http://idrott/idrott/get_named_user?sid=1234567890&username=jb
do_cmd("get_named_user", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Username = get_val("username", L, ""),
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    Res = {struct, [{status, "ok"},
                                    {user, user2object(OU)}]};
                false ->
                    Res = {struct, [{status, "error"},
                                    {reason, "unknown user"}]}
            end;
        #user{} ->
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res,[]}, S};
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("get_selected_users", L, Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Users = get_user_by_select(Json, S),
            NewS = S,
            Res = {struct, [{status, "ok"},
                            {users, {array, [user2object(SU) ||
                                                SU <- Users]}}]};
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res,[]}, NewS};

%% http://idrott/idrott/send_reset_password?user=johan
do_cmd("mail_selected_users", L, {struct, JsonL}, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Selection = get_val("recievers", JsonL, []),
            Subject = get_val("subject", JsonL, []),
            Message = get_val("message", JsonL, []),
            Users = get_user_by_select(Selection, S),
            {SentTo, Rejected} = mail_users(Users, Subject, Message),
            NewS = S,
            SentToU = {array, [user2object(SU) || SU <- SentTo]},
            RejectedU = {array, [user2object(SU) || SU <- Rejected]},
            Res = {struct, [{status, "ok"},
                            {sent_to, SentToU},
                            {rejected, RejectedU}]};
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};

%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("set_user", L, Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            U2 = apply_user_ops(U,L),
            NewU = apply_user_json(U2, Json),
            Res = {struct, [{status, "ok"}]},
            {{Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]},
            {{Res, []}, S}
    end;
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("set_named_user", L, Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Username = get_val("username", L, ""),
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    U2 = apply_user_ops(OU,L),
                    NewU = apply_user_json(U2, Json),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=update_user(NewU, S#state.users)};
                false ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "unknown user"}]}
            end;
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};
%% http://idrott/idrott/add_user?sid=1234567890&username=jb&password=test&role=user&foo=bar
do_cmd("add_user", L0, Json, A, S) ->
    Sid = get_sid(A, L0),
    L = merge_attrs(L0, Json),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            %% login successful
            Username = get_val("username", L, ""),
            Password = get_val("password", L, ""),
            Role = get_val("role", L, ""),
            Exists = get_user_by_name(Username, S) =/= false,
            if Username == "" ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid username"}]};
               Password == "" ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid password"}]};
               Role == "" ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid role"}]};
               Exists ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "a user with the same name "
                                     "already exists"}]};
               true ->
                    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
                    U2 = #user{username=Username,
                               password=Md5Pass,
                               sid=mk_id(S),
                               role=?l2a(Role)},
                    NewU = apply_user_json(U2, Json),
                    store_users([NewU|S#state.users]),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=[NewU|S#state.users]}
            end;
        #user{} ->
            %% login successful
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only admin can create users"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};
%% http://idrott/idrott/del_user?sid=1234567890&username=jb
do_cmd("del_user", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            %% login successful
            Username = get_val("username", L, ""),
            DelUser = get_user_by_name(Username, S),
            if Username == "" ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid username"}]};
               DelUser == false ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "the user does not exist"}]};
               true ->
                    NewUsers = lists:delete(DelUser, S#state.users),
                    store_users(NewUsers),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=NewUsers}
            end;
        #user{} ->
            %% login successful
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only admin can remove users"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};
%% http://idrott/idrott/get_all_events?sid=1234567890
do_cmd("get_all_events", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        #user{} ->
            %% login successful
            Res = {struct, [{status, "ok"},
                            {events, {array, [event2object(U) ||
                                                 U <- S#state.events]}}]};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown event id"}]}
    end,
    {{Res, []}, S};
%% http://idrott/idrott/create_event?sid=1234567890
do_cmd("create_event", L, Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            %% login successful
            {S2, Id} = new_event_id(S),
            Event = apply_event_json(#event{id=Id}, Json),
            NewS = S2#state{events=[Event|S2#state.events]},
            store_events(NewS#state.events),
            Res = {struct, [{status, "ok"},
                            {event, event2object(Event)}]};
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only admin can create events"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown session id"}]}
    end,
    {{Res,[]}, NewS};
do_cmd("change_event", L0, Json, A, S) ->
    Sid = get_sid(A,L0),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            L = case Json of
                     {struct, Ops} ->  L0 ++ Ops;
                     _ -> L0
                 end,
            %% login successful
            EId = to_int(get_val("id", L, "")),
            case get_event_by_id(EId, S) of
                false ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid event id"}]};
                Event0 ->
                    Event = apply_event_ops(Event0, L),
                    NewEvents = update_event(Event, S#state.events),
                    NewS = S#state{events=NewEvents},
                    Res = {struct, [{status, "ok"},
                                    {event, event2object(Event)}]}
            end;
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only admin can create events"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown session id"}]}
    end,
    {{Res,[]}, NewS};
do_cmd("get_event", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        #user{} ->
            %% login successful
            EId = to_int(get_val("id", L, "")),
            case get_event_by_id(EId, S) of
                false ->
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid event id"}]};
                Event ->
                    Res = {struct, [{status, "ok"},
                                    {event, event2object(Event)}]}
            end;
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown event id"}]}
    end,
    {{Res, []}, S};
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("get_selected_events", L, Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Events = get_event_by_select(Json, S),
            NewS = S,
            Res = {struct, [{status, "ok"},
                            {events, {array, [event2object(E) ||
                                                 E <- Events]}}]};
        #user{} ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};
do_cmd("del_event", L, _Json, A, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            %% login successful
            Id = to_int(get_val("id", L, "")),
            DelEvent = get_event_by_id(Id, S),
            if DelEvent == false ->
                    NewS = S,
                    Res = {struct, [{status, "error"},
                                    {reason, "the event does not exist"}]};
               true ->
                    NewEvents = lists:delete(DelEvent, S#state.events),
                    store_events(NewEvents),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{events=NewEvents}
            end;
        #user{} ->
            %% login successful
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "only admin can remove event"}]};
        _ ->
            NewS = S,
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {{Res, []}, NewS};
do_cmd(Unknown, _L, _Json, _A, S) ->
    Error = lists:flatten(io_lib:format("unknown request: ~p", [Unknown])),
    error_logger:format("~s", [Error]),
    {{{struct, [{"status", "error"}, {reason, Error}]}, []}, S}.

rpcreply({Response, ExtraHeaders}) ->
    X = json2:encode(Response),
    [{header, {cache_control, "no-cache"}},
     {header, "Access-Control-Allow-Origin: *"},
     {header, "Expires: -1"}]++ExtraHeaders++[{html, X}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_users() ->
    {ok, B} = file:read_file(?USER_DB),
    {ok, {array, Users}} = json2:decode_string(?b2l(B)),
    [object2user(U) || U <- Users].

store_users(Users) ->
    UserStructs = [user2object(U) || U <- Users],
    String = idrott_json2:encode_pretty({array, UserStructs}),
    file:write_file(?USER_DB_TMP, String),
    file:rename(?USER_DB_TMP, ?USER_DB).

user2object(U) ->
    {struct,
     [{"username", U#user.username},
      {"password", U#user.password},
      {"sid", U#user.sid},
      {"confirmed", U#user.confirmed},
      {"role", ?a2l(U#user.role)},
      {"passwd_reset_id", U#user.passwd_reset_id},
      {"passwd_reset_send_time", U#user.passwd_reset_send_time}|
      U#user.data]}.

get_user_opt("username", U, _Default) ->
    U#user.username;
get_user_opt("password", U, _Default) ->
    U#user.password;
get_user_opt("sid", U, _Default) ->
    U#user.sid;
get_user_opt("confirmed", U, _Default) ->
    U#user.confirmed;
get_user_opt("role", U, _Default) ->
    U#user.role;
get_user_opt("password_reset_id", U, _Default) ->
    U#user.passwd_reset_id;
get_user_opt("password_reset_send_time", U, _Default) ->
    U#user.passwd_reset_send_time;
get_user_opt(Key, U, Default) ->
    case lists:keysearch(Key, 1, U#user.data) of
        {value, {_, Val}} ->
            Val;
        _ ->
            Default
    end.

object2user({struct, Props}) ->
    object2user(#user{}, Props, _Data=[]).

object2user(U, [], Data) ->
    if U#user.username == [] orelse U#user.sid ->
            throw({error, "incomplete user"});
       true ->
            U#user{data=Data}
    end;
object2user(U, [{"username", Username}|Props], Data) ->
    object2user(U#user{username=Username}, Props, Data);
object2user(U, [{"password", Password}|Props], Data) ->
    object2user(U#user{password=Password}, Props, Data);
object2user(U, [{"sid", Sid}|Props], Data) ->
    object2user(U#user{sid=Sid}, Props, Data);
object2user(U, [{"confirmed", Confirmed}|Props], Data) ->
    object2user(U#user{confirmed=Confirmed}, Props, Data);
object2user(U, [{"role", Role}|Props], Data) ->
    object2user(U#user{role=?l2a(Role)}, Props, Data);
object2user(U, [{"passwd_reset_id", PRI}|Props], Data) ->
    object2user(U#user{passwd_reset_id=PRI}, Props, Data);
object2user(U, [{"passwd_reset_send_time", PRST}|Props], Data) ->
    object2user(U#user{passwd_reset_send_time=PRST}, Props, Data);
object2user(U, [D={Key,_Value}|Props], Data)
  when is_list(Key) ->
    object2user(U, Props, [D|Data]);
object2user(U, [Unknown|Props], Data) ->
    error_logger:format("unknown user property ~p", [Unknown]),
    object2user(U, Props, Data).


apply_user_ops(U, []) ->
    U;
apply_user_ops(U, [{"sid", _}|Ops]) ->
    apply_user_ops(U, Ops);
apply_user_ops(U, [{"password", _}|Ops]) ->
    apply_user_ops(U, Ops);
apply_user_ops(U, [{"username", _}|Ops]) ->
    apply_user_ops(U, Ops);
apply_user_ops(U, [{"passwd_reset_id", _}|Ops]) ->
    apply_user_ops(U, Ops);
apply_user_ops(U, [{"passwd_reset_send_time", _}|Ops]) ->
    apply_user_ops(U, Ops);
apply_user_ops(U, [{"role", NewRole}|Ops]) ->
    if U#user.role == admin ->
            apply_user_ops(U#user{role=?a2l(NewRole)}, Ops);
       true ->
            apply_user_ops(U, Ops)
    end;
apply_user_ops(U, [{Key,Value}|Ops]) ->
    Data = lists:keydelete(Key, 1, U#user.data),
    apply_user_ops(U#user{data=[{Key, Value}|Data]}, Ops);
apply_user_ops(U, [_|Ops]) ->
    apply_user_ops(U, Ops).

apply_user_json(U, {struct, Ops}) ->
    apply_user_ops(U, Ops).

merge_attrs(L, {struct, Ops}) ->
    L ++ Ops;
merge_attrs(L, _Json) ->
    L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_events() ->
    {ok, B} = file:read_file(?EVENTS_DB),
    {ok, {array, Events}} = json2:decode_string(?b2l(B)),
    [object2event(E) || E <- Events].

store_events(Events) ->
    EventStructs = [event2object(E) || E <- Events],
    String = idrott_json2:encode_pretty({array, EventStructs}),
    file:write_file(?EVENTS_DB_TMP, String),
    file:rename(?EVENTS_DB_TMP, ?EVENTS_DB).

event2object(E) ->
    {struct,
     [{"id", E#event.id},
      {"name", E#event.name},
      {"date", E#event.date}|
      E#event.data]}.

object2event({struct, Props}) ->
    object2event(#event{}, Props, _Data=[]).

object2event(E, [], Data) ->
    if E#event.id == undefined ->
            throw({error, "incomplete event"});
       true ->
            E#event{data=Data}
    end;
object2event(E, [{"name", Name}|Props], Data) ->
    object2event(E#event{name=Name}, Props, Data);
object2event(E, [{"date", Date}|Props], Data) ->
    object2event(E#event{date=Date}, Props, Data);
object2event(E, [{"id", Id}|Props], Data) ->
    object2event(E#event{id=Id}, Props, Data);
object2event(E, [D={Key,_Value}|Props], Data)
  when is_list(Key) ->
    object2event(E, Props, [D|Data]);
object2event(E, [Unknown|Props], Data) ->
    error_logger:format("unknown user property ~p", [Unknown]),
    object2event(E, Props, Data).


apply_event_ops(E, []) ->
    E;
apply_event_ops(E, [{"name", Name}|Ops]) ->
    apply_event_ops(E#event{name=Name}, Ops);
apply_event_ops(E, [{"id", _Id}|Ops]) ->
    %% ignore
    apply_event_ops(E, Ops);
apply_event_ops(E, [{"date", Date}|Ops]) ->
    apply_event_ops(E#event{date=Date}, Ops);
apply_event_ops(E, [{Key,Value}|Ops]) ->
    Data = lists:keydelete(Key, 1, E#event.data),
    apply_event_ops(E#event{data=[{Key, Value}|Data]}, Ops);
apply_event_ops(E, [_|Ops]) ->
    apply_event_ops(E, Ops).

apply_event_json(E, {struct, Ops}) ->
    apply_event_ops(E, Ops).

%%

get_user_by_id(User, S) ->
    case lists:keysearch(User, #user.sid, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

get_user_by_name(User, S) ->
    case lists:keysearch(User, #user.username, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

get_user_by_select({struct, Opts}, S) ->
    filter_users(S#state.users, Opts, _Acc=[]).

filter_users([], _Opts, Acc) ->
    Acc;
filter_users([U|Us], Opts, Acc) ->
    case has_opts(Opts, U) of
        true ->
            filter_users(Us, Opts, [U|Acc]);
        false ->
            filter_users(Us, Opts, Acc)
    end.

has_opts([], _U) ->
    true;
has_opts([{"username", Name}|Opts], U) ->
    if U#user.username == Name ->
            has_opts(Opts, U);
       true ->
            false
    end;
has_opts([{"confirmed", C}|Opts], U) ->
    if U#user.confirmed == C ->
            has_opts(Opts, U);
       true ->
            false
    end;
has_opts([{"role", Role}|Opts], U) ->
    case ?a2l(U#user.role) of
        Role ->
            has_opts(Opts, U);
        _ ->
            false
    end;
has_opts([Opt={Key, MObject}|Opts], U) ->
    case lists:keysearch(Key, 1, U#user.data) of
        {value, {_, UObject}} ->
            case match_object(MObject, UObject) of
                true ->
                    has_opts(Opts, U);
                false ->
                    false
            end;
        _ ->
            case lists:member(Opt, U#user.data) of
                true ->
                    has_opts(Opts, U);
                false ->
                    false
            end
    end;
has_opts([Opt|Opts], U) ->
    case lists:member(Opt, U#user.data) of
        true ->
            has_opts(Opts, U);
        false ->
            false
    end.

match_object(X, X) ->
    true;
match_object({array, MArray}, {array, Array}) ->
    match_all(MArray, Array);
match_object({struct, MStruct}, {struct, Struct}) ->
    match_struct(MStruct, Struct);
match_object(_, _) ->
    false.

match_all([], _Array) ->
    true;
match_all([M|Ms], Array) ->
    case [true || O <- Array,
                  match_object(M, O)] of
        [] ->
            false;
        _ ->
            match_all(Ms, Array)
    end.

match_struct([], _Struct) ->
    true;
match_struct([{K,V}|Ms], Struct) ->
    case lists:keysearch(K, 1, Struct) of
        {value, {_, S}} ->
            case match_object(V, S) of
                true ->
                    match_struct(Ms, Struct);
                false ->
                    false
            end;
        _ ->
            false
    end.

get_user_by_rpid(PRID, S) ->
    case lists:keysearch(PRID, #user.passwd_reset_id, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

update_user(User, Users) ->
    NewUsers = lists:keyreplace(User#user.username,
                                #user.username, Users, User),
    store_users(NewUsers),
    NewUsers.

get_event_by_id(Id, S) ->
    case lists:keysearch(Id, #event.id, S#state.events) of
        {value, E=#event{}} ->
            E;
        false ->
            false
    end.

get_event_by_select({struct, Opts}, S) ->
    filter_events(S#state.events, Opts, _Acc=[]).

filter_events([], _Opts, Acc) ->
    Acc;
filter_events([E|Es], Opts, Acc) ->
    case event_has_opts(Opts, E) of
        true ->
            filter_events(Es, Opts, [E|Acc]);
        false ->
            filter_events(Es, Opts, Acc)
    end.

event_has_opts([], _E) ->
    true;
event_has_opts([{"name", Name}|Opts], E) ->
    if E#event.name == Name ->
            event_has_opts(Opts, E);
       true ->
            false
    end;
event_has_opts([{"date", D}|Opts], E) ->
    if E#event.date == D ->
            event_has_opts(Opts, E);
       true ->
            false
    end;
event_has_opts([Opt|Opts], E) ->
    case lists:member(Opt, E#event.data) of
        true ->
            event_has_opts(Opts, E);
        false ->
            false
    end.

update_event(Event, Events) ->
    NewEvents = lists:keyreplace(Event#event.id, #event.id, Events, Event),
    store_events(NewEvents),
    NewEvents.

%%

mk_id(S) ->
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    Id = ?i2l(N),
    case lists:keysearch(Id, #user.sid, S#state.users) of
        {value, _} ->
            mk_id(S);
        _ ->
            Id
    end.

mk_rpid(S) ->
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    Id = ?i2l(N),
    case lists:keysearch(Id, #user.passwd_reset_id, S#state.users) of
        {value, _} ->
            mk_rpid(S);
        _ ->
            Id
    end.

%% Time management

gnow() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()).

gtostr(Secs) -> gtostr(Secs, date_time).

gtostr(undefined, _) -> "-";
gtostr(Secs, date) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
gtostr(Secs, time) ->
    {_, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
                                [Hour, Minute, Second]));
gtostr(Secs, time24hm) ->
    {_, {Hour, Minute, _Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w", [Hour, Minute]));
gtostr(Secs, date_time) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                                [Year, Month, Day, Hour, Minute, Second]));
gtostr(Secs, date_time_nospace) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w_~2.2.0w:~2.2.0w:~2.2.0w",
                                [Year, Month, Day, Hour, Minute, Second])).


days_diff(A, B) when is_integer(A), is_integer(B) ->
    DiffSecs = A - B,
    DiffSecs div 86400;
days_diff(A, B) when is_tuple(A), is_tuple(B) ->
    days_diff(date2gdate(A), date2gdate(B)).

date2gdate(YMD) ->
    calendar:datetime_to_gregorian_seconds({YMD, {0,0,0}}).

%%

get_val(Key, L, Default) ->
    case lists:keysearch(Key, 1, L) of
        {value, {_, undefined}} -> Default;
        {value, {_, Val}} -> Val;
        _ -> Default
    end.


to_int(Str) ->
    case catch ?l2i(Str) of
        I when is_integer(I) ->
            I;
        _ ->
            -1
    end.

get_event_users(E, S) ->
    F = fun(UId) ->
                case get_user_by_name(UId, S) of
                    false ->
                        false;
                    User ->
                        {true, User}
                end
        end,
    case get_val("funcinfo", E#event.data, []) of
        {array, Users} ->
            lists:zf(F, Users);
        _ ->
            []
    end.

dequote([]) -> [];
dequote([$\\, $t|Rest]) -> [$\t|dequote(Rest)];
dequote([$\\, $n|Rest]) -> [$\t|dequote(Rest)];
dequote([$\\, $r|Rest]) -> [$\t|dequote(Rest)];
dequote([C|Rest]) -> [C|dequote(Rest)].

skip_ws([$\t|Rest]) -> skip_ws(Rest);
skip_ws([$\n|Rest]) -> skip_ws(Rest);
skip_ws([$\r|Rest]) -> skip_ws(Rest);
skip_ws([$ |Rest]) -> skip_ws(Rest);
skip_ws(Rest) -> Rest.

new_event_id(S) ->
    Id = S#state.event_id+1,
    case get_event_by_id(Id, S) of
        false ->
            {S#state{event_id=Id}, Id};
        _ ->
            new_event_id(S#state{event_id=Id})
    end.


mail_users(Users, Subject, Message) ->
    mail_users(Users, Subject, Message, [], []).

mail_users([], _Subject, _Message, SentTo, Rejected) ->
    ?liof("done ~p\n", [{SentTo, Rejected}]),
    {SentTo, Rejected};
mail_users([U|Us], Subject, Message, SentTo, Rejected) ->
    ?liof("Sending to user ~p\n", [get_user_opt("email", U, "no-address")]),
    case catch smtp:send(?MAILSERVER, ?EMAIL_SENDER,
                         [get_user_opt("email", U, "no-address")],
                         Subject, Message, _Attatchment=[]) of
        ok ->
            ?liof("success\n", []),
            mail_users(Us, Subject, Message, [U|SentTo], Rejected);
        _Error ->
            ?liof("failed ~p\n", [_Error]),
            mail_users(Us, Subject, Message, SentTo, [U|Rejected])
    end.

read_data() ->
    case file:read_file(?DATAFILE) of
        {ok, Bin} ->
            binary_to_term(Bin);
        _ ->
            []
    end.

save_data(Data) ->
    file:write_file(?DATAFILE, term_to_binary(Data)).

format_expires_date({{Year, Month, Date}, {Hours, Minutes, Seconds}}) ->
    lists:flatten(
      io_lib:format("~s, ~p ~s ~p ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                    [weekday({Year, Month, Date}), Date,
                     month(Month), Year, Hours, Minutes, Seconds])).

weekday(Date) ->
    int_to_wd(calendar:day_of_the_week(Date)).

int_to_wd(1) ->
    "Mon";
int_to_wd(2) ->
    "Tue";
int_to_wd(3) ->
    "Wed";
int_to_wd(4) ->
    "Thu";
int_to_wd(5) ->
    "Fri";
int_to_wd(6) ->
    "Sat";
int_to_wd(7) ->
    "Sun".

month(1) ->
    "Jan";
month(2) ->
    "Feb";
month(3) ->
    "Mar";
month(4) ->
    "Apr";
month(5) ->
    "May";
month(6) ->
    "Jun";
month(7) ->
    "Jul";
month(8) ->
    "Aug";
month(9) ->
    "Sep";
month(10) ->
    "Oct";
month(11) ->
    "Nov";
month(12) ->
    "Dec".

get_sid(A, L) ->
    case get_val("sid", L, "") of
        "" ->
            get_cookie_val("sid", A);
        Sid ->
            Sid
    end.

get_cookie_val(Cookie, Arg) ->
    H = Arg#arg.headers,
    yaws_api:find_cookie_val(Cookie, H#headers.cookie).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(HELVETICA, "Helvetica").
-define(HELVETICA_BOLD, "Helvetica-Bold").
-define(LANEWIDTH, 60).

gen_pdf(Json) ->
    %% ?liof("Json=~p\n", [Json]),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_author(PDF, "idrott.bevemyr.com"),
    eg_pdf:set_title(PDF, "Schema"),
    eg_pdf:set_subject(PDF,"Schema"),
    eg_pdf:set_keywords(PDF,"Schema"),

    %% grid
    %% grid(PDF),

    body(PDF, Json),

    {Serialized, NoPages} = eg_pdf:export(PDF),
    ?liof("NoPages=~p\n", [NoPages]),
    eg_pdf:delete(PDF),
    Serialized.

body(PDF, {struct, Vals}) ->
    %% Process one day at a time
    Days = get_days(Vals),
    build_days(PDF, Days, Vals).

build_days(_PDF, [], _Vals) ->
    done;
build_days(PDF, [Day|Days], Vals) ->
    DayName0 = get_val("name", Day, ""),
    DayName = unicode:characters_to_list(?l2b(DayName0)),
    header(PDF, Vals, DayName),
    print_day(PDF, Day, Vals),
    if Days =/= [] ->
            eg_pdf:new_page(PDF);
       true ->
            ok
    end,
    build_days(PDF, Days, Vals).

print_day(PDF, Day, Vals) ->
    Arenas = get_arenas(Vals),
    Events = get_events(Vals),
    DXA = get_dxa(Vals),
    AXE = get_axe(Vals),
    DayId = get_val("id", Day, ""),
    [TodayDXA] = [TDXA || TDXA <- DXA,
                          lists:member({"day", DayId}, TDXA)],
    TodaysArenaIds = unpack(get_val("arenas", TodayDXA, "")),
    TodaysArenas = [A || A <- Arenas,
                         lists:member(get_val("id", A, ""), TodaysArenaIds)],
    TodaysEvents = [E || E <- Events,
                         lists:member({"day", DayId}, E)],
    ColumnIds = [get_val("id", A, "") || A <- TodaysArenas],
    StartTime = get_val("starttime", Day, 0),
    ColumnPages = split_columns_on_pages(ColumnIds, 9),
    print_day_pages(PDF, ColumnPages, Arenas, TodaysEvents, StartTime, AXE).

split_columns_on_pages(ColumnIds, Size) ->
    if length(ColumnIds) < Size ->
            [ColumnIds];
       true ->
            Page = lists:sublist(ColumnIds, 1, Size),
            Rest = lists:nthtail(Size, ColumnIds),
            [Page|split_columns_on_pages(Rest, Size)]
    end.

print_day_pages(_PDF, [], _Arenas, _TodaysEvents, _StartTime, _AXE) ->
    ok;
print_day_pages(PDF, [ColumnIds|Pages], Arenas, TodaysEvents, StartTime, AXE) ->
    PageEvents = add_column(TodaysEvents, AXE, ColumnIds, []),
    PageArenas = [A || A <- Arenas,
                       lists:member(get_val("id", A, ""), ColumnIds)],
    box(PDF, {181, 217, 230}, {25, 790}, {?LANEWIDTH*length(ColumnIds), 10}),
    print_events(PDF, PageEvents, StartTime, _PrevColor={0,0,0}),
    arena_header(PDF, PageArenas, 0),
    if length(Pages) > 0 ->
            eg_pdf:new_page(PDF);
       true ->
            ok
    end,
    print_day_pages(PDF, Pages, Arenas, TodaysEvents, StartTime, AXE).

print_events(_PDF, [], _StartTime, _PrevColor) ->
    ok;
print_events(PDF, [Event|Events], StartTime, PrevColor) ->
    Column = get_val("column", Event, 0),
    Duration = get_val("duration", Event, 0),
    Class = unicode(get_val("class", Event, "")),
    Color = get_color(Event, PrevColor),
    Gren = unicode(get_val("gren", Event, "")),
    EventStart = get_val("starttime", Event, 0),
    Time = StartTime+EventStart,
    XPos = 25+Column*?LANEWIDTH,
    YPos = 785-scale(EventStart),
    TimeStr = min_to_time(Time),
    box(PDF, Color, {XPos, YPos-scale(Duration)},
        {?LANEWIDTH, scale(Duration)}),
    eg_pdf:set_line_width(PDF, 0.5),
    box(PDF, {XPos, YPos-scale(Duration)},
        {?LANEWIDTH, scale(Duration)}),
    eg_pdf:set_line_width(PDF, 1),
    if Duration < 7 ->
             Text = lists:flatten([TimeStr," (", ?i2l(Duration), " m) ",
                                  Class," ",Gren]),
            FontSize = 5;
       true ->
            Text = lists:flatten([TimeStr," ", Class," ",Gren]),
            FontSize = 7
    end,
    eg_pdf:set_font(PDF, ?HELVETICA, FontSize),
    eg_pdf:moveAndShow(PDF, XPos+2, YPos-FontSize, Text),
    if Duration > 10 ->
            DurText = lists:flatten(["(",?i2l(Duration)," min)"]),
            eg_pdf:set_font(PDF, ?HELVETICA, FontSize),
            eg_pdf:moveAndShow(PDF, XPos+2, YPos-2*FontSize, DurText);
       true ->
            ok
    end,
    print_events(PDF, Events, StartTime, Color).

add_column([], _AXE, _ColumnIds, Acc) ->
    lists:reverse(Acc);
add_column([Event|Events], AXE, ColumnIds, Acc) ->
    EventId = get_val("id", Event, ""),
    [ArenaId|_] = [get_val("arena", A, "") ||
                      A <- AXE,
                      lists:member(EventId, get_val("events", A, ""))],
    case index_of(ArenaId, ColumnIds, 0) of
        -1 ->
            add_column(Events, AXE, ColumnIds, Acc);
        Index ->
            add_column(Events, AXE, ColumnIds, [[{"column", Index}|Event]|Acc])
    end.

index_of(_Id, [], _N) ->
    -1;
index_of(Id, [Id|_Ids], N) ->
    N;
index_of(Id, [_Id|Ids], N) ->
    index_of(Id, Ids, N+1).

arena_header(PDF, [], Nr) ->
    box(PDF, {25,25}, {Nr*?LANEWIDTH, 760});
arena_header(PDF, [Arena|Arenas], Nr) ->
    Name0 = get_val("name", Arena, ""),
    Name = unicode:characters_to_list(?l2b(Name0)),
    XPos = 25+Nr*?LANEWIDTH,
    eg_pdf:set_font(PDF, ?HELVETICA_BOLD, 8),
    eg_pdf:moveAndShow(PDF, XPos, 792, Name),
    eg_pdf:line(PDF, XPos+?LANEWIDTH, 25, XPos+?LANEWIDTH, 785),
    arena_header(PDF, Arenas, Nr+1).

header(PDF, Vals, DayName) ->
    %% Header: event name and version
    Name = get_val("name", Vals, ""),
    Version = get_val("version", Vals, ""),
    eg_pdf:set_font(PDF, ?HELVETICA_BOLD, 14),
    eg_pdf:moveAndShow(PDF, 25, 814, Name++" "++DayName),
    eg_pdf:set_font(PDF, ?HELVETICA, 10),
    VsnTxt = "Version: "++Version,
    VsnWidth = eg_pdf:get_string_width(PDF, ?HELVETICA, 10, VsnTxt),
    eg_pdf:moveAndShow(PDF, 570-VsnWidth, 814, VsnTxt),
    eg_pdf:set_stroke_color(PDF, {51,102,153}),
    eg_pdf:set_line_width(PDF, 1.5),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:line(PDF, 25, 810, 570, 810),
    eg_pdf:set_line_width(PDF, 1).

box(PDF, Color, Pos, WH) ->
    eg_pdf:set_fill_color(PDF, Color),
    eg_pdf:rectangle(PDF, Pos, WH, fill),
    eg_pdf:set_fill_color(PDF, black).

box(PDF, Pos, WH) ->
    eg_pdf:rectangle(PDF, Pos, WH, stroke).

grid(PDF) ->
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_gray(PDF, 0.75),
    eg_pdf:set_stroke_gray(PDF, 0.75),
    eg_pdf:show_grid(PDF, a4),
    eg_pdf:restore_state(PDF).

get_days(Vals) ->
    {array, Days} = get_val("days", Vals, ""),
    [DVals || {struct, DVals} <- Days].

get_arenas(Vals) ->
    {array, Arenas} = get_val("arenas", Vals, ""),
    [AVals || {struct, AVals} <- Arenas].

get_events(Vals) ->
    {array, Events} = get_val("events", Vals, ""),
    [EVals || {struct, EVals} <- Events].

get_dxa(Vals) ->
    {array, DXA} = get_val("dxa", Vals, ""),
    [unpack(DVals) || {struct, DVals} <- DXA].

get_axe(Vals) ->
    {array, AXE} = get_val("axe", Vals, ""),
    [unpack(DVals) || {struct, DVals} <- AXE].

unpack(Json) when is_list(Json) ->
    unpack(Json, []);
unpack({array, Elems}) ->
    unpack(Elems, []);
unpack({struct, Attr}) ->
    unpack(Attr, []);
unpack({Tag, Data}) ->
    {Tag, unpack(Data)};
unpack(Other) ->
    Other.

unpack([], Acc) ->
    lists:reverse(Acc);
unpack([J|Rest], Acc) ->
    unpack(Rest, [unpack(J)|Acc]).

unicode(Str) ->
    unicode:characters_to_list(?l2b(Str)).

min_to_time(Minutes) ->
    Hours = Minutes div 60,
    Mins = Minutes rem 60,
    lists:flatten(
      io_lib:format("~2.2.0w:~2.2.0w", [Hours, Mins])).

scale(D) ->
    round(D * 1.5).

get_color(Event, PrevColor) ->
    case get_val("color", Event, "") of
        "" ->
            pick_color(PrevColor);
        Color ->
            color_to_rgb(Color)
    end.

pick_color(PrevColor) ->
    Palette = colors(),
    case lists:nth(random:uniform(length(Palette)), Palette) of
        PrevColor ->
            pick_color(PrevColor);
        Color ->
            Color
    end.

colors() ->
    [{215,223,1},
     {255,191,0},
     {1,223,215},
     {247,254,46},
     {211,211,211}].

color_to_rgb([$#,R1,R2,G1,G2,B1,B2]) ->
    {list_to_integer([R1,R2], 16),
     list_to_integer([G1,G2], 16),
     list_to_integer([B1,B2], 16)}.
