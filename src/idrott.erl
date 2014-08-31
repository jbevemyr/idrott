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
-export([start/0, stop/0]).

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

-define(EMAIL_SENDER, "info@idrott.se").
-define(MAILSERVER, "mail.bevemyr.com").

-define(USER_DB, "/home/share/jb/work/idrott/user.db.json").
-define(USER_DB_TMP, "/home/share/jb/work/idrott/user.db.json.tmp").

-define(EVENTS_DB, "/home/share/jb/work/idrott/events.db.json").
-define(EVENTS_DB_TMP, "/home/share/jb/work/idrott/events.db.json.tmp").

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
            PostStr = clean_leading_ws(remove_unescape(?b2l(?l2b([Acc|Res])))),
            case json2:decode_string(PostStr) of
                {ok, Json} ->
                    io:format("Json=~p\n", [Json]),
                    L = yaws_api:parse_query(A),
                    do_op(A#arg.appmoddata, L, Json);
                _Reason ->
                    io:format("json=~p\n", [PostStr]),
                    io:format("got error ~p\n", [_Reason]),
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
    case json2:decode_string(remove_unescape(DecodedClidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json);
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
    case json2:decode_string(remove_unescape(Clidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json);
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
    do_op(A#arg.appmoddata, L, []).

do_op(Cmd, L, Json) ->
    Res = gen_server:call(?SERVER, {Cmd, L, Json}, infinity),
    rpcreply(Res).


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
          event_id=0
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).
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
    {ok, #state{users=Users, events=Events}}.

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

handle_call({Cmd, L, Json}, _From, S) ->
    try
        {Res, NewS} = do_cmd(Cmd, L, Json, S),
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

%% http://idrott/idrott/login?user=johan&password=test
do_cmd("login", L, _Json, S) ->
    User = get_val("user", L, ""),
    Password = get_val("password", L, ""),
    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
    case get_user_by_name(User, S) of
        U=#user{password = Md5Pass} ->
            %% login successful
            Res = {struct, [{status, "ok"}, {"sid", U#user.sid},
                            {group, ?a2l(U#user.role)}]};
        #user{} ->
            Res = {struct, [{status, "error"}, {reason, "invalid password"}]};
        _ ->
            Res = {struct, [{status, "error"}, {reason, "unknown user"}]}
    end,
    {Res, S};
%% http://idrott/idrott/send_reset_password?user=johan
do_cmd("send_reset_password", L, _Json, S) ->
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
    {Res, NewS};
%% http://idrott/idrott/reset_password?rpid=1234567890&password=test
do_cmd("reset_password", L, _Json, S) ->
    Rpid = get_val("rpid", L, ""),
    Password = get_val("password", L, ""),
    case get_user_by_rpid(Rpid, S) of
        U=#user{} ->
            Age = days_diff(U#user.passwd_reset_send_time, ktime:gnow()),
            if Age < 5 ->
                    %% login successful
                    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
                    NewU = U#user{password=Md5Pass},
                    Res = {struct, [{status, "ok"}, {user, user2object(U)}]},
                    {Res, S#state{users=update_user(NewU, S#state.users)}};
               true ->
                    Res = {struct, [{status, "error"},
                                    {reason, "password reset link has expired"}]},
                    {Res, S}
            end;
        _ ->
            Res = {struct, [{status, "error"}, {reason, "unknown sid"}]},
            {Res, S}
    end;
%% http://idrott/idrott/set_password?sid=1234567890&old_password=test&new_password=test2
do_cmd("set_password", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
    OldPass = get_val("old_password", L, ""),
    NewPass = get_val("new_password", L, ""),
    Md5OldPass = ?b2l(base64:encode(crypto:hash(md5, OldPass))),
    Md5NewPass = ?b2l(base64:encode(crypto:hash(md5, NewPass))),
    case get_user_by_id(Sid, S) of
        U=#user{} when Md5OldPass == U#user.password ->
            %% successful auth of old pass
            NewU = U#user{password=Md5NewPass},
            Res = {struct, [{status, "ok"}]},
            {Res, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{status, "error"}, {reason, "unknown sid"}]},
            {Res, S}
    end;
%% http://idrott/idrott/set_user_password?sid=1234567890&username=jb&password=test
do_cmd("set_user_password", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
                    {Res, NewS};
                false ->
                    Res = {struct, [{status, "error"},
                                    {reason, "unknown user"}]},
                    {Res, S}
            end;
        #user{} ->
            Res = {struct, [{status, "error"},
                            {reason, "only allowed for admin user"}]},
            {Res, S};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]},
            {Res, S}
    end;
%% http://idrott/idrott/get_all_users?sid=1234567890
do_cmd("get_all_users", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, S};
%% http://idrott/idrott/get_user?sid=1234567890
do_cmd("get_user", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            Res = {struct, [{status, "ok"}, {user, user2object(U)}]};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]}
    end,
    {Res, S};
%% http://idrott/idrott/get_named_user?sid=1234567890&username=jb
do_cmd("get_named_user", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, S};
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("get_selected_users", L, Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, NewS};
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("set_user", L, Json, S) ->
    Sid = get_val("sid", L, ""),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            U2 = apply_user_ops(U,L),
            NewU = apply_user_json(U2, Json),
            Res = {struct, [{status, "ok"}]},
            {Res, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown sid"}]},
            {Res, S}
    end;
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("set_named_user", L, Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, NewS};
%% http://idrott/idrott/add_user?sid=1234567890&username=jb&password=test&role=user&foo=bar
do_cmd("add_user", L0, Json, S) ->
    Sid = get_val("sid", L0, ""),
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
    {Res, NewS};
%% http://idrott/idrott/del_user?sid=1234567890&username=jb
do_cmd("del_user", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, NewS};
%% http://idrott/idrott/get_all_events?sid=1234567890
do_cmd("get_all_events", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, S};
%% http://idrott/idrott/create_event?sid=1234567890&name=foo&date=2014-10-01&pm=foo&timeschedule=foo&location=vallen&funcinfo=foo&funccount=10&funccall=open
do_cmd("create_event", L0, Json, S) ->
    Sid = get_val("sid", L0, ""),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            L = case Json of
                     {struct, Ops} ->  L0 ++ Ops;
                     _ -> L0
                 end,
            %% login successful
            {S2, Id} = new_event_id(S),
            Event = apply_event_ops(#event{id=Id}, L),
            NewS = S2#state{events=[Event|S2#state.events]},
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
    {Res, NewS};
do_cmd("change_event", L0, Json, S) ->
    Sid = get_val("sid", L0, ""),
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
                    Res = {struct, [{status, "ok"}]}
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
    {Res, NewS};
do_cmd("get_event", L, _Json, S) ->
    Sid = get_val("sid", L, ""),
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
                                    {events, event2object(Event)}]}
            end;
        _ ->
            Res = {struct, [{status, "error"},
                            {reason, "unknown event id"}]}
    end,
    {Res, S};
%% http://idrott/idrott/set_user?sid=1234567890&foo=bar
do_cmd("get_selected_events", L, Json, S) ->
    Sid = get_val("sid", L, ""),
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
    {Res, NewS};
do_cmd(Unknown, _L, _Json, S) ->
    Error = lists:flatten(io_lib:format("unknown request: ~p", [Unknown])),
    error_logger:format("~s", [Error]),
    {{struct, [{"status", "error"}, {reason, Error}]}, S}.

rpcreply(Response) ->
    X = json2:encode(Response),
    [{header, {cache_control, "no-cache"}},
     {header, "Expires: -1"},
     {html, X}].

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
     [{"id", ?i2l(E#event.id)},
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
apply_event_ops(E, [{"date", Date}|Ops]) ->
    apply_event_ops(E#event{date=Date}, Ops);
apply_event_ops(E, [{Key,Value}|Ops]) ->
    Data = lists:keydelete(Key, 1, E#event.data),
    apply_event_ops(E#event{data=[{Key, Value}|Data]}, Ops);
apply_event_ops(E, [_|Ops]) ->
    apply_event_ops(E, Ops).

apply_event_json(E, {struct, Ops}) ->
    apply_event_json(E, Ops).

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

remove_unescape([]) -> [];
remove_unescape([$\\, $t|Rest]) -> [$\t|remove_unescape(Rest)];
remove_unescape([$\\, $n|Rest]) -> [$\t|remove_unescape(Rest)];
remove_unescape([$\\, $r|Rest]) -> [$\t|remove_unescape(Rest)];
remove_unescape([C|Rest]) -> [C|remove_unescape(Rest)].

clean_leading_ws([$\t|Rest]) -> clean_leading_ws(Rest);
clean_leading_ws([$\n|Rest]) -> clean_leading_ws(Rest);
clean_leading_ws([$\r|Rest]) -> clean_leading_ws(Rest);
clean_leading_ws([$ |Rest]) -> clean_leading_ws(Rest);
clean_leading_ws(Rest) -> Rest.

new_event_id(S) ->
    Id = S#state.event_id+1,
    case get_event_by_id(S, Id) of
        false ->
            {S#state{event_id=Id}, Id};
        _ ->
            new_event_id(S#state{event_id=Id})
    end.
