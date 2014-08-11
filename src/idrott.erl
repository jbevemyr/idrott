%    -*- Erlang -*- 
%    File:	idrott.erl  (~jb/work/idrott/src/idrott.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Aug  5 14:29:44 2014
%    Purpose:   

-module('idrott').
-author('jb@bevemyr.com').

-compile(export_all).
%%-export([Function/Arity, ...]).

-include("yaws_api.hrl").

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

-define(USER_DB, "user.db.json").
-define(USER_DB_TMP, "user.db.json.tmp").

%%

out(A) ->
    L = case (A#arg.req)#http_request.method of
	    'GET' ->
		yaws_api:parse_query(A);
	    'POST' ->
		yaws_api:parse_post(A)
	end,
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    do_op(A#arg.appmoddata, L).

do_op("login", L) ->
    User = get_val("user", L, ""),
    Password = get_val("password", L, ""),
    Res = gen_server:call(?SERVER, {login, User, Password}, infinity),
    rpcreply(Res);
do_op("send_reset_password", L) ->
    User = get_val("user", L, ""),
    Res = gen_server:call(?SERVER, {send_reset_password, User}, infinity),
    rpcreply(Res);
do_op("list_users", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {list_users, Sid}, infinity),
    rpcreply(Res);
do_op("get_user", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {get_user, Sid, L}, infinity),
    rpcreply(Res);
do_op("set_user", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {set_user, Sid, L}, infinity),
    rpcreply(Res);
do_op("set_password", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {set_password, Sid, L}, infinity),
    rpcreply(Res);
do_op("add_user", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {add_user, Sid, L}, infinity),
    rpcreply(Res);
do_op("del_user", L) ->
    Sid = get_val("sid", L, ""),
    Res = gen_server:call(?SERVER, {del_user, Sid, L}, infinity),
    rpcreply(Res);
do_op(Unknown, _L) ->
    Error = lists:flatten(io_lib:format("unknown request: ~p", [Unknown])),
    error_logger:format("~s", [Error]),
    rpcreply({struct, [{"status", "error"}, {"reason", Error}]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server

-record(user, {
          username = [],          %% string() - username (email addess)
          password = [],          %% string() - password MD5
          name = [],              %% string() - user full name
          address = [],           %% string() - user snail mail address
          sid = [],               %% string() - session id
          confirmed = false,      %% boolean() - confirmed
          role = user,            %% admin | user - privilege group
          passwd_reset_id = [],   %% string() - password reset id
          passwd_reset_send_time=0  %% date_time() - send time time of last
                                  %% password reset email
         }).

-record(state, {
          users=[]            %% list of #user{}
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, monark, [], []).

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
    {ok, #state{users=Users}}.

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
handle_call({login, User, Password}, _From, S) ->
    case get_user_by_id(User, S) of
        U=#user{password = Password} ->
            %% login successful
            Res = {struct, [{status, "ok"}, {"sid", U#user.sid}]};
        #user{} ->
            Res = {struct, [{state, "error"}, {"reason", "invalid password"}]};
        _ ->
            Res = {struct, [{state, "error"}, {"reason", "unknown user"}]}
    end,
    {reply, Res, S};
            
handle_call({send_reset_password, User}, _From, S) ->
    case get_user_by_name(User, S) of
        U=#user{} ->
            RPid = mk_rpid(S),
            smtp:send(?MAILSERVER, ?EMAIL_SENDER, [User],
                      "Password reset request",
                      "You have requested a password reset. Follow the link\n"
                      "below to reset the password. Ignore this email if you haven't\n"
                      "requested a reset.",
                      "http://www.idrott.org/reset_password.yaws?rpid="++RPid, []),
            Res = {struct, [{state, "ok"}]},
            NewUser = U#user{passwd_reset_id=RPid, passwd_reset_send_time=gnow()},
            NewUsers = update_user(NewUser, S#state.users),
            NewS = S#state{users=NewUsers};
        _ ->
            Res = {struct, [{state, "error"}, {"reason", "unknown user"}]},
            NewS = S
    end,
    {reply, Res, NewS};
            
handle_call(_Request, _From, S) ->
    Reply = ok,
    {reply, Reply, S}.

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

rpcreply(Response) ->
    X = json2:encode(Response),
    [{header, {cache_control, "no-cache"}},
     {header, "Expires: -1"},
     {html, X}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_users() ->
    {ok, B} = file:read_file(?USER_DB),
    {array, Users} = json2:decode_string(B),
    [object2user(U) || U <- Users].

store_users(Users) ->
    UserStructs = [user2object(U) || U <- Users],
    String = json2:encode({array, UserStructs}),
    file:write_file(?USER_DB_TMP, String),
    file:rename(?USER_DB_TMP, ?USER_DB).
    
user2object(U) ->
    {struct, 
     [{"username", U#user.username},
      {"password", U#user.password},
      {"name", U#user.name},
      {"address", U#user.address},
      {"sid", U#user.sid},
      {"confirmed", U#user.confirmed},
      {"role", U#user.role},
      {"passwd_reset_id", U#user.passwd_reset_id},
      {"passwd_reset_send_time", U#user.passwd_reset_send_time}]}.

object2user({struct, Props}) ->
    object2user(#user{}, Props).

object2user(U, []) ->
    if U#user.username == [] orelse U#user.sid ->
            throw({error, "incomplete user"});
       true ->
            U
    end;
object2user(U, [{"username", Username}|Props]) ->
    object2user(U#user{username=Username}, Props);
object2user(U, [{"password", Password}|Props]) ->
    object2user(U#user{password=Password}, Props);
object2user(U, [{"name", Name}|Props]) ->
    object2user(U#user{name=Name}, Props);
object2user(U, [{"address", Address}|Props]) ->
    object2user(U#user{address=Address}, Props);
object2user(U, [{"sid", Sid}|Props]) ->
    object2user(U#user{sid=Sid}, Props);
object2user(U, [{"confirmed", Confirmed}|Props]) ->
    object2user(U#user{confirmed=?l2a(Confirmed)}, Props);
object2user(U, [{"role", Role}|Props]) ->
    object2user(U#user{role=?l2a(Role)}, Props);
object2user(U, [{"passwd_reset_id", PRI}|Props]) ->
    object2user(U#user{passwd_reset_id=PRI}, Props);
object2user(U, [{"passwd_reset_send_time", PRST}|Props]) ->
    object2user(U#user{passwd_reset_send_time=PRST}, Props);
object2user(U, [Unknown|Props]) ->
    error_logger:format("unknown user property ~p", [Unknown]),
    object2user(U, Props).

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

get_user_by_prid(PRID, S) ->
    case lists:keysearch(PRID, #user.passwd_reset_id, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

update_user(User, Users) ->
    lists:keyreplace(User#user.username, #user.username, Users, User).

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
