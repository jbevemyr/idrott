%%% File    : ktime.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : 
%%% Created : 18 Dec 2004 by  <klacke@hyber.org>


%% @doc All times in kred must be specified with this module
%% this includes everything except regular receive/gen_server
%% timeouts. The reason for this is that we must be able to  fake time
%% For example speedstep time so that we simulate that .. say 30 days
%% have passed

-module(ktime).
-export([now_plus/1,
	 days_diff/2,
	 week_day/1,
	 yesterday/0,
	 plus/2,
	 gnow/0,
         unow/0,
	 snow/0,
	 gdate/0,
	 gsecs2gdate/1, gdate2date/1, date2gdate/1,
         gsecs2time/1,
	 gtostr/1,
	 gtostr/2,
	 strtog/1,
	 pno_age/1,
	 day_of_week/1,
	 add_days_to_date/2,
	 add_months_to_date/2,
	 add_months_to_gdate/2,
	 strtodate/1,
	 datetostr/1,
	 daily_send_at/3,
	 date_time_to_str/1,
	 now2gnow/1,
	 univ2local/1
	]).

%% @doc This function returns the time as gregorian_secs with
%% the Arg added to it. Arg is
%% {plus|minus, {Int, seconds|minutes|hours|days}} 
%% Returns  greg_secs

now_plus(Arg) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    plus(Now, Arg).

days_diff(A, B) when is_integer(A), is_integer(B) ->
    DiffSecs = A - B,
    DiffSecs div 86400;
days_diff(A, B) when is_tuple(A), is_tuple(B) ->
    days_diff(date2gdate(A), date2gdate(B)).
    

week_day(Secs) ->
    Gdate = gsecs2gdate(Secs),
    calendar:day_of_the_week(gdate2date(Gdate)).

plus(Time, Arg) ->
    Add = case element(2, Arg) of
	      {Int, seconds} ->
		  Int;
	      {Int, minutes} ->
		  60 * Int;
	      {Int, hours} ->
		  60 * 60  * Int;
	      {Int, days} ->
		  60 * 60  * 24 * Int
	  end,
    case element(1, Arg) of
	plus ->  Time + Add;
	minus -> Time - Add
    end.

%% @doc Return gregorian seconds as of now()

gnow() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%% Get real time, not internal erlang time. Needed when the clock is
%% set.
snow() -> univ2local(now2gnow(syst:now())).

unow() -> now2gnow(syst:now()).

gdate() -> calendar:datetime_to_gregorian_seconds({date(), {0,0,0}}).

now2gnow({MSecs, Secs, _}) ->
    MSecs * 1000000 + Secs + 62167219200. %% 62167219200 == 1/1 1970 00:00:00

univ2local(Secs) ->
    DTime = calendar:gregorian_seconds_to_datetime(Secs),
    LDtime = calendar:universal_time_to_local_time(DTime),
    calendar:datetime_to_gregorian_seconds(LDtime).

gsecs2gdate(Secs) ->
    date2gdate(gdate2date(Secs)).

gdate2date(Secs) ->
    {YMD, _} = calendar:gregorian_seconds_to_datetime(Secs),
    YMD.

gsecs2time(Secs) ->
    {_, Time} = calendar:gregorian_seconds_to_datetime(Secs),
    Time.

date2gdate(YMD) ->
    calendar:datetime_to_gregorian_seconds({YMD, {0,0,0}}).

%% @doc Returns standard format string

gtostr(Secs) -> gtostr(Secs, date_time).

gtostr(undefined, _) -> "-";
gtostr(Secs, date) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
gtostr(Secs, xdate) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w", [Year, Month, Day]));
gtostr(Secs, time) ->
    {_, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				[Hour, Minute, Second]));
gtostr(Secs, time12) ->
    {_, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Secs),
    Hour12 = if Hour > 12 -> Hour - 12;
		true -> Hour
	     end,
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				[Hour12, Minute, Second]));
gtostr(Secs, time12ampm) ->
    {_, {Hour, Minute, _Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
    {Hour12, AmPm} = if Hour > 12 -> {Hour - 12, 'PM'};
			true -> {Hour, 'AM'}
		     end,
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w ~s",
				[Hour12, Minute, AmPm]));
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

%% @doc Returns gregorian seconds

strtog(Date) ->
    case catch io_lib:fread("~4d-~2d-~2d", Date) of
	{ok, [Year, Month, Day], []} ->
	    DateTime = {{Year, Month, Day}, {0, 0, 0}},
	    case catch calendar:datetime_to_gregorian_seconds(DateTime) of
		{'EXIT', _} -> error;
		Secs ->	{ok, Secs}
	    end;
	_ ->
	    case catch io_lib:fread("~4d-~2d-~2d ~d:~d:~d", Date) of
		{ok, [Year, Month, Day, Hours,Minutes,Seconds], []} ->
		    DateTime = {{Year, Month, Day}, {Hours, Minutes, Seconds}},
		    case catch calendar:datetime_to_gregorian_seconds(
				 DateTime) of
			{'EXIT', _} -> error;
			Secs ->	{ok, Secs}
		    end;
		_ ->
		    error
	    end
    end.

yesterday() ->
    plus(gdate(), {minus, {1, days}}).


%% return age in years of a pno or atom invalid
pno_age(Pno) ->
    Date = {Pno div 100000000 + 1900,
	    Pno div 1000000 rem 100,
	    Pno div 10000 rem 100},
    Time = {0, 0, 0},
    Now = ktime:gnow(),
    case catch calendar:datetime_to_gregorian_seconds({Date, Time}) of
	{'EXIT', _} ->
	    invalid;
	GDate ->
	    Age = Now-GDate,
	    if
		Age > 86400*365*120 -> invalid;
		Age < 0 -> invalid;
		true -> Age div (86400*365)
	    end
    end.

day_of_week(GDate) ->
    calendar:day_of_the_week(gdate2date(GDate)).



add_months_to_gdate({Date, {0,0,0}}, Months) ->
    {add_months_to_date(Date, Months), {0,0,0}}.

add_months_to_date({Y,M,D}, Months) ->
    if
	M + Months > 12 ->
	    add_months_to_date({Y+1,1,D}, Months - (12 - M) -1);
	true ->
	    {Y,M+Months,D}
    end.
	    

add_days_to_date({Y,M,D}, Days) ->
    Mdays = days_in_month(M),
    if 
	(D + Days) > Mdays, M == 12 ->
	    add_days_to_date({Y+1,1, 1}, Days - (Mdays-D) -1);
	(D + Days) > Mdays ->
	    add_days_to_date({Y,M+1, 1}, Days - (Mdays-D) -1);
       true ->
	    {Y,M,D+Days}
    end.
	    
days_in_month(1) ->
    31;
days_in_month(2) ->
    28;
days_in_month(3) ->
    31;
days_in_month(4) ->
    30;
days_in_month(5) ->
    31;
days_in_month(6) ->
    30;
days_in_month(7) ->
    31;
days_in_month(8) ->
    31;
days_in_month(9) ->
    30;
days_in_month(10) ->
    31;
days_in_month(11) ->
    30;
days_in_month(12) ->
    31.

strtodate(S) ->
    [Y,M,D] = lists:map(fun(X) -> list_to_integer(X) end, 
			string:tokens(S, "-:")),
    true = calendar:valid_date(Ret = {Y,M,D}),
    Ret.


datetostr({Y,M,D}) ->
    io_lib:format("~w-~w-~w", [Y,M,D]).


hms({H,M,S}) ->
    timer:hms(H,M,S).


%% every day send message to a Pid a particular time.
daily_send_at(Time, PidOrName, Msg) ->
    Now = time(),
    Sleep = if Now > Time ->
		    %% send tomorrow
		    TomorrowMillis = hms(Time),
		    TodayMillis = (3600 * 24 * 1000) - hms(Now),
		    TomorrowMillis + TodayMillis;
	       true ->
		    hms(Time) - hms(Now)
	    end,
    spawn(fun() -> daily_send_at_loop(Sleep, PidOrName, Msg) end).

daily_send_at_loop(S, PidOrName, Msg) ->
    Oneday = 3600 * 24 * 1000,
    receive 
    after abs(S) ->
	    catch PidOrName ! Msg
    end,
    daily_send_at_loop(Oneday, PidOrName, Msg).
    
date_time_to_str({Date, Time}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = {Date, Time},
     lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
				 [Year, Month, Day, Hour, Minute, Second])).
