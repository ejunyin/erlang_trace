-module(ets_io_server).

-export([start_link/0, init/0, loop/1, until_newline/3, until_enough/3]).

-define(CHARS_PER_REC, 10).

-record(state, {
	  table,
	  position, % absolute
	  mode % binary | list
	 }).

start_link() ->
    spawn_link(?MODULE,init,[]).

init() ->
    Table = ets:new(noname,[ordered_set]),
    ?MODULE:loop(#state{table = Table, position = 0, mode=list}).

loop(State) ->
    receive
	{io_request, From, ReplyAs, Request} ->
	    case request(Request,State) of
		{Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
		    reply(From, ReplyAs, Reply),
		    ?MODULE:loop(NewState);
		{stop, Reply, _NewState} ->
		    reply(From, ReplyAs, Reply),
		    exit(Reply)
	    end;
	%% Private message
	{From, rewind} ->
	    From ! {self(), ok},
	    ?MODULE:loop(State#state{position = 0});
	ets_list ->
		io:format("ets data contains~n~p~n", [ets:tab2list(State#state.table)]),
		?MODULE:loop(State);
	_Unknown ->
	    ?MODULE:loop(State)
    end.

reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),State);
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
	request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
	_:_ ->
	    {error, {error,Function}, State}
    end;

request({get_until, Encoding, _Prompt, M, F, As}, State) ->
    get_until(Encoding, M, F, As, State);
request({get_chars, Encoding, _Prompt, N}, State) ->
    %% To simplify the code, get_chars is implemented using get_until
    get_until(Encoding, ?MODULE, until_enough, [N], State);
request({get_line, Encoding, _Prompt}, State) ->
    %% To simplify the code, get_line is implemented using get_until
    get_until(Encoding, ?MODULE, until_newline, [$\n], State);

request({get_geometry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) ->
    setopts(Opts, State);
request(getopts, State) ->
    getopts(State);
request({requests, Reqs}, State) ->
     multi_request(Reqs, {ok, ok, State});


request({put_chars,Chars}, State) ->
    request({put_chars,latin1,Chars}, State);
request({put_chars,M,F,As}, State) ->
    request({put_chars,latin1,M,F,As}, State);
request({get_chars,Prompt,N}, State) ->
    request({get_chars,latin1,Prompt,N}, State);
request({get_line,Prompt}, State) ->
    request({get_line,latin1,Prompt}, State);
request({get_until, Prompt,M,F,As}, State) ->
    request({get_until,latin1,Prompt,M,F,As}, State);


request(_Other, State) ->
    {error, {error, request}, State}.


multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

setopts(Opts0,State) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}], 
	       Opts0)),
    case check_valid_opts(Opts) of
	true ->
	        case proplists:get_value(binary, Opts) of
		    true ->
			{ok,ok,State#state{mode=binary}};
		    false ->
			{ok,ok,State#state{mode=binary}};
		    _ ->
			{ok,ok,State}
		end;
	false ->
	    {error,{error,enotsup},State}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

getopts(#state{mode=M} = S) ->
    {ok,[{binary, case M of
		      binary ->
			  true;
		      _ ->
			  false
		  end}],S}.


put_chars(Chars, #state{table = T, position = P} = State) ->
    R = P div ?CHARS_PER_REC,
    C = P rem ?CHARS_PER_REC,
    [ apply_update(T,U) || U <- split_data(Chars, R, C) ],
    {ok, ok, State#state{position = (P + length(Chars))}}.


get_until(Encoding, Mod, Func, As, 
	  #state{position = P, mode = M, table = T} = State) ->
    case get_loop(Mod,Func,As,T,P,[]) of
	{done,Data,_,NewP} when is_binary(Data); is_list(Data) ->
	    if
		M =:= binary -> 
		    {ok, 
		     unicode:characters_to_binary(Data, unicode, Encoding),
		     State#state{position = NewP}};
		true ->
		    case check(Encoding, 
		               unicode:characters_to_list(Data, unicode))
                    of
			{error, _} = E ->
			    {error, E, State};
			List ->
			    {ok, List,
			     State#state{position = NewP}}
		    end
	    end;
	{done,Data,_,NewP} ->
	    {ok, Data, State#state{position = NewP}};
	Error ->
	    {error, Error, State}
    end.

get_loop(M,F,A,T,P,C) ->
    {NewP,L} = get(P,T),
    case catch apply(M,F,[C,L|A]) of
	{done, List, Rest} ->
	    {done, List, [], NewP - length(Rest)};
	{more, NewC} ->
	    get_loop(M,F,A,T,NewP,NewC);
	_ ->
	    {error,F}
    end.

check(unicode, List) ->
    List;
check(latin1, List) ->
    try 
	[ throw(not_unicode) || X <- List,
				X > 255 ],
	List
    catch
	throw:_ ->
	    {error,{cannot_convert, unicode, latin1}}
    end.

until_newline([],eof,_MyStopCharacter) ->
    {done,eof,[]};
until_newline(ThisFar,eof,_MyStopCharacter) ->
    {done,ThisFar,[]};
until_newline(ThisFar,CharList,MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
    of
	{L,[]} ->
            {more,ThisFar++L};
	{L2,[MyStopCharacter|Rest]} ->
	    {done,ThisFar++L2++[MyStopCharacter],Rest}
    end.

until_enough([],eof,_N) ->
    {done,eof,[]};
until_enough(ThisFar,eof,_N) ->
    {done,ThisFar,[]};
until_enough(ThisFar,CharList,N) 
  when length(ThisFar) + length(CharList) >= N ->
    {Res,Rest} = my_split(N,ThisFar ++ CharList, []),
    {done,Res,Rest};
until_enough(ThisFar,CharList,_N) ->
    {more,ThisFar++CharList}. 

get(P,Tab) ->
    R = P div ?CHARS_PER_REC,
    C = P rem ?CHARS_PER_REC,
    case ets:lookup(Tab,R) of
	[] ->
	    {P,eof};
	[{R,List}] ->
	    case my_split(C,List,[]) of
		{_,[]} ->
		    {P+length(List),eof};
		{_,Data} ->
		    {P+length(Data),Data}
	    end
    end.

my_split(0,Left,Acc) ->
    {lists:reverse(Acc),Left};
my_split(_,[],Acc) ->
    {lists:reverse(Acc),[]};
my_split(N,[H|T],Acc) ->
    my_split(N-1,T,[H|Acc]).

split_data([],_,_) ->
    [];
split_data(Chars, Row, Col) ->
    {This,Left} = my_split(?CHARS_PER_REC - Col, Chars, []),
    [ {Row, Col, This} | split_data(Left, Row + 1, 0) ].

apply_update(Table, {Row, Col, List}) ->     
    case ets:lookup(Table,Row) of
	[] ->
	    ets:insert(Table,{Row, lists:duplicate(Col,0) ++ List});
	[{Row, OldData}] ->
	    {Part1,_} = my_split(Col,OldData,[]),
	    {_,Part2} = my_split(Col+length(List),OldData,[]),
	    ets:insert(Table,{Row, Part1 ++ List ++ Part2})
    end.


