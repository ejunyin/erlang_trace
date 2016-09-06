-module(ets_io_server).

-export([start/0, start_link/0, init/0, loop/1, stop/1]).

-define(MAX_INDEX, 100).
-define(MAX_TABLE, 4).
-define(TABLE_NAME, log_buffer).

-record(state, {
          table,
          index,  %% the index in table
          table_p_index, %% store current table index that is used to record log
          table_c_index, %% store current table index that is ready to send log
	  mode, % binary | list
          handler,
          plc,
          sending
	 }).

start_link() ->
    Pid = spawn_link(?MODULE,init,[]),
    {ok, Pid}.

start() ->
    Pid = spawn(?MODULE,init,[]),
    {ok, Pid}.

stop(Pid) -> 
    Pid!stop.

init() ->
    {ok, Handler} = sender:start(),
    [ets:new(get_table_name(Index),[public, named_table, ordered_set]) || Index <- lists:seq(1, ?MAX_TABLE)],
    Table = get_table_name(1),
    ?MODULE:loop(#state{table=Table, index=0, table_p_index=1, table_c_index=1, mode=list, handler=Handler, plc=false, sending=false}).

loop(#state{index=?MAX_INDEX, table_p_index=TPI, table_c_index=TCI, handler=Handler}=State) ->
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
	stop ->
            stop;
	{sent_done, Table} ->
            ets:delete_all_objects(Table),
            NewTable=get_next_table(TCI),
            NewTCI=get_next_table_index(TCI),
            Plc = is_plc_overload(TPI, NewTCI),
            Result = send_table_data(NewTable, Handler),
            case Result of
                paused ->
                    NewState=State#state{table_c_index=NewTCI,plc=Plc, sending=false};
                _ ->
                    NewState=State#state{table_c_index=NewTCI,plc=Plc}
            end,
            ?MODULE:loop(NewState);
        {stop_done} ->	
             [ets:delete(get_table_name(Index)) || Index <- lists:seq(1, ?MAX_TABLE)],
             sender:stop(Handler),
             done;
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
request({put_chars,Chars}, State) ->
    request({put_chars,latin1,Chars}, State);
request({put_chars,M,F,As}, State) ->
    request({put_chars,latin1,M,F,As}, State);
request(_Other, State) ->
    {error, {error, request}, State}.

put_chars(Chars, #state{index=?MAX_INDEX, table_p_index=TPI, table_c_index=TCI, handler=Handler, sending=Sending}=State) ->
    Table = get_next_table(TPI),
    NewTPI = get_next_table_index(TPI),
    Plc = is_plc_overload(NewTPI, TCI),
    case Sending of
        true ->
            NewState = State#state{index=0,table=Table,table_p_index=NewTPI,plc=Plc},
            skip;
        false ->
            NewState = State#state{index=0,table=Table,table_p_index=NewTPI,plc=Plc, sending=true},
	    send_table_data(get_table_name(TCI), Handler)
    end,
    put_chars(Chars, NewState);
put_chars(Chars, #state{table=T, index=I}=State) ->
    ets:insert(T, {I, Chars}),
    {ok, ok, State#state{index=I+1}}.

get_table_name(Index) ->
    list_to_atom(atom_to_list(?TABLE_NAME) ++ integer_to_list(Index)).

get_next_table_index(I) when I<?MAX_TABLE ->
    I+1;
get_next_table_index(_I) ->
    1.

get_next_table(I) when I<?MAX_TABLE ->
	get_table_name(I+1);
get_next_table(_I) ->
    get_table_name(1).

get_free(T1, T2) when T1 > T2 ->
    ?MAX_TABLE - (T1 - T2) - 1;
get_free(T1, T2)->
    T2 - T1 - 1.

is_plc_overload(T1, T2) ->
    D = get_free(T1, T2),
    case D of
        0 -> stop;
        1 -> yes;
        _ -> no
    end.

send_table_data(Table, Handler) ->
    TabSize = ets:info(Table, size),
    case TabSize of
        ?MAX_INDEX ->
            Handler!{start_send, Table};
        _ ->	
            paused
    end.
