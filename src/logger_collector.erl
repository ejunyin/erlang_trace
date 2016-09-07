-module(logger_collector).

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
    sending,
    stopped %% stop all, game over.
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
    
    [ets:new(get_table_name(Index),[public, named_table, ordered_set]) || Index <- lists:seq(1, ?MAX_TABLE)],
    Table = get_table_name(1),
    ?MODULE:loop(#state{table=Table, index=0, table_p_index=1, table_c_index=1, 
                        mode=list, handler=undefined, plc=no, sending=false, stopped=false}).

%%
%%       logger_collector   logger_sender
%%              |                |
%%              |   start_send   |
%%              |--------------->|
%%              |   send_done    |  
%%              |<---------------|
%%              |                |   
%%              | send_and_stop  |
%%              |--------------->|
%%              |   stop_done    |
%%              |<---------------|
%%
loop(#state{index=?MAX_INDEX, table_p_index=TPI, table_c_index=TCI, handler=Handler, sending=Sending, stopped=Stopped, plc=Plc}=State) ->
    receive
    {io_request, From, ReplyAs, Request} ->
        case Plc of
            stop -> %% don't collect log any more.
                ?MODULE:loop(State); 
            _ -> %% yes or no.
                case request(Request,State) of
                    {Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
                        reply(From, ReplyAs, Reply),
                        ?MODULE:loop(NewState);
                    {stop, Reply, _NewState} ->
                        reply(From, ReplyAs, Reply),
                        exit(Reply)
                end  
        end;
    stop ->
        case Sending of
            false ->
                NewState=State#state{plc=stop},
                Handler ! {send_and_stop, TCI};
            true ->
                NewState=State#state{plc=stop, stopped=true}
        end,
        ?MODULE:loop(NewState);		
    {sent_done, Table} ->
		%% send ok, delete content in this table.
        ets:delete_all_objects(Table),
		%% find the next sending table.
        NewTable=get_next_table(TCI),
        NewTCI=get_next_table_index(TCI),
		%% plc status check
        Plc = is_plc_overload(TPI, NewTCI),
		%% try to send data
        Result = send_table_data(NewTable, Handler),
        case Result of
			%% paused means data is not full. waiting for put_char, so set sending to false.
            paused ->
                case Stopped of
                    %% don't need stop, continue. but it is paused,so set sending to false. 
                    false ->
                        NewState=State#state{table_c_index=NewTCI,plc=Plc, sending=false},
						?MODULE:loop(NewState);
                    true ->
                        Size = ets:info(TCI, size),
                        case Size of
                            0 -> 
                                delete_all_table(),
                                sender:stop(Handler),
                                done;
                            _ ->
                                %% send the last table.
                                Handler ! {send_and_stop, TCI},
                                ?MODULE:loop(State)
                            end 
                    end;    
            _ ->
                %% continue
                NewState=State#state{table_c_index=NewTCI,plc=Plc},
                ?MODULE:loop(NewState)
        end;
    stop_done ->	
        delete_all_table(),
        sender:stop(Handler),
        done;
    {set_handler, Pid} ->
        {ok, Handler} = sender:start(),
        NewState = State#state{handler=Handler},
		Handler!{set_collector, Pid},
        ?MODULE:loop(NewState);
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

%% if we arrive the last position i.e. index=?MAX_INDEX, we need switch the table and select next one.
%% so, table_p_index should be added 1.
%% give a chance to send data to logger server. but firstly, we must check the sending status. only do it when sending = false.
%% if sending = true, it will be triggered automatically.
%% update plc status here.
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
put_chars(Chars, #state{table=T, index=I, plc=Plc}=State) ->
    case Plc of
        no ->
            ets:insert(T, {I, Chars}),
            {ok, ok, State#state{index=I+1}};
        yes ->
            %% check priority
            ets:insert(T, {I, Chars}),
            {ok, ok, State#state{index=I+1}}; 
        stop ->
            {ok, ok, State}
    end.              

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

get_free(T1, T2) when T1 >= T2 ->
    ?MAX_TABLE - (T1 - T2) - 1;
get_free(T1, T2)->
    T2 - T1 - 1.

is_plc_overload(T1, T2) ->
    D = get_free(T1, T2),
    case D of
        0 -> stop; %% don't collect log any more
        1 -> yes;  %% only collect high priority level log
        _ -> %% no problem, go ahead
            case {memory_overload(), cpu_overload()} of
                {no, no} -> no;
                {yes, no} -> yes;
                {no, yes} -> yes;
                {yes, yes} -> yes
            end
    end.

memory_overload() ->
    %% if memory usage exceed 80%, return yes, or return no.
    no.

cpu_overload() ->
    %% if run_queue is more than 1000, return yes, or return no.
    no.

delete_all_table() ->
    [ets:delete(get_table_name(Index)) || Index <- lists:seq(1, ?MAX_TABLE)].

send_table_data(Table, Handler) ->
    TabSize = ets:info(Table, size),
    case TabSize of
        ?MAX_INDEX ->
            Handler!{start_send, Table};
        _ ->	
            paused
    end.