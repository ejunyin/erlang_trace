%% @author ejunyin
%% @doc @todo Add description to sender.


-module(logger_sender).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start_link/0, init/0, loop/1, stop/1]).

-define(PORT_NUM, 12345).

-record(state, {
    socket,
    producer
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
    {ok, Socket} = gen_tcp:connect("localhost", ?PORT_NUM, [binary], 1000),
    ?MODULE:loop(#state{socket=Socket, producer=undefined}).

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
loop(#state{socket=Socket, producer=Producer}=State) ->
    receive
    {set_collector, Pid} ->
        NewState=State#state{producer=Pid},
        ?MODULE:loop(NewState);
    {start_send, Table} ->
        start_send(Table, Socket, Producer),
        ?MODULE:loop(State);
    {send_and_stop, Table} ->
        send_and_stop(Table, Socket, Producer),
        ?MODULE:loop(State);		
    stop ->
        gen_tcp:close(Socket),
        done;
    _Unknown ->
        ?MODULE:loop(State)
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================

start_send(Table, Socket, Producer) ->
    send_table(Table, Socket),
    Producer!{sent_done, Table}.

send_and_stop(Table, Socket, Producer) ->
    send_table(Table, Socket),
    Producer!stop_done.

send_table(Table, Socket)->
    [
        try
            gen_tcp:send(Socket, Content)
        catch
        _:_ ->
            error_happen
        end
    || {_Key, Content} <- ets:tab2list(Table)],
    ok.