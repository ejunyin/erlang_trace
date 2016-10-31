%% @author ejunyin
%% @doc @todo Add description to sender.


-module(logger_sender).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/1, init/1, loop/1, stop/1]).

-export([send_table_with_compress/2, send_table/2]).

-define(PORT_NUM, 12345).

-record(state, {
    socket,
    producer
 }).

start_link(Args) ->
    Pid = spawn_link(?MODULE,init,[Args]),
    {ok, Pid}.

start(Args) ->
    Pid = spawn(?MODULE,init,[Args]),
    {ok, Pid}.

stop(Pid) ->
    Pid!stop.

init(Args) ->
	Ip = Args,
	io:format("Ip is ~p~n", [Ip]),
    {ok, Socket} = gen_tcp:connect(Ip, ?PORT_NUM, [binary], 1000),
    ?MODULE:loop(#state{socket=Socket}).

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
        %io:format("[sender] receive start_send  ~p~n", [State]),
        start_send(Table, Socket, Producer),
        ?MODULE:loop(State);
    {send_and_stop, Table} ->
        %io:format("[sender] receive send_and_stop  ~p~n", [State]),
        send_and_stop(Table, Socket, Producer),
        ?MODULE:loop(State);		
    stop ->
        %io:format("[sender] receive stop  ~p~n", [State]),
        gen_tcp:close(Socket),
        done;
    _Unknown ->
        ?MODULE:loop(State)
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================

start_send(Table, Socket, Producer) ->
    %send_table(Table, Socket),
    send_table_with_compress(Table, Socket),
    Producer!{sent_done, Table}.

send_and_stop(Table, Socket, Producer) ->
    %send_table(Table, Socket),
    send_table_with_compress(Table, Socket),
    Producer!stop_done.

send_table(Table, Socket)->
    [
        try
            %io:format("send data is : ~p~n", [Content]),
            %timer:sleep(100),
			gen_tcp:send(Socket, Content)
        catch
        _:_ ->
            error_happen
        end
    || {_Key, Content} <- ets:tab2list(Table)],
    ok.

send_table_with_compress(Table, Socket) ->
	Objs = ets:tab2list(Table),
	Data = term_to_binary(Objs),
	try
		CData = zlib:zip(Data),
		Len = size(CData),
		Bin1 = term_to_binary(Len),
        Bin = <<Bin1/binary, CData/binary>>,
		io:format("send len is ~p~n", [Len]),
		gen_tcp:send(Socket, Bin)
	catch
	_:_ ->
		error_happen
	end,
    ok.