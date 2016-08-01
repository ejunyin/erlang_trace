%% @author ejunyin
%% @doc @todo Add description to logger_server.


-module(logger_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {listen_pid = undefined, listen_socket = undefined, ets = undefined}).

-define(LISTEN_PORT, 12345).
-define(LOGGER_FOLDER, ".").
-define(LOGGER_NAME, "erlang_trace").

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	process_flag(trap_exit, true), 
	rotating_logger:start_link(),
	rotating_logger:add_handler([{name,?LOGGER_NAME},{dir,?LOGGER_FOLDER}]),
	{ok, Listen} = gen_tcp:listen(?LISTEN_PORT, [binary, {active, false}, {reuseaddr, true}]),
	From = self(),
	ListenPid = spawn_link(fun() -> loop_listen(Listen, From) end),
	Ets = ets:new(privateTable, [set, private]),
    {ok, #state{listen_pid=ListenPid, listen_socket=Listen, ets=Ets}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(stop, _From, State) ->
	terminate(stop, State),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({LPid, client_accept, LSocket, Socket}, #state{listen_pid=LPid, listen_socket=LSocket, ets=Ets} = State) ->
	{ok, Info} = inet:peername(Socket),
	Pid = spawn_handle_log_process(Socket),
	io:format("A new Pid ~p is created to handle incoming connection.~n", [Pid]),
	gen_tcp:controlling_process(Socket, Pid),
	inet:setopts(Socket, [{active, true}]),
	ets:insert(Ets, {Pid, LSocket, Info}),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{listen_pid=ListenPid, listen_socket=ListenSocket} = State) ->
    if is_port(ListenSocket) ->
        gen_tcp:close(ListenSocket);
    true ->
        ok
    end,
    stop_all_loop_handlers(State),
    if is_pid(ListenPid) ->
        exit(ListenPid, normal);
    true ->
        ok
    end,
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

loop_listen(ListenSocket, From) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			gen_tcp:controlling_process(Socket, From),
			From ! {self(), client_accept, ListenSocket, Socket},
			loop_listen(ListenSocket, From);
		{error, Error} ->
			exit(Error)
	end.

spawn_handle_log_process(Socket) ->
    _From = self(),
    {ok, {_PeerAddress, _}} = inet:peername(Socket),
    spawn_link(fun() -> loop_handler(Socket) end).

loop_handler(Socket) ->
    receive
		{tcp, Socket, Bin} ->
            rotating_logger:log(log, Bin),
            loop_handler(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket be closed~n");
		stop ->
			io:format("Stop socket ~p~n", [Socket]),
			gen_tcp:close(Socket),
            done;
		Error ->
			exit(Error)
	end.

stop_all_loop_handlers(#state{ets=Ets}) ->
    All = ets:tab2list(Ets),
    Fun = fun({Pid, _, _}) ->
				  Pid ! stop,
                  ets:delete(Ets, Pid)
          end,
    [Fun(X) || X <- All].