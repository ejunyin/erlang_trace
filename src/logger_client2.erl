%% @author ejunyin
%% @doc @todo Add description to logger_client2.


-module(logger_client2).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, calls/2, calls/3]).

start() ->
	recon_trace:clear(),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

calls({Mod, Fun, Args}, Max) ->
	gen_server:call(?MODULE, {call1, {Mod, Fun, Args}, Max});
calls(TSpecs = [_|_], Max) ->
	gen_server:call(?MODULE, {call2, TSpecs, Max}).

calls({Mod, Fun, Args}, Max, Opts) ->
	gen_server:call(?MODULE, {call3, {Mod, Fun, Args}, Max, Opts});
calls(TSpecs = [_|_], {Max, Time}, Opts) ->
	gen_server:call(?MODULE, {call4, TSpecs, {Max, Time}, Opts});
calls(TSpecs = [_|_], Max, Opts) ->
    gen_server:call(?MODULE, {call5, TSpecs, Max, Opts}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {device = undefined, socket = undefined}).

-define(PORT_NUM, 12345).

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
	{ok, Socket} = gen_tcp:connect("localhost", ?PORT_NUM, [binary], 1000),
	{ok, Device} = tcp_io_server:start(Socket),
    {ok, #state{socket=Socket, device=Device}}.


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
handle_call(stopped, _From, State) ->
	io:format("logger server connection lost!~n"),
	terminate(stop, State),
    {stop, normal, stopped, State};
handle_call({call1, {Mod, Fun, Args}, Max}, _From, #state{device=Device}=State) ->
	recon_trace:calls({Mod, Fun, Args}, Max, [{io_server, Device}]),
    {reply, ok, State};
handle_call({call2, TSpecs, Max}, _From, #state{device=Device}=State) ->
	recon_trace:calls(TSpecs, Max, [{io_server, Device}]),
    {reply, ok, State};
handle_call({call3, {Mod, Fun, Args}, Max, Opts}, _From, #state{device=Device}=State) ->
	Opts1 = proplists:delete(io_server, Opts),
	recon_trace:calls([{Mod,Fun,Args}], Max, Opts1++[{io_server, Device}]),
	{reply, ok, State};
handle_call({call4, TSpecs, {Max, Time}, Opts}, _From, #state{device=Device}=State) ->
	Opts1 = proplists:delete(io_server, Opts),
	recon_trace:calls(TSpecs, {Max, Time}, Opts1++[{io_server, Device}]),
	{reply, ok, State};
handle_call({call5, TSpecs, Max, Opts}, _From, #state{device=Device}=State) ->
	Opts1 = proplists:delete(io_server, Opts),
    recon_trace:calls(TSpecs, Max, Opts1++[{io_server, Device}]),
	{reply, ok, State};
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
terminate(_Reason, #state{device=Device, socket=Socket}) ->
    tcp_io_server:stop(Device),
	gen_tcp:close(Socket),
    recon_trace:clear(),
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