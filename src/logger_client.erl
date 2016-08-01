%% @author ejunyin
%% @doc @todo Add description to logger_client.


-module(logger_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, calls/2, calls/3]).

-define(PORT_NUM, 12345).

start() ->
	{ok, Socket} = gen_tcp:connect("localhost", ?PORT_NUM, [binary], 1000),
	{ok, Device} = tcp_io_server:start(Socket),
	put(socket, Socket),
	put(device, Device),
	ok.

stop() ->
    Device = get(device),
    tcp_io_server:stop(Device),
	Socket = get(socket),
	gen_tcp:close(Socket),
    recon_trace:clear().

calls({Mod, Fun, Args}, Max) ->
	Device = get(device),
    recon_trace:calls({Mod, Fun, Args}, Max, [{io_server, Device}]);
calls(TSpecs = [_|_], Max) ->
	Device = get(device),
    recon_trace:calls(TSpecs, Max, [{io_server, Device}]).

calls({Mod, Fun, Args}, Max, Opts) ->
	Device = get(device),
	Opts1 = proplists:delete(io_server, Opts),
    recon_trace:calls([{Mod,Fun,Args}], Max, Opts1++[{io_server, Device}]);
calls(TSpecs = [_|_], {Max, Time}, Opts) ->
	Device = get(device),
	Opts1 = proplists:delete(io_server, Opts),
	recon_trace:calls(TSpecs, {Max, Time}, Opts1++[{io_server, Device}]);
calls(TSpecs = [_|_], Max, Opts) ->
	Device = get(device),
	Opts1 = proplists:delete(io_server, Opts),
	recon_trace:calls(TSpecs, Max, Opts1++[{io_server, Device}]).


%% ====================================================================
%% Internal functions
%% ====================================================================


