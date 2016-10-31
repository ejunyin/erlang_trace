%% @author ejunyin
%% @doc @todo Add description to logger_client.


-module(logger_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/0, calls/2, calls/3]).
-export([stop_low_priority_trace/0]).
-export([resume_low_priority_trace/0]).
-export([stop_all_trace/0]).
-export([resume_all_trace/0]).
-define(PORT_NUM, 12345).

start(Args) ->
	application:start(sasl),
	application:start(os_mon),
	{ok, Collector} = logger_collector:start_link(Args),
	Collector ! {set_handler, Collector},
	put(device, Collector),
	ok.

stop() ->
    Device = get(device),	
    Device!stop,
    recon_trace:clear().

stop_low_priority_trace() ->
    recon_trace:stop_low_priority_trace().

resume_low_priority_trace() ->
    recon_trace:resume_low_priority_trace().

stop_all_trace() ->
    recon_trace:stop_all_trace().

resume_all_trace() ->
    recon_trace:resume_all_trace().

calls({Mod, Fun, Args}, Max) ->
	calls({Mod, Fun, Args, high}, Max);
calls({Mod, Fun, Args, Prio}, Max) ->
	Device = get(device),
    recon_trace:calls({Mod, Fun, Args, Prio}, Max, [{io_server, Device}]);
calls(TSpecs = [_|_], Max) ->
	Device = get(device),
    recon_trace:calls(TSpecs, Max, [{io_server, Device}]).

calls({Mod, Fun, Args}, Max, Opts) ->
	calls({Mod, Fun, Args, high}, Max, Opts);
calls({Mod, Fun, Args, Prio}, Max, Opts) ->
	Device = get(device),
	Opts1 = proplists:delete(io_server, Opts),
    recon_trace:calls([{Mod,Fun,Args,Prio}], Max, Opts1++[{io_server, Device}]);
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


