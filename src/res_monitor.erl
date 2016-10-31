%% @author ejunyin
%% @doc @todo Add description to res_monitor.


-module(res_monitor).

-define(TIMEOUT, 2000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([memory_util/0, cpu_util/0, cpu_util_get_cpu_load_loop/1]).

memory_util() ->
    case memsup:get_system_memory_data() of
    [] ->
        {1000, 1000};
    MemData ->
        {value, {_, Total}} = lists:keysearch(total_memory, 1, MemData),
        {value, {_, Free}} = lists:keysearch(free_memory, 1, MemData),

        %% Buffered and cached memory was not taken care of
        Buffered = case lists:keysearch(buffered_memory, 1, MemData) of
                        {value, {buffered_memory, B}} -> B;
                        false -> 0
                   end,
        Cache = case lists:keysearch(cached_memory, 1, MemData) of
                        {value, {cached_memory, C}} -> C;
                        false -> 0
                   end,
        NewFree = Free + Cache + Buffered,
        {round(Total/(1024*1024)), round(NewFree/(1024*1024))}
    end.

%%%------------------------------------------------------------------
%%% cpu_util_get_cpu_load()
%%%------------------------------------------------------------------
cpu_util_get_cpu_load() ->
    NewLoad = cpu_sup:util([per_cpu]),
    case NewLoad of
        {error, _} ->
            0;
        {all, 0, 0, []} -> % Returns this if cpu_sup is not available.
            0;
        Utils ->
            Fun = fun({_Cpus, Busy, _NonBusy, _Misc}, {TotBusy, NoOfCpu}) ->
                          {TotBusy + Busy, NoOfCpu + 1}
                  end,
            {TotalBusy, NoOfCpu} = lists:foldl(Fun, {0, 0}, Utils),
            round(TotalBusy / NoOfCpu)
    end.

%%%------------------------------------------------------------------
%%% cpu_util_get_cpu_load_loop()
%%%------------------------------------------------------------------
cpu_util_get_cpu_load_loop(Load) ->
    receive
        measure ->
            NewLoad = cpu_util_get_cpu_load(),
            cpu_util_get_cpu_load_loop(NewLoad);
        {Caller,Ref,get_cpu_load} ->
            Caller ! {cpu_load,Ref,Load},
            cpu_util_get_cpu_load_loop(Load);
        _Message ->
            cpu_util_get_cpu_load_loop(Load)
    end.


%%%------------------------------------------------------------------
%%% cpu_util()
%%%------------------------------------------------------------------
cpu_util() ->
    GetCpuLoadFun =
        fun(Pid) ->
                Ref = erlang:make_ref(),
                Pid ! {self(),Ref,get_cpu_load},
                receive
                    {cpu_load, Ref, Load} ->
                        Load
                after 2000 ->
                        cpu_util_get_cpu_load()
                end
        end,
    case whereis(sysPerfCpuMonitor) of
        undefined ->
            Pid = spawn(?MODULE, cpu_util_get_cpu_load_loop, [0]),
            register(sysPerfCpuMonitor, Pid),
            {ok, _TRef} = timer:send_interval(?TIMEOUT,
                                              sysPerfCpuMonitor,
                                              measure),
            sysPerfCpuMonitor ! measure,
            timer:sleep(200),
            sysPerfCpuMonitor ! measure,
            GetCpuLoadFun(Pid);
        Pid ->
            GetCpuLoadFun(Pid)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


