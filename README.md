# erlang_trace

Providing erlang trace for cluster.

##src/
* [recon_trace.erl](<https://github.com/ferd/recon/blob/master/src/recon_trace.erl)
* [rotating_logger.erl](https://github.com/mattwilliamson/rotating_logger/blob/master/src/rotating_logger.erl)
* logger_client.erl
* logger_server.erl
* logger_collector.erl
* logger_sender.erl
* res_monitor.erl

##guide
```erlang
%% start the logger_server.
logger_server:start().
```
```erlang
logger_client:start("169.254.100.1").
logger_client:calls({lists,last,fun(_) -> return_trace() end}, 100).
logger_client:calls({lists,min,fun(_) -> return_trace() end}, {1000,1000}).

%% traced function start 
lists:last([1,2,3,4,5]).
lists:last([1,2,3,4,5,6]).
lists:last([1,2,3,4,5,6,7]).
lists:min([1,2,3,4,5,6,7]).
%% traced function end

logger_client:stop().
```
