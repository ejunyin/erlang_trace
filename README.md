# erlang_trace

Providing erlang trace for cluster.

##src/
* [recon_trace.erl](<https://github.com/ferd/recon/blob/master/src/recon_trace.erl)
* [tcp_io_server.erl](https://github.com/rvirding/tcp_io_server/blob/master/tcp_io_server.erl)
* [rotating_logger.erl](https://github.com/mattwilliamson/rotating_logger/blob/master/src/rotating_logger.erl)
* logger_client.erl
* logger_client2.erl
* logger_server.erl
* logger_collector.erl
* logger_sender.erl

##guide
```erlang
%% start the logger_server.
logger_server:start().
```
```erlang
logger_client2:start().
logger_client2:calls({lists,last,fun(_) -> return_trace() end}, 100).

%% traced function start 
lists:last([1,2,3,4,5]).
lists:last([1,2,3,4,5,6]).
lists:last([1,2,3,4,5,6,7]).
%% traced function end

logger_client:stop().
```
