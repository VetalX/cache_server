# cache_server
Home task of erlang academy

### config
* env/cache_server.config 

### compile
* make

### devel start
* ./start-dev.sh
 
### production start
* ./_rel/cache_server/bin/cache_server {start|start_boot \<file\>|foreground|stop|restart|reboot|pid|ping|console|console_clean|console_boot \<file\>|attach|remote_console|upgrade|escript|rpc|rpcterms}

## Erlang API
###### support commands:
* cache_server_srv:insert(Key, Value).
* cache_server_srv:delete(Key).
* cache_server_srv:lookup(Key).
* cache_server_srv:lookup_by_date(DateFrom, DateTo).

## Tcp API
###### support commands:
* get key
* set key val
* del key
* get_by_date yyyy/mm/dd hh24:mi:ss yyyy/mm/dd hh24:mi:ss

## Http API
###### support commangs:
* curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key", "value":[1,2,3]}' http://localhost:8080/api/cache_server
* curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup","key":"some_key"}' http://localhost:8080/api/cache_server
* curl -H "Content-Type: application/json" -X POST -d '{"action":"delete","key":"some_key"}' http://localhost:8080/api/cache_server
* curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup_by_date","date_from":"2015/1/1 00:00:00", "date_to":"2015/1/10 23:59:59"}' http://localhost:8080/api/cache_server
