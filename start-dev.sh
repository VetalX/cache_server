erl -pa ebin/ deps/*/ebin/ -sname cache_server -boot start_sasl -config env/cache_server -s cache_server_dev
