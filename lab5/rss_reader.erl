-module(rss_reader).
-include("logging.hrl").
-compile(export_all).
-define(RETRIEVE_INTERVAL,3000).

%% @doc start(Url, QPid)
%% Запуск сервера
start(Url,QPid)->
	% Инициализация сетевой службы
	application:start(inets),
	spawn(?MODULE,server,[Url,QPid]).

%% @doc Чтение ресурса
server(Url, QPid)->
	?INFO("URL ~p \n", [Url]),
	
	% {{Version, StatusCode, ReasonPhrase}, Headers, Body}
	{ok,{{_,StatusCode,_},_,Load}}=httpc:request(Url),
	case StatusCode of 
		200 ->
			?INFO("HTTPCode ~p \n", [StatusCode]),

			% Загрузка XML файла
			{RSS,_} = xmerl_scan:string(Load),

			case rss_parse:is_rss2_feed(RSS) of
				ok -> 
					% Добавление фида в очередь
					rss_queue:add_feed(QPid,RSS),
					
					% Ожидание сообщений
					receive
					after ?RETRIEVE_INTERVAL -> 
						server(Url, QPid)
					end;
				_ ->
					{error,not_rss2_feed}
			end;
		_ -> {error,StatusCode}
	end.