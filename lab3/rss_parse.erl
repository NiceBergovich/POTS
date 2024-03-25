-module(rss_parse).

-export([
    is_rss2_feed/1, 
    compare_feed_items/2,
    getitem/1, 
    run_test/0, 
    run_test_1/1, 
    run_test_2/1, 
    run_test_3/1
]).

%% Команда используется для включения заголовков файла .hrl
%% В частности, включаются необходимые макросы и определения для работы с XML
-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xpath, [string/2]).
-import(lists, [map/2]).

%% 1. Получает результат чтения .xml файла и возвращает атом true, 
%% если используется rss версии 2.0
is_rss2_feed(XML) ->
    % функция xmerl_xpath:string используется для поиска элемента <rss>
    % Возвращает список найденных дочерних узлов
    Ver = xmerl_xpath:string("/rss[@version='2.0']/text()", XML),
    
    % Если узлы были найдены
    length(Ver) > 0.
    
%% 2.
%% Эта функция принимает RSS-ленту версии 2.0 и возвращает 
%% список всех элементов <item> в записи (<channel>)
%%
get_feed_items(RSS2Feed) ->
    % Ищем в корне "//" ищем все дочерние тэги channel/item
    xmerl_xpath:string("//channel/item", RSS2Feed).

%% 3. Функция, которая ищет время публикации в xml документе
%% 
get_item_time(Item) ->
    [T] = xmerl_xpath:string("/item/pubDate/text()", Item),
    
    % Функция для преобразования  строки даты в Erlang-совместимый формат времени
    % Rec T#type.key
    ND = httpd_util:convert_request_date(T#xmlText.value),
    
    % Преобразование преобразует дату и время в количество секунд с 1 января 1970 года
    Res = calendar:datetime_to_gregorian_seconds(ND),
    
    if
        Res == true  -> Res;
        true -> bad_date
    end.


%% 4. 
%% @private
%% @doc Эта вспомогательная функция просматривает заданный XML элемент
%%      и удаляет из него сведения о других XML элементах, например содержащиеся в полях
%%      "parents" или "pos".
%%
%% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%%
extract_xml(Elem = #xmlElement{}) ->
    % Структура данных, в которой по итогу будет xml документ без лишней информации
    Elem#xmlElement{
        parents=[], 
        pos=0,
	    namespace=none, % попробуем упростить структуру
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)
    };
% Сопоставление с типом данных Rec
extract_xml(Attr = #xmlAttribute{}) -> 
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) -> 
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) -> 
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) -> Other.


collect_feed_item_data(Item) when is_record(Item, xmlElement) ->
    [Guid] = xmerl_xpath:string("guid/text()", Item),

    [Title] = xmerl_xpath:string("title/text()", Item),

    [Link] = xmerl_xpath:string("link/text()", Item),

    [PubDate] = xmerl_xpath:string("pubDate/text()", Item),
    [Guid, Title, Link, PubDate].


%% 5. Функция, получающая два Record 
compare_feed_items(OldItem, NewItem) when is_record(OldItem, xmlElement), is_record(NewItem, xmlElement) ->
    % Получаем глобальный уникальный идентификатор, заголовок, ссылка и время публикации
    [Guid1, Title1, Link1, PubDate1] = collect_feed_item_data(OldItem),
    [Guid2, Title2, Link2, PubDate2] = collect_feed_item_data(NewItem),
    compare_feed_fulls(Guid1, Title1, Link1, PubDate1, Guid2, Title2, Link2, PubDate2).


compare_feed_fulls(Guid, Title, Link, PubDate, Guid, Title, Link, PubDate) ->
    same;
compare_feed_fulls(_, _, Link, _, _, _, Link, _) ->
    updated;
compare_feed_fulls(Guid, _, _, _, Guid, _, _, _) ->
    updated;
compare_feed_fulls(_, Title, _, _, _, Title, _, _) ->
    updated;
compare_feed_fulls(_, _, _, _, _, _, _, _) ->
    different.








%% Helper function for quick testing
%% 
getitem(F) ->
    % Строка с именем файла на чтение
    F,

    % Парсинг .xml файла 
    {XML, _} = xmerl_scan:file(F),

    % Получаем первый <item>
    [X1| _] = get_feed_items(XML),
    extract_xml(X1).

run_test() ->
    run_test_3("RIA.xml").

run_test_1(F) ->
    {XML, _} = xmerl_scan:file(F),
    is_rss2_feed(XML).

run_test_2(F) ->
    {XML, _} = xmerl_scan:file(F),
    map(fun get_item_time/1, get_feed_items(XML)).

run_test_3(F) ->
    {XML, _} = xmerl_scan:file(F),
    [X1, X2| _] = get_feed_items(XML),
    [compare_feed_items(X1, X2), compare_feed_items(X1, X1)].

