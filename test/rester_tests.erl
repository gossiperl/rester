-module(rester_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVICE_NAME, httpbin).
-define(PROTOCOL, <<"http">>).
-define(URL, <<"httpbin.org">>).
-define(PORT, 80).

rester_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun util_test/0,
    fun configuration_test/0,
    fun test_delete_plain/0,
    fun test_get_plain/0,
    fun test_head_plain/0,
    fun test_patch_plain/0,
    fun test_post_plain/0,
    fun test_put_plain/0,
    fun test_get_qs/0,
    fun test_get_custom_header/0 ] }.

start() ->
  Apps = [ crypto, asn1, public_key, ssl, idna, hackney, jsx, rester ],
  [ application:start( App ) || App <- Apps ],
  rester_sup:add_http_endpoint( ?SERVICE_NAME, ?URL ).

stop(_State) ->
  noreply.

util_test() ->
  MixedCaseHeaderName = <<"X-Mixed-Case">>,
  LowerCaseHeaderName = <<"x-mixed-case">>,
  HeaderValue         = <<"header-value">>,
  InputHeaders = [ { MixedCaseHeaderName, HeaderValue } ],
  Normalised   = rester_util:normalise_headers( InputHeaders ),
  ?assertMatch( [ { LowerCaseHeaderName, HeaderValue } ], Normalised ).

configuration_test() ->
  timer:sleep(1000),
  ConfigCallResult = gen_server:call( ?SERVICE_NAME, show_config ),
  ?assertMatch( { ok, { config, ?SERVICE_NAME, ?PROTOCOL, ?URL, ?PORT, [], [] } }, ConfigCallResult ).

test_delete_plain() ->
  Response = gen_server:call( ?SERVICE_NAME, { delete, <<"/delete">> } ),
  ?assertMatch( { ok, 200, _ }, Response ).

test_get_plain() ->
  Response = gen_server:call( ?SERVICE_NAME, { get, <<"/get">> } ),
  ?assertMatch( { ok, 200, _, _ }, Response ).

test_head_plain() ->
  Response = gen_server:call( ?SERVICE_NAME, { head, <<"/get">> } ),
  ?assertMatch( { ok, 200, _ }, Response ).

test_patch_plain() ->
  PatchData = <<"Some patch data">>,
  Response = gen_server:call( ?SERVICE_NAME, { patch, <<"/patch">>, PatchData } ),
  ?assertMatch( { ok, 200, _, _ }, Response ),
  { ok, 200, Body, _ } = Response,
  ?assertEqual( PatchData, proplists:get_value( <<"data">>, Body ) ).

test_post_plain() ->
  PostData = <<"This is some post data">>,
  Response = gen_server:call( ?SERVICE_NAME, { post, <<"/post">>, PostData } ),
  ?assertMatch( { ok, 200, _, _ }, Response ),
  { ok, 200, Body, _ } = Response,
  ?assertEqual( PostData, proplists:get_value( <<"data">>, Body ) ).

test_put_plain() ->
  PutData = <<"And finally, some put data">>,
  Response = gen_server:call( ?SERVICE_NAME, { put, <<"/put">>, PutData } ),
  ?assertMatch( { ok, 200, _, _ }, Response ),
  { ok, 200, Body, _ } = Response,
  ?assertEqual( PutData, proplists:get_value( <<"data">>, Body ) ).

test_get_qs() ->
  QsData = [ { arg1, 1 }, { arg2, true }, { arg3, <<"some data">> } ],
  Response = gen_server:call( ?SERVICE_NAME, { get, <<"/get">>, QsData } ),
  ?assertMatch( { ok, 200, _, _ }, Response ),
  { ok, 200, Body, _ } = Response,
  ?assertEqual( << (?PROTOCOL)/binary, "://", (?URL)/binary, "/get?arg1=1&arg2=true&arg3=some data">>, proplists:get_value( <<"url">>, Body ) ).

test_get_custom_header() ->
  CustomHeader = <<"x-custom-header">>,
  CustomHeaderValue = <<"custom-header-value">>,
  Response = gen_server:call( ?SERVICE_NAME, { get, <<"/get">>, [], [ { CustomHeader, CustomHeaderValue } ], [] } ),
  ?assertMatch( { ok, 200, _, _ }, Response ),
  { ok, 200, Body, _ } = Response,
  RespHeaders = proplists:get_value( <<"headers">>, Body ),
  NormalisedHeaders = rester_util:normalise_headers( RespHeaders ),
  ?assertEqual( CustomHeaderValue, proplists:get_value( CustomHeader, NormalisedHeaders ) ).
