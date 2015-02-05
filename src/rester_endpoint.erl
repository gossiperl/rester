-module(rester_endpoint).

-behaviour(gen_server).

-record( config, { name = undefined :: atom(),
                   protocol :: binary(),
                   host :: binary(),
                   port :: integer(),
                   default_headers = [] :: list(),
                   options = [] :: list() } ).

-export([ start_link/4, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ]).

-type qs_arg_name() :: atom().
-type qs_arg_value() :: atom() | binary() | number() | boolean().
-type qs_arg() :: { qs_arg_name(), qs_arg_value() }.
-type http_verb_response() :: get | patch | post | put.
-type http_verb_no_response() :: delete | head.
-type hackney_option() :: term().
-type request_data() :: binary().
-type rest_response() :: binary() | [ term() ].

start_link( undefined, Protocol, Host, Port ) ->
  gen_server:start_link(?MODULE, [ undefined, Protocol, Host, Port ], []);

start_link( Name, Protocol, Host, Port ) when is_atom( Name )
                                              andalso is_atom( Protocol )
                                              andalso is_binary( Host )
                                              andalso is_integer( Port ) ->
  gen_server:start_link({ local, Name }, ?MODULE, [ Name, Protocol, Host, Port ], []).

init([ Name, Protocol, Host, Port ]) ->
  { ok, #config{ name = Name, protocol = get_protocol( Protocol ), host = Host, port = Port } }.

%% Sync: Set default headers for calls on this endpoint.
handle_call( { set, headers, Headers }, From, C = #config{} ) ->
  NewConfig = C#config{ default_headers = Headers },
  gen_server:reply( From, ok ),
  { noreply, NewConfig };

%% Sync: Set default options for calls on this endpoint.
handle_call( { set, options, Options }, From, C = #config{} ) ->
  NewConfig = C#config{ options = Options },
  gen_server:reply( From, ok ),
  { noreply, NewConfig };

%% Get current configuration.
handle_call( show_config, From, C = #config{} ) ->
  gen_server:reply( From, { ok, C } ),
  { noreply, C };

%%% ***********************
%%% REST Operations       /
%%% ***********************

%% @doc Execute DELETE request with path only.
handle_call( { delete, Path }, From, C = #config{} ) ->
  gen_server:reply( From, no_response_call( delete, Path, [], [], C ) ),
  { noreply, C };

%% @doc Execute DELETE request with path, headers and options.
handle_call( { delete, Path, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, no_response_call( delete, Path, Headers, Options, C ) ),
  { noreply, C };

%% @doc Execute GET request with path only.
handle_call( { get, Path }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( get, Path, [], <<"">>, [], [], C ) ),
  { noreply, C };

%% @doc Execute GET request with path and query string arguments only.
handle_call( { get, Path, QSOptions }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( get, Path, QSOptions, <<"">>, [], [], C ) ),
  { noreply, C };

%% @doc Execute GET request with path, query string arguments, headers and options.
handle_call( { get, Path, QSOptions, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( get, Path, QSOptions, <<"">>, Headers, Options, C ) ),
  { noreply, C };

%% @doc Execute HEAD request with path only.
handle_call( { head, Path }, From, C = #config{} ) ->
  gen_server:reply( From, no_response_call( head, Path, [], [], C ) ),
  { noreply, C };

%% @doc Execute HEAD request with path, headers and options.
handle_call( { head, Path, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, no_response_call( head, Path, Headers, Options, C ) ),
  { noreply, C };

%% @doc Execute PATCH request with path and data only.
handle_call( { patch, Path, Data }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( patch, Path, [], Data, [], [], C ) ),
  { noreply, C };

%% @doc Execute PATCH request with path, data, headers and options.
handle_call( { patch, Path, Data, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( patch, Path, [], Data, Headers, Options, C ) ),
  { noreply, C };

%% @doc Execute POST request with path and data only.
handle_call( { post, Path, Data }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( post, Path, [], Data, [], [], C ) ),
  { noreply, C };

%% @doc Execute POST request with path, data, headers and options.
handle_call( { post, Path, Data, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( post, Path, [], Data, Headers, Options, C ) ),
  { noreply, C };

%% @doc Execute PUT request with path and data only.
handle_call( { put, Path, Data }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( put, Path, [], Data, [], [], C ) ),
  { noreply, C };

%% @doc Execute PUT request with path, data, headers and options.
handle_call( { put, Path, Data, Headers, Options }, From, C = #config{} ) ->
  gen_server:reply( From, response_call( put, Path, [], Data, Headers, Options, C ) ),
  { noreply, C }.

%%% ***********************
%%% REST Operations   / END
%%% ***********************

handle_cast( _Msg, LoopData ) ->
  { noreply, LoopData }.

handle_info(_Msg, LoopData) ->
  {noreply, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _LoopData) ->
  ok.

-spec no_response_call( http_verb_no_response(),
                        binary(),
                        [ rester_util:header() ],
                        [ hackney_option() ],
                        term() ) -> { ok, non_neg_integer(), [ rester_util:header() ] }
                                    | { error, non_neg_integer(), [ rester_util:header() ] }.
no_response_call( Method, Path, Headers, Options, #config{ protocol        = Proto,
                                                           host            = H,
                                                           port            = Port,
                                                           default_headers = DH,
                                                           options         = O } ) ->
  P = list_to_binary(integer_to_list( Port )),
  Uri = <<Proto/binary, "://", H/binary, ":", P/binary, "/", Path/binary>>,
  { RespStatusCode, RespHeaders } = case hackney:request( Method, Uri, DH ++ Headers, <<"">>, O ++ [ Options ] ) of
    { ok, StatusCode1, Headers1, _ } -> { StatusCode1, Headers1 };
    { ok, StatusCode2, Headers2 }    -> { StatusCode2, Headers2 }
  end,
  case trunc( RespStatusCode/100 ) of
    2 -> { ok, RespStatusCode, RespHeaders };
    _ -> { error, RespStatusCode, RespHeaders }
  end.

%% @doc Execute a HTTP call for which there could potentially be a response.
-spec response_call( http_verb_response(),
                     binary(),
                     [ qs_arg() ],
                     request_data(),
                     [ rester_util:header() ],
                     [ hackney_option() ],
                     term() ) -> { ok, non_neg_integer(), rest_response(), [ rester_util:header() ] }
                                 | { error, non_neg_integer(), [ rester_util:header() ] }.
response_call( Method, Path, QSOptions, Data, Headers, Options, #config{ protocol        = Proto,
                                                                         host            = H,
                                                                         port            = Port,
                                                                         default_headers = DH,
                                                                         options         = O } ) ->
  P = list_to_binary(integer_to_list( Port )),
  Uri = <<Proto/binary, "://", H/binary, ":", P/binary, "/", Path/binary, ( get_query_string( QSOptions, <<"">> ) )/binary>>,
  { ok, StatusCode, RespHeaders, Ref } = hackney:request( Method, Uri, DH ++ Headers, Data, O ++ [ Options ] ),
  case trunc(StatusCode/100) of
    2 ->
      { ok, Body } = hackney:body(Ref),
      case response_content_type( RespHeaders ) of
        <<"application/json">> ->
          { ok, StatusCode, get_json_response( Body ), RespHeaders };
        _ ->
          { ok, StatusCode, Body, RespHeaders }
      end;
    _ ->
      { error, StatusCode, RespHeaders }
  end.

%% @doc Given a list of arguments, create query string.
-spec get_query_string( [ qs_arg() ], binary() ) -> binary().
get_query_string( [ { Param, Value } | T ], Bin ) when is_atom(Param)
                                                       andalso is_atom(Value)
                                                       andalso is_binary(Bin) ->
  Bin2 = << Bin/binary,
            ( get_qs_join( Bin ) )/binary,
            ( list_to_binary( atom_to_list( Param ) ) )/binary,
            "=", ( list_to_binary( http_uri:encode( atom_to_list( Value ) ) ) )/binary>>,
  get_query_string( T, Bin2 );

get_query_string( [ { Param, Value } | T ], Bin ) when is_atom(Param)
                                                       andalso is_number(Value)
                                                       andalso is_binary(Bin) ->
  Bin2 = << Bin/binary,
            ( get_qs_join( Bin ) )/binary,
            ( list_to_binary( atom_to_list( Param ) ) )/binary,
            "=", ( list_to_binary( lists:flatten(io_lib:format("~p", [ Value ])) ) )/binary>>,
  get_query_string( T, Bin2 );

get_query_string( [ { Param, Value } | T ], Bin ) when is_atom(Param)
                                                       andalso is_binary(Value)
                                                       andalso is_binary(Bin) ->
  Bin2 = << Bin/binary,
            ( get_qs_join( Bin ) )/binary,
            ( list_to_binary( atom_to_list( Param ) ) )/binary,
            "=", (list_to_binary( http_uri:encode( binary_to_list(Value) ) ) )/binary>>,
  get_query_string( T, Bin2 );

get_query_string( [ { Param, Value } | T ], Bin ) when is_atom(Param)
                                                       andalso is_boolean(Value)
                                                       andalso is_binary(Bin) ->
  Bin2 = << Bin/binary,
            ( get_qs_join( Bin ) )/binary,
            ( list_to_binary( http_uri:encode( atom_to_list( Param ) ) ) )/binary,
            "=", ( list_to_binary( lists:flatten(io_lib:format("~p", [ Value ])) ) )/binary>>,
  get_query_string( T, Bin2 );

get_query_string( [], Bin ) ->
  Bin.

-spec get_qs_join( binary() ) -> binary().
get_qs_join( <<"">> ) -> <<"?">>;
get_qs_join( Any ) when is_binary(Any) -> <<"&">>.

%% @doc Get binary representiation of the protocol.
-spec get_protocol( rester_sup:http_protocol() ) -> binary().
get_protocol( http ) -> <<"http">>;
get_protocol( https ) -> <<"https">>.

%% @doc Get response content-type, if exists.
-spec response_content_type( [ rester_util:header() ] ) -> binary().
response_content_type( RespHeaders ) ->
  LRespHeaders = rester_util:normalise_headers( RespHeaders ),
  proplists:get_value( <<"content-type">>, LRespHeaders, <<"application/x-undefined">> ).

%% @doc Parse binary reponse to term.
-spec get_json_response(binary()) -> term().
get_json_response( BinResponse ) ->
  try
    jsx:decode( BinResponse )
  catch _:_ ->
    BinResponse
  end.
  