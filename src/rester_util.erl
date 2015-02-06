-module(rester_util).

-export([
  normalise_headers/1,
  normalise_headers/2,
  is_json_response/1 ]).

-export_type([header/0]).

-type header() :: { binary(), binary() }.

%% @doc Returns a list of headers where every header name is lower case.
-spec normalise_headers( [ header() ] ) -> [ header() ].
normalise_headers( [ { HeaderName, HeaderValue } | T ] ) when is_binary( HeaderName ) andalso is_binary( HeaderValue ) ->
  normalise_headers( [ { HeaderName, HeaderValue } ] ++ T, [] );
normalise_headers( [] ) ->
  [].

%% @doc Returns a list of headers where every header name is lower case.
-spec normalise_headers( [ header() ], list() ) -> [ header() ].
normalise_headers( [ { HeaderName, HeaderValue } | T ], Acc ) when is_binary( HeaderName )
                                                                   andalso is_binary( HeaderValue )
                                                                   andalso is_list( Acc ) ->
  normalise_headers( T, 
                     [ { list_to_binary( string:to_lower( binary_to_list( HeaderName ) ) ), HeaderValue } ] ++ Acc );

normalise_headers( [], Acc ) when is_list( Acc ) ->
  Acc.


%% @doc Get response content-type, if exists.
-spec is_json_response( [ header() ] ) -> binary().
is_json_response( RespHeaders ) ->
  LRespHeaders = rester_util:normalise_headers( RespHeaders ),
  case proplists:get_value( <<"content-type">>, LRespHeaders, <<"application/x-undefined">> ) of
    <<"application/json", _/binary>> -> true;
    AnyOther                         -> false
  end.
