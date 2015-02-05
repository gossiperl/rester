-module(rester_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([ add_http_endpoint/2,
          add_https_endpoint/2, 
          add_endpoint/3,
          add_endpoint/4,
          remove_endpoint/1 ]).

-export_type([ http_protocol/0 ]).

-define(DEFAULT_HTTP_PORT, 80).
-define(DEFAULT_HTTPS_PORT, 443).

-type http_protocol() :: http | https.
-type remove_endpoint_error() :: running | restarting | not_found | simple_one_for_one.

%% @doc Starts erflux supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor init.
init([]) ->
  {ok, {{one_for_one, 10, 10}, [] }}.

%% @doc Add named HTTP endpoint.
-spec add_http_endpoint( atom(), binary() ) -> supervisor:child_spec() | supervisor:startchild_ret() | supervisor:startchild_err().
add_http_endpoint( Name, Host ) when is_atom( Name ) andalso is_binary( Host ) ->
  add_endpoint_( http, Name, Host, ?DEFAULT_HTTP_PORT ).

%% @doc Add named HTTPS endpoint.
-spec add_https_endpoint( atom(), binary() ) -> supervisor:child_spec() | supervisor:startchild_ret() | supervisor:startchild_err().
add_https_endpoint( Name, Host ) when is_atom( Name ) andalso is_binary( Host ) ->
  add_endpoint_( https, Name, Host, ?DEFAULT_HTTPS_PORT ).

%% @doc Add named endpoint.
-spec add_endpoint( http_protocol(), atom(), binary() ) -> supervisor:child_spec() | supervisor:startchild_ret() | supervisor:startchild_err().
add_endpoint( http, Name, Host ) when is_atom( Name ) andalso is_binary( Host ) ->
  add_endpoint_( http, Name, Host, ?DEFAULT_HTTP_PORT );

%% @doc Add named endpoint.
add_endpoint( https, Name, Host ) when is_atom( Name ) andalso is_binary( Host ) ->
  add_endpoint_( https, Name, Host, ?DEFAULT_HTTPS_PORT ).

%% @doc Add named endpoint.
-spec add_endpoint( http_protocol(), atom(), binary(), non_neg_integer() ) -> supervisor:child_spec() | supervisor:startchild_ret() | supervisor:startchild_err().
add_endpoint( http, Name, Host, Port ) when is_atom( Name ) andalso is_binary( Host ) andalso is_integer( Port ) ->
  add_endpoint_( http, Name, Host, Port );

%% @doc Add named endpoint.
add_endpoint( https, Name, Host, Port ) when is_atom( Name ) andalso is_binary( Host ) andalso is_integer( Port ) ->
  add_endpoint_( https, Name, Host, Port ).

-spec add_endpoint_( http_protocol(), atom(), binary(), non_neg_integer() ) -> supervisor:child_spec() | supervisor:startchild_ret() | supervisor:startchild_err().
add_endpoint_( Protocol, Name, Host, Port ) ->
  supervisor:start_child(?MODULE, {
    Name,
    { rester_endpoint, start_link, [ Name, Protocol, Host, Port ] },
    permanent,
    brutal_kill,
    supervisor,
    []
  }).

%% @doc Remove an endpoint by name.
-spec remove_endpoint( atom() ) -> ok | { error, remove_endpoint_error() }.
remove_endpoint( Name ) when is_atom( Name ) orelse is_binary( Name ) ->
  case supervisor:terminate_child(?MODULE, Name ) of
    ok ->
      supervisor:delete_child(?MODULE, Name);
    {error, Reason} ->
      {error, Reason}
  end.
