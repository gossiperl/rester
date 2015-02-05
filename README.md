# rester - REST with hackney

A very simple REST wrapper library built on top of hackney and jsx.

# Running

    Apps = [ crypto, asn1, public_key, ssl, idna, hackney, jsx, rester ],
    [ application:start( App ) || App <- Apps ].

# Endpoints

In order to talk to a REST endpoint, an endpoint has to be registered with rester. To add Github API endpoint:

    _ = rester_sup:add_https_endpoint( github, <<"api.github.com">> ).

# Request basics

    { ok, StatusCode, Body, ResponseHeaders } = gen_server:call( github, { get, <<"/users/radekg/gists">> } ).

`Body` may be one of the two:

- `binary()`: plain text of the response
- `term()`: if `content-type` of the response is `application/json`, rester will attempt deserializing the response to a term

# Default API headers

Certain APIs require certain headers to be sent with every request, this could be authentications headers or content-type. rester provides a way of configuring the endpoint to always use a certain set of headers. To do so, after creating the endpoint, run the following command:

    gen_server:call( { set, headers, [ { <<"header-name">>, <<"header-value">> }, ... ] } ).

# Requests

## DELETE

    gen_server:call( Name, { delete, Path } )
    gen_server:call( Name, { delete, Path, Headers, Options } )

## GET

    gen_server:call( Name, { get, Path } )
    gen_server:call( Name, { get, Path, QSOptions } )
    gen_server:call( Name, { get, Path, QSOptions, Headers, Options } )

## HEAD

    gen_server:call( Name, { head, Path } )
    gen_server:call( Name, { head, Path, Headers, Options } )

## PATCH

    gen_server:call( Name, { patch, Path, Data } )
    gen_server:call( Name, { patch, Path, Data, Headers, Options } )

## POST

    gen_server:call( Name, { post, Path, Data } )
    gen_server:call( Name, { post, Path, Data, Headers, Options } )

## PUT

    gen_server:call( Name, { put, Path, Data } )
    gen_server:call( Name, { put, Path, Data, Headers, Options } )

# Running tests

    rebar clean get-deps compile eunit

# Author

Radoslaw Gruchalski <radek@gruchalski.com>
https://github.com/radekg

# License

The MIT License (MIT)

Copyright (c) 2015 Radoslaw Gruchalski <radek@gruchalski.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.