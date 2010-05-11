%% Copyright 2010 Gina Hagg 
%% Licensed under the Apache License, Version 2.0 (the "License"); 
%% you may not use this file except in compliance with the License. 
%% You may obtain a copy of the License at 
%%
%% http://www.apache.org/licenses/LICENSE-2.0 
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, 
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
%% See the License for the specific language governing permissions and 
%% limitations under the License.

-module(rs_util).

-export([send_json_request/3, send_request/4, send_lhttpc_request/4]).

-define(BASE_URL, "https://servers.api.rackspacecloud.com").
        
generate_default_headers(Token,post)->
		[{"X-Auth-Token",edoc_lib:escape_uri(Token)},{"Content-Type","application/json; charset=UTF-8"},{"Accept","application/json"}].

generate_default_headers(Token)->
		[{"X-Auth-Token",edoc_lib:escape_uri(Token)},{"Accept","application/xml"}].

send_json_request(Keys,Method,Action) ->
    inets:start(),
    ssl:start(),
	check_token_expiration(Keys,get_expiration()),
    Req = generate_json_request(Action),
    send_request(Method,Req).

send_request(Keys,Method,Action,[]) ->
    inets:start(),
    ssl:start(),
	check_token_expiration(Keys,get_expiration()),
    Req = generate_request(Action),
	%io:format("get:~n~p~n",[Req]),
    send_request(Method,Req);

send_request(Keys,Method,Action,Data) ->
    inets:start(),
    ssl:start(),	
	check_token_expiration(Keys,get_expiration()),
    Req = generate_request(Action,Data),
	%io:format("post:~n~p~n",[Req]),
    send_request(Method,Req).

send_request (Method,Req)->
	Resp = http:request(Method, Req, [{ssl, []}], []),
	%io:format("~nsend_request:resp~n~p~n",[Resp]), 
	case Resp of {ok, _}->
		{ok, {{_, Status, _},_,Body}}=Resp,
    	case Status of 200 ->
			{ok, Body};
		202->{ok, Body};
		203->{ok,Body};
		401->
			io:format("~nDaily Token expired!!, please call rs_util:get_auth_token(Keys).~n",[]),
			Body;
    	_->{error,Body}
		end;
	_-> Resp end.

send_lhttpc_request(Keys,Method,Action,Data)->
	crypto:start(),
    ssl:start(),
	lhttpc:start(),
	check_token_expiration(Keys,get_expiration()),
    Hdrs = generate_default_headers(get_token(),Method)++[{"Content-Length", integer_to_list(iolist_size(Data))}],
    Resp = lhttpc:request(generate_url(Action), Method, Hdrs, Data, 60000, []),
	case Resp of {ok,_} ->
    	Resp;
	_->{error,Resp}
	end.

generate_request(Action)->
	[Token,Server,_] = rs_auth:read_token(),
	RC_URL = Server ++ Action,
	{RC_URL, generate_default_headers(Token)}.

generate_json_request(Action)->
	[Token,Server,_] = rs_auth:read_token(),
	RC_URL = Server ++ Action,
	{RC_URL, generate_default_headers(Token,post)}.

get_token()->
	[Token,_,_] = rs_auth:read_token(),
	Token.

get_expiration()->
	[_,_,Expiration] = rs_auth:read_token(),
	Expiration.

generate_request(Action,Data)->
	[Token,Server,_] = rs_auth:read_token(),
	RC_URL = Server ++ Action,
	{RC_URL, generate_default_headers(Token,post), [],Data}.

generate_url(Action)->
	[_,Server,_] = rs_auth:read_token(),	
	Server ++ Action.

check_token_expiration(Keys,Expires)->
	%io:format("~ntoken:~p~n",[Keys]),
	NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	if NowSecs > Expires ->
		rs_auth:get_auth_token(Keys);
	true->false end.