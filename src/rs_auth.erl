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

-module(rs_auth).

-export([get_auth_token/1, read_token/0,prepareCredentials/1]).

%-include("rackspace.hrl").

-define(AUTH_HOST,"https://auth.api.rackspacecloud.com/v1.0").

get_auth_token(Keys)->	
	inets:start(),
    ssl:start(), 
	case http:request(get, {?AUTH_HOST, prepareCredentials(Keys) }, [{ssl, []}], []) of
        {ok, Response} ->
			{_Status, _Headers, _ } =Response,
		    Token = lists:keyfind("x-storage-token",1,_Headers),
            Mgr = lists:keyfind("x-server-management-url",1,_Headers),
			store_token(Token,Mgr),
			Response;
        Error ->
            Error
    end.

twenty_four_hours_from_now()->
	NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	NowSecs + 24 * 60 * 60.	

store_token(Token,Mgr)->
	%store these two keys in dets with timestamp. We can use them 24 hours.
    {_,A} = dets:open_file(rackspace.dets,[]),
	%delete the old key.
	dets:delete_all_objects(A),
	dets:insert(A,{"x-storage-token",Token}),
	dets:insert(A,{"x-server-management-url",Mgr}),
	dets:insert(A,{"expires",twenty_four_hours_from_now()}),
	dets:close(A).

read_token()->
	{_,A} = dets:open_file(rackspace.dets,[]),
	[{_, {_,Token} }] = dets:lookup(A,"x-storage-token"),
    [{_, {_,Srv} }] = dets:lookup(A,"x-server-management-url"),
	[{_,Expires}] = dets:lookup(A,"expires"),
	dets:close(A),
	[Token,Srv,Expires].

prepareCredentials(Keys) ->
	{_,Username,Pass} = Keys,
	[{"X-Auth-User",edoc_lib:escape_uri(Username)},{"X-Auth-Key",edoc_lib:escape_uri(Pass)}].



		

