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

-module(rs_image).
-export([ describe_all/1, describe_all_details/1, describe/2,create/3,terminate/2]).

describe_all(Keys) ->
	Response = rs_util:send_request(Keys,get,"/images",[]),
	case Response of {ok, Body}->
	{ok,{_,_,Result},_}= erlsom:simple_form(Body),
		lists:map(fun(X)->{_,BBody,_} = X, BBody end, Result);
	_->Response end.

describe_all_details(Keys) ->
	Response = rs_util:send_json_request(Keys,get,"/images/detail"),
	case Response of {ok, Body}->
	{struct,[{_,{array,Props}}]}=mochijson:decode(Body),
	lists:map(fun(X)->{struct,Prop}=X, Prop end, Props);
	_->Response end.

describe(Keys,ImageId) ->
	Response = rs_util:send_json_request(Keys,get,"/images/"++ ImageId),	
	case Response of {ok, Body}->
	{struct,[{_,{struct,Props}}]} = mochijson:decode(Body),
	{ok,Props};
	_->Response end.

terminate(Keys,ImageId) ->
	rs_util:send_request(Keys,delete,"/images/" ++ ImageId, []).


%{"image" : {"serverId" : 12,"name" : "Just in case"}}
create(Keys,Name,InstanceId)->
	J1 = {struct,[{<<"serverId">>,InstanceId},{<<"name">>,Name}]},
	ImgJson = {struct,[{<<"image">>,J1}]},
    Body = iolist_to_binary(mochijson:encode(ImgJson)),
	Resp = rs_util:send_lhttpc_request(Keys,post,"/images",Body),
	case Resp of {ok,_} ->
    	{ok,{_,_,RBody}} = Resp,
		{struct,[{_,{struct,Props}}]} = mochijson:decode(RBody),
		{ok,Props};
	_->  Resp end.
