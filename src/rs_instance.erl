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

-module(rs_instance).

-export([run/4, run/5,run/6, terminate/2, describe_all/1, describe_instance/2, 
		 describe_all_details/1,reboot/3,rebuild/3, resize/3,confirm_resized/2,
		 revert_resized/2,list_addresses/2,list_public_addresses/2,share_ip_address/3,unshare_ip_address/3,
		 create_shared_ip_group/2,terminate_shared_group/2,update_admin_pwd/3,
		 update_server_name/3,create_backup_schedule/5,disable_backup_schedule/2,list_backup_schedules/2,shared_ip_groups/1]).

-export([get_limits/1,get_flavor_details/2,list_flavors/1]).

%-include("rackspace.hrl").
-define(NAMESPACE, "http://docs.rackspacecloud.com/servers/api/v1.0").
-define(LNM, 50).
%Keys = rs_keys:new(Uname,Pass).

describe_all(Keys) ->
	Response = rs_util:send_request(Keys,get,"/servers",[]),
	parse_describes_response(Response).

describe_all_details(Keys) ->
	Response = rs_util:send_json_request(Keys,get,"/servers/detail"),
	parse_describes_response_detail(Response).

describe_instance(Keys,InstanceId) ->
	Response = rs_util:send_request(Keys,get,"/servers/"++ InstanceId,[]),
    parse_describes_response(Response).

terminate(Keys,InstanceId) ->	
	rs_util:send_request(Keys,delete,"/servers/" ++ InstanceId, []).


%{"reboot" : {"type" : "HARD"}}
%HARD||SOFT
reboot(Keys,InstanceId,RebootType)->
	J2 = {struct,[{<<"type">>,RebootType}]},
	Js = {struct,[{<<"reboot">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	Resp = rs_util:send_lhttpc_request(Keys,post,"/servers/"++ InstanceId ++"/action",Body),
	{ok,{Status,_,_}}= Resp,
	Status.

%{"rebuild" : {"imageId" : 2}}
rebuild(Keys,InstanceId,ImageId)->
	J2 = {struct,[{<<"imageId">>,ImageId}]},
	Js = {struct,[{<<"rebuild">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,post,"/servers/"++ InstanceId ++"/action",Body).

%{"resize" : {"flavorId" : 3}}
resize(Keys,InstanceId,FlavorId)->
	J2 = {struct,[{<<"flavorId">>,FlavorId}]},
	Js = {struct,[{<<"resize">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,post,"/servers/"++ InstanceId ++"/action",Body).

%{"confirmResize" : null}
confirm_resized(Keys,InstanceId)->
	Js = {struct,[{<<"confirmResize">>,null}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,post,"/servers/"++ InstanceId ++"/action",Body).

%{"revertResize" : null}
revert_resized(Keys,InstanceId)->
	Js = {struct,[{<<"revertResize">>,null}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,post,"/servers/"++ InstanceId ++"/action",Body).

%{
%"server" : {
%"name" : "new-server-test","imageId" : 2,"flavorId" : 1,
%"metadata" : {"My Server Name" : "Apache1"},
%"personality" : [{"path" : "/etc/banner.txt",
%"contents" : "ICAgICAgDQoiQSBjbG91ZCBkb2VzIG5vdCBrbm93IHdoeSBpdCBtb3ZlcyBpbiBqdXN0IHN1Y2ggYSBkaXJlY3Rpb24gYW5kIGF0IHN1Y2ggYSBzcGVlZC4uLkl0IGZZWxzIGFuIGltcHVsc2lvbi4uLnRoaXMgaXMgdGhlIHBsYWNlIHRvIGdvIG5vdy4gQnV0IHRoZSBza3kga25vd3MgdGhlIHJlYXNvbnMgYW5kIHRoZSBwYXR0ZXJucyBZWhpbmQgYWxsIGNsb3VkcywgYW5kIHlvdSB3aWxsIGtub3csIHRvbywgd2hlbiB5b3UgbGlmdCB5b3Vyc2VsZiBoaWdoIGVub3VnaCB0byBzZWUgYmV5b25kIGhvcmlb25zLiINCg0KLVJpY2hhcmQgQmFjaA=="
%}]}}
run(Keys,Name,Image,Size)->
	run(Keys,Name,Image,Size,[],[]).

run(Keys,Name,Image,Size,MetaData)->
	run(Keys,Name,Image,Size,MetaData,[]).

run(Keys,Name,Image,Size,MetaData,Files)->
	F = {array,files_to_json(Files)},
	J1={struct,[{<<"name">>,Name},{<<"imageId">>,Image},{<<"flavorId">>,Size},{<<"metadata">>,{struct,MetaData}},
				{<<"personality">>,F}]},
	Js={struct,[{<<"server">>,J1}]},
	Body = iolist_to_binary(mochijson:encode(Js)),
	Resp = rs_util:send_lhttpc_request(Keys,post,"/servers",Body),
	{ok,{_,_,RBody}} = Resp,
	{struct,[{_,{struct,Props}}]} = mochijson:decode(RBody),
	{ok,clean_address(Props)}.

files_to_json([])->[];
files_to_json(Files) when length(Files) >0 ->
	 [{struct,[{<<"path">>,K},{<<"contents">>,base64:encode(V)}]}||{K,V}<-Files].

%list_addresses("12345")
list_addresses(Keys,InstanceId)->
	Resp = rs_util:send_json_request(Keys,get,"/servers/"++ InstanceId ++ "/ips"),
	case Resp of {ok,Body}->
		Body;
    _-> Resp end.

%list_public_addresses("12345")
list_public_addresses(Keys,InstanceId)->
	Resp = rs_util:send_json_request(Keys,get,"/servers/"++ InstanceId ++ "/ips"),
	case Resp of {ok,Body}->
		Body;
    _-> Resp end.

%{"shareIp" : {"sharedIpGroupId" : 1234,"configureServer" : true}}
%share_ip_address("12345", 1234)
share_ip_address(Keys,InstanceId, SharedIpGroupId)->
   	J2 = {struct,[{<<"sharedIpGroupId">>,SharedIpGroupId},{<<"configureServer">>,true}]},
	Js = {struct,[{<<"shareIp">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,put,"/servers/"++InstanceId++"/ips/public/address",Body).

unshare_ip_address(Keys,InstanceId, Address)->
	rs_util:send_request(Keys,delete,"/servers/"++InstanceId++"/ips/public/" ++ Address,[]).

shared_ip_groups(Keys)->
	Response = rs_util:send_request( Keys,get,"/shared_ip_groups",[]),	
	case Response of {ok, Body}->
		{ok,Result,_}=erlsom:simple_form(Body),
    	{_,_,LL} = Result,
		%lists:map(fun(V)->{_,Srv,Mtd}=V,{sharedIpGroup, Srv, [Rest||{_,_,Rest} <- Mtd]} end, LL);
		[{sharedIpGroup, Srv, [Rest||{_,_,Rest} <- Mtd]} || {_,Srv,Mtd} <- LL];
    _-> Response end.

%{"sharedIpGroup" : {"name" : "Shared IP Group 1","server" : 422}}
%create_shared_ip_group("test_share_gp")
create_shared_ip_group(Keys,SharedGpName)->
	J2 = {struct,[{<<"name">>,SharedGpName}]},
	GpJson = {struct,[{<<"sharedIpGroup">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(GpJson)),
	Resp = rs_util:send_lhttpc_request(Keys,post,"/shared_ip_groups",Body),
	{ok,{_,_,RBody}} = Resp,
	RBody.

%String GroupId
terminate_shared_group(Keys,GroupId)->
	rs_util:send_request(Keys,delete,"/shared_ip_groups/" ++ GroupId,[]).

%String InstanceId
list_backup_schedules(Keys,InstanceId)->
	rs_util:send_request(Keys,get,"/servers/"++ InstanceId ++ "/backup_schedule" ,[]).

%/servers/id/backup_schedule{"backupSchedule" : {"enabled" : true,"weekly" : "THURSDAY","daily" : "H_0400_0600"}}
%create_backup_schedule("12345",true,"THURSDAY","H_0400_0600").
create_backup_schedule(Keys,InstanceId,IsEnabled,WeekDay,Hour)->
	J2 = {struct,[{<<"enabled">>,IsEnabled},{<<"weekly">>,WeekDay},{<<"daily">>,Hour}]},
	Js = {struct,[{<<"backupSchedule">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	rs_util:send_lhttpc_request(Keys,post,"/servers/"++InstanceId++"/backup_schedule",Body).

%/servers/id/backup_schedule
%disable_backup_schedule(Keys,"12345").
disable_backup_schedule(Keys,InstanceId)->
	rs_util:send_request(Keys,delete,"/servers/"++InstanceId++"/backup_schedule",[]).

%{"server" :{"name" : "new-server-test","adminPass" : "newPassword"}}
%update_server_name(Keys,"new_name","12345").
update_server_name(Keys,NewName,InstanceId)->
	J2 = {struct,[{<<"name">>,NewName}]},
	Js = {struct,[{<<"server">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	Resp = rs_util:send_lhttpc_request(Keys,put,"/servers/"++ InstanceId ,Body),
	{ok,{Status,_,_}} = Resp, 
    Status.

%update_admin_pwd("gina123","12345").
update_admin_pwd(Keys,NewPwd,InstanceId)->
	J2 = {struct,[{<<"adminPass">>,NewPwd}]},
	Js = {struct,[{<<"server">>,J2}]},
    Body = iolist_to_binary(mochijson:encode(Js)),
	Resp = rs_util:send_lhttpc_request(Keys,put,"/servers/"++ InstanceId ,Body),
	{ok,{Status,_,_}} = Resp, 
    Status.

list_flavors(Keys)->
	Resp = rs_util:send_json_request(Keys,get,"/flavors"),
	case Resp of {ok,Body}->
		{struct,[{_,{array,Props}}]} = mochijson:decode(Body),
		{ok,Props};
    _-> Resp end.

%get_flavor_details("1").
get_flavor_details(Keys,FlavorId)->
	Resp = rs_util:send_json_request(Keys,get,"/flavors/" ++ FlavorId),
	case Resp of {ok,Body}->
		{struct,[{_,{struct,Props}}]} = mochijson:decode(Body),
		{ok,Props};
    _-> Resp end.

get_limits(Keys)->
	Resp = rs_util:send_json_request(Keys,get,"/limits"),
	case Resp of {ok,Body}->
		{struct,[{_,{struct,Props}}]} = mochijson:decode(Body),
		{ok,Props};
    _-> Resp end.

parse_describes_response(Response)->
	case Response of {ok, Body}->
		parse_describes(Body);
    _-> Response end.

parse_describes_response_detail(Response)->
	case Response of {ok, Body}->
		parse_describes_detail(Body);
    _-> Response end.

parse_describes(Body)->
	{ok,{_,_,LL},_}=erlsom:simple_form(Body),
	[Srv|| {_,Srv,_} <- LL].

parse_describes_detail(Body)->
	 {struct, [{_,{_,X}}]} = mochijson:decode(Body),
	 L = [Y||{_,Y}<-X],
	 [clean_address(Z)||Z<-L].

clean_address(Info)->
	{_,Addr} = proplists:get_value("addresses",Info),
    NewT = [{K,V}||{K,{_,[V]}}<-Addr],	    
	lists:keyreplace("addresses",1,Info,{"addresses",NewT}).
    
	
	    
%drop_empty(I) ->
%	length(I) > 0.

%is_address(I)->
%	Pos = string:str(I,"meta"),
%	Pos==0.

