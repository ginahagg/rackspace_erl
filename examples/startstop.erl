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

-module(startstop).

-compile(export_all).

run(Keys) ->
	
	case rs_instance:run(Keys,"rs_test2",8,1,[{"server name","rs_test2"},{"my size is","1"}],["/etc/test",atom_to_list(node())]) of
		{ok, Props} ->
			io:format("~n~p~n",[Props]),
			InstanceId = integer_to_list(proplists:get_value("id", Props)),
			case rs_instance:terminate(Keys,InstanceId) of
				{ok, _Result} ->
					io:format("Instance terminated~n");
				{error, Error} ->
					io:format("Oops! Instance still running: ~p~n", [Error])
			end;
		{error, Error} ->
			io:format("Error: ~p~n", [Error]),	
			io:format("Oops! Running instance failed!~n")
	end.

create_image(Keys,InstanceId)->
	case Resp = rs_image:create(Keys,"rs_test_image",InstanceId) of
		{ok, Props} ->
			Props;
		_ ->
			io:format("Error: ~p~n", [Resp]),	
			io:format("Oops! Couldn't create image!~n")
	end.

%/servers/id/backup_schedule{"backupSchedule" : {"enabled" : true,"weekly" : "THURSDAY","daily" : "H_0400_0600"}}
backup_schedule(Keys,InstanceId)->
	IsEnabled = true,
	WeekDay = "THURSDAY",
	Hour="H_0400_0600",
  	rs_instance:create_backup_schedule(Keys,InstanceId,IsEnabled,WeekDay,Hour).
	

