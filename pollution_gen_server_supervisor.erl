%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. cze 2019 01:39
%%%-------------------------------------------------------------------
-module(pollution_gen_server_supervisor).
-author("Mateusz").

%% API
-behaviour(supervisor).

-export([start_link/0, cacher/1]).
-export([init/1]).



start_link() ->
  {ok, Pid} = supervisor:start_link(pollution_gen_server_supervisor, []),
  unlink(Pid),
  CacherPid = spawn(pollution_gen_server_supervisor, cacher, [[]]),
  register(pollution_cache, CacherPid).


cacher(State) ->
  receive
    NewState -> cacher(NewState)
  after
    100 ->
      pollution_server ! State,
      cacher(State)
  end.

init(_Args) ->
  SupFlags = #{intensity => 5, % optional
    period => 30},
  ChildSpecs = [#{id => pollution_server,
    start => {pollution_gen_server, start_link, []},
    shutdown => brutal_kill}],
  {ok, {SupFlags, ChildSpecs}}.