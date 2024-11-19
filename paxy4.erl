-module(paxy4).
-export([start/1, stop/0, stop/1, run_proposers/4, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(BLACK, {0,0,0}).
-define(GREY, {150,150,150}).
-define(ORANGE, {255, 165, 0}).
-define(PURPLE, {128, 0, 128}).
-define(YELLOW, {255,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      % local
      %  spawn(fun() -> 
      %   Begin = erlang:monotonic_time(),
      %   start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
      %   wait_proposers(length(PropIds)),
      %   End = erlang:monotonic_time(),
      %   Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
      %   io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      % end)
      % remote
      spawn('paxy-pro@127.0.0.1', paxy, run_proposers,[PropIds, PropInfo, lists:map(fun(X)->{X,'paxy-acc@127.0.0.1'} end, AccRegister), Sleep])
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      % remote
      spawn('paxy-acc@127.0.0.1', fun() -> register(RegName, acceptor:start(RegName, AccId)) end),
      % local
      % register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

run_proposers(PropIds, PropInfo, AccRegister, Sleep)->
  Begin = erlang:monotonic_time(),
  start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
  wait_proposers(length(PropIds)),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed]).

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  spawn('paxy-acc@127.0.0.1',paxy,stop,[homer]),
  spawn('paxy-acc@127.0.0.1',paxy,stop,[marge]),
  spawn('paxy-acc@127.0.0.1',paxy,stop,[bart]),
  spawn('paxy-acc@127.0.0.1',paxy,stop,[lisa]),
  spawn('paxy-acc@127.0.0.1',paxy,stop,[maggie]),
  % stop(homer),
  % stop(marge),
  % stop(bart),
  % stop(lisa),
  % stop(maggie),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop,
      pers:close(Name),
      pers:delete(Name),
      unregister(Name)
  end.

crash(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      unregister(Name),
      exit(Pid, "crash"),
      pers:open(Name),
      {_, _, _, Pn} = pers:read(Name),
      Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
      pers:close(Name),
      timer:sleep(3000),
      register(Name, acceptor:start(Name, na))
end.