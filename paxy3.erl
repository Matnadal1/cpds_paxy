-module(paxy3).
-export([start/1, start_acceptors/2, run_proposers/4, stop/0, stop/1, stop_acceptors/0, stop_proposers/0, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(PINK, {255,192,203}).
-define(ORANGE, {255,165,0}).
-define(PURPLE, {128,0,128}).
-define(CYAN, {46, 242, 219}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie", "Burns", "Apu", "Ned"],
  AccRegister = [homer, marge, bart, lisa, maggie, burns, apu, ned],
  QualifiedAccRegister = [{Name, 'paxy-acc@127.0.0.1'} || Name <- AccRegister],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE},
    {"Amy", ?YELLOW}, {"Zoidberg", ?ORANGE}, {"Professor", ?PINK},
    {"Hermes", ?PURPLE}, {"Scruffy", ?CYAN}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE},
    {amy, ?YELLOW}, {zoidberg, ?ORANGE}, {professor, ?PINK},
    {hermes, ?PURPLE}, {scruffy, ?CYAN}],

  QualifiedProRegisters = [{Name, 'paxy-pro@127.0.0.1'} || Name <- PropInfo],

  % Start GUI on the local node
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      
      % Spawn acceptors on the remote 'paxy-acc' node
      spawn('paxy-acc@127.0.0.1', paxy3, start_acceptors, [AccIds, QualifiedAccRegister]),
      % Wait for all proposers to finish
      spawn('paxy-pro@127.0.0.1', paxy3, run_proposers, [PropIds, QualifiedProRegisters, QualifiedAccRegister, Sleep])
  end.

start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [{RegName, _}|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

run_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    Begin = erlang:monotonic_time(),
    % Spawn proposers on the remote 'paxy-pro' node
    start_proposers(PropIds, PropInfo, Acceptors, Sleep, self()),
    wait_proposers(length(PropIds)),
    End = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
    io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed]).

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{{RegName, Colour}, _}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.


% Wait for all proposers to finish
wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

% Stop all processes on both nodes
stop() ->
  % Stop acceptors on paxy-acc
  spawn('paxy-acc@127.0.0.1', paxy3, stop_acceptors, []),
  % Stop proposers on paxy-pro
  spawn('paxy-pro@127.0.0.1', paxy3, stop_proposers, []),
  % Stop local GUI process
  stop(gui),
  stop_proposers(),
  stop_acceptors().

% Stop a specific process by name
stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:close(Name),
      pers:delete(Name),
      Pid ! stop
  end.

% Stop all acceptors on paxy-acc
stop_acceptors() ->
  stop(homer),
  stop(marge),
  stop(bart),
  stop(lisa),
  stop(maggie),
  stop(burns),
  stop(apu),
  stop(ned).

% Stop all proposers on paxy-pro
stop_proposers() ->
  stop(fry),
  stop(bender),
  stop(leela),
  stop(amy),
  stop(zoidberg),
  stop(professor),
  stop(hermes),
  stop(scruffy).

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
