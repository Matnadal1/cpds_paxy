-module(paxy_remote).
-export([start/1, start/7, start_acceptors/4, run_proposers/6, stop/0, stop/1, stop_acceptors/0, stop_proposers/0, stopAll/0, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(PINK, {255,192,203}).
-define(ORANGE, {255,165,0}).
-define(PURPLE, {128,0,128}).
-define(CYAN, {46,242,219}).
-define(GRAY, {60,60,60}).
-define(BROWN, {140,106,86}).
-define(SKY, {135,206,235}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  start(Sleep, 3, 5, 1, 1, 2000, 100).
start(Sleep, NumProposers, NumAcceptors, Drop, Delay, PropTimeout, PropBackoff) ->
  % Define lists of names and corresponding data
  AcceptorNames = [
      "Homer", "Marge", "Bart", "Lisa", "Maggie", 
      "Ned", "Maude", "Rod", "Todd", "Milhouse",
      "Apu", "Manjula", "Carl", "Lenny", "Ralph",
      "Nelson", "Jimbo", "Martin", "Skinner", "Edna",
      "Otto", "Krusty", "Barney", "Smithers", "Burns",
      "Wiggum", "Lou", "Eddie", "Seymour", "Gil"
    ],
  AccRegister = [
      homer, marge, bart, lisa, maggie, 
      ned, maude, rod, todd, milhouse,
      apu, manjula, carl, lenny, ralph,
      nelson, jimbo, martin, skinner, edna,
      otto, krusty, barney, smithers, burns,
      wiggum, lou, eddie, seymour, gil
    ],
    
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE},
    {"Amy", ?YELLOW}, {"Zoidberg", ?ORANGE}, {"Professor", ?PINK},
    {"Hermes", ?PURPLE}, {"Scruffy", ?CYAN}, {"Nibbler", ?BROWN},
    {"Kif", ?GRAY}, {"Zapp", ?SKY}],
    
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE},
    {amy, ?YELLOW}, {zoidberg, ?ORANGE}, {professor, ?PINK},
    {hermes, ?PURPLE}, {scruffy, ?CYAN}, {nibbler, ?BROWN},
    {kif, ?GRAY}, {zapp, ?SKY}],

  % Ensure inputs are within bounds
  MaxAcceptors = length(AcceptorNames),
  MaxProposers = length(ProposerNames),
  NumAcceptors1 = min(NumAcceptors, MaxAcceptors),
  NumProposers1 = min(NumProposers, MaxProposers),
  
  % Select the required number of acceptors and proposers
  SelectedAcceptorNames = lists:sublist(AcceptorNames, NumAcceptors1),
  SelectedAccRegister = lists:sublist(AccRegister, NumAcceptors1),
  SelectedProposerNames = lists:sublist(ProposerNames, NumProposers1),
  SelectedPropInfo = lists:sublist(PropInfo, NumProposers1),

  QualifiedAccRegister = [{Name, 'paxy-acc@127.0.0.1'} || Name <- SelectedAccRegister],
  QualifiedProRegisters = [{Name, 'paxy-pro@127.0.0.1'} || Name <- SelectedPropInfo],

  % Start GUI on the local node
  register(gui, spawn(fun() -> gui:start(SelectedAcceptorNames, SelectedProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      
      % Spawn acceptors on the remote 'paxy-acc' node
      spawn('paxy-acc@127.0.0.1', paxy3, start_acceptors, [AccIds, QualifiedAccRegister, Drop, Delay]),
      % Wait for all proposers to finish
      spawn('paxy-pro@127.0.0.1', paxy3, run_proposers, [PropIds, QualifiedProRegisters, QualifiedAccRegister, Sleep, PropTimeout, PropBackoff])
  end.

start_acceptors(AccIds, AccReg, Drop, Delay) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [{RegName, _}|RegNameRest] = AccReg,
      io:format("[RegName ~w] [AccId ~w] [Drop ~w] [Delay ~w]~n",
                 [RegName, AccId, Drop, Delay]),
      register(RegName, modular_acceptor:start(RegName, AccId, Drop, Delay)),
      start_acceptors(Rest, RegNameRest, Drop, Delay)
  end.

run_proposers(PropIds, PropInfo, Acceptors, Sleep, Timeout, Backoff) ->
    Begin = erlang:monotonic_time(),
    % Spawn proposers on the remote 'paxy-pro' node
    start_proposers(PropIds, PropInfo, Acceptors, Sleep, Timeout, Backoff, self()),
    wait_proposers(length(PropIds)),
    End = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
    io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed]).

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Timeout, Backoff, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{{RegName, Colour}, _}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      modular_proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Timeout, Backoff, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Timeout, Backoff, Main)
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
  AccRegister = [
    homer, marge, bart, lisa, maggie, 
    ned, maude, rod, todd, milhouse,
    apu, manjula, carl, lenny, ralph
  ],
  [stop(Name) || Name <- AccRegister].

% Stop all proposers on paxy-pro
stop_proposers() ->
  stop(fry),
  stop(bender),
  stop(leela),
  stop(amy),
  stop(zoidberg),
  stop(professor),
  stop(hermes),
  stop(scruffy),
  stop(nibbler),
  stop(kif),
  stop(zapp).

stopAll() ->
  stop(gui),

  % Stop acceptors on paxy-acc
  spawn('paxy-acc@127.0.0.1', paxy3, stop_acceptors, []),
  % Stop proposers on paxy-pro
  spawn('paxy-pro@127.0.0.1', paxy3, stop_proposers, []).

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
