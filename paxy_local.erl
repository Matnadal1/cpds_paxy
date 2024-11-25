-module(paxy_local).
-export([start/1, start/8, stop/0, stop/1, stopAll/0, crash/1]).

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

  start(Sleep) ->
    start(Sleep, 3, 5, 1, 1, 2000, 100, 0).
  start(Sleep, NumProposers, NumAcceptors, Drop, Delay, PropTimeout, PropBackoff, SorryCount) ->
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
  
    % Start the GUI and spawn processes
    register(gui, spawn(fun() -> gui:start(SelectedAcceptorNames, SelectedProposerNames) end)),
    gui ! {reqState, self()},
    receive
      {reqState, State} ->
        {AccIds, PropIds} = State,
        start_acceptors(AccIds, SelectedAccRegister, Drop, Delay),
        spawn(fun() -> 
          Begin = erlang:monotonic_time(),
          start_proposers(PropIds, SelectedPropInfo, SelectedAccRegister, Sleep, PropTimeout, PropBackoff, SorryCount, self()),
          wait_proposers(length(PropIds)),
          End = erlang:monotonic_time(),
          Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
          io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed]),
          io:format("ok.~n")
        end)
    end.
  
    
start_acceptors(AccIds, AccReg, Drop, Delay) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, modular_acceptor:start(RegName, AccId, Drop, Delay)),
      start_acceptors(Rest, RegNameRest, Drop, Delay)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Timeout, Backoff, SorryCount, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      modular_proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Timeout, Backoff, SorryCount, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Timeout, Backoff, SorryCount, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(homer),
  stop(marge),
  stop(bart),
  stop(lisa),
  stop(maggie),
  stop(gui).

stopAll() ->
  stop(gui),
  AccRegister = [
    homer, marge, bart, lisa, maggie, 
    ned, maude, rod, todd, milhouse,
    apu, manjula, carl, lenny, ralph,
    nelson, jimbo, martin, skinner, edna,
    otto, krusty, barney, smithers, burns,
    wiggum, lou, eddie, seymour, gil
  ],

  [stop(Name) || Name <- AccRegister].

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:close(Name),
      pers:delete(Name),
      Pid ! stop
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
 
