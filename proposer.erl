-module(proposer).
-export([start/6]).

-define(timeout, 1000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal, Acceptors) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, Acceptors) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(N, Round, MaxVoted, Proposal, Acceptors) ->
  collect(N, Round, MaxVoted, Proposal, 0, length(Acceptors)).

collect(0, _, _, Proposal, _, _) ->
  {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal, SorryCount, TotalAcceptors) ->
  QuorumNeeded = (TotalAcceptors div 2) + 1,
  % If we've received too many sorry messages to reach quorum, abort
  case SorryCount > (TotalAcceptors - QuorumNeeded) of
    true -> 
      io:format("[Proposer] Aborting - received ~w sorry messages, can't reach quorum~n", 
               [SorryCount]),
      abort;
    false ->
      receive 
        {promise, Round, _, na} ->
          collect(N-1, Round, MaxVoted, Proposal, SorryCount, TotalAcceptors);
        {promise, Round, Voted, Value} ->
          case order:gr(Voted, MaxVoted) of
            true ->
              collect(N-1, Round, Voted, Value, SorryCount, TotalAcceptors);
            false ->
              collect(N-1, Round, MaxVoted, Proposal, SorryCount, TotalAcceptors)
          end;
        {promise, _, _, _} ->
          collect(N-1, Round, MaxVoted, Proposal, SorryCount, TotalAcceptors);
        {sorry, {prepare, Round}} ->
          collect(N, Round, MaxVoted, Proposal, SorryCount + 1, TotalAcceptors);
        {sorry, _} ->
          collect(N, Round, MaxVoted, Proposal, SorryCount, TotalAcceptors)
      after ?timeout ->
        abort
      end
  end.

vote(N, Round, Acceptors) ->
  vote(N, Round, 0, length(Acceptors)).

vote(0, _, _, _) ->
  ok;
vote(N, Round, SorryCount, TotalAcceptors) ->
  QuorumNeeded = (TotalAcceptors div 2) + 1,
  % If we've received too many sorry messages to reach quorum, abort
  case SorryCount > (TotalAcceptors - QuorumNeeded) of
    true -> 
      io:format("[Proposer] Aborting vote phase - received ~w sorry messages, can't reach quorum~n", 
               [SorryCount]),
      abort;
    false ->
      receive
        {vote, Round} ->
          vote(N-1, Round, SorryCount, TotalAcceptors);
        {vote, _} ->
          vote(N-1, Round, SorryCount, TotalAcceptors);
        {sorry, {accept, Round}} ->
          vote(N, Round, SorryCount + 1, TotalAcceptors);
        {sorry, _} ->
          vote(N, Round, SorryCount, TotalAcceptors)
      after ?timeout ->
        abort
      end
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
