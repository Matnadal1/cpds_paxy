-module(modular_proposer).
-export([start/9]).

-define(default_timeout, 100).
-define(default_backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Timeout, Backoff, SorryCount, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Timeout, Backoff, SorryCount, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Timeout, Backoff, SorryCount, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, Backoff, Round, Proposal, Acceptors, PanelId, Timeout, SorryCount),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId, Timeout, SorryCount) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId, Timeout, SorryCount) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId, Timeout, SorryCount)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId, Timeout, SorryCount) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal, Timeout, 0, length(Acceptors), SorryCount) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, Timeout, 0, length(Acceptors), SorryCount) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0, _, _, Proposal, _, _, _, _ ) ->
  {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal, Timeout, SorryCount, TotalAcceptors, CountSorries) ->
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
          collect(N-1, Round, MaxVoted, Proposal, Timeout, SorryCount, TotalAcceptors, CountSorries);
        {promise, Round, Voted, Value} ->
          case order:gr(Voted, MaxVoted) of  % ({N1,I1}, {N2,I2})
            true ->
              collect(N-1, Round, Voted, Value, Timeout, SorryCount, TotalAcceptors, CountSorries);
            false ->
              collect(N-1, Round, MaxVoted, Proposal, Timeout, SorryCount, TotalAcceptors, CountSorries)
          end;
        {promise, _, _,  _} ->
          collect(N-1, Round, MaxVoted, Proposal, Timeout, SorryCount, TotalAcceptors, CountSorries);
        {sorry, {prepare, Round}} ->
          collect(N, Round, MaxVoted, Proposal, Timeout, SorryCount + CountSorries, TotalAcceptors, CountSorries);
        {sorry, _} ->
          collect(N, Round, MaxVoted, Proposal, Timeout, SorryCount, TotalAcceptors, CountSorries)
      after Timeout ->
        abort
      end
  end.

vote(0, _, _, _, _, _) ->
  ok;
vote(N, Round, Timeout, SorryCount, TotalAcceptors, CountSorries) ->
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
          vote(N-1, Round, Timeout, SorryCount, TotalAcceptors, CountSorries);
        {vote, _} ->
          vote(N-1, Round, Timeout, SorryCount, TotalAcceptors, CountSorries);
        {sorry, {accept, Round}} ->
          vote(N, Round, Timeout, SorryCount + CountSorries, TotalAcceptors, CountSorries);
        {sorry, _} ->
          vote(N, Round, Timeout, SorryCount, TotalAcceptors, CountSorries)
      after Timeout ->
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
