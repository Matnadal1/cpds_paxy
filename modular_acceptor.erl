-module(modular_acceptor).
-export([start/4]).

start(Name, PanelId, Drop, Delay) ->
  spawn(fun() -> init(Name, PanelId, Drop, Delay) end).
        
init(Name, PanelId, Drop, Delay) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId, Drop, Delay).

acceptor(Name, Promised, Voted, Value, PanelId, Drop, Delay) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          P = rand:uniform(100),
          if P =< Drop ->
              io:format("message dropped~n");
            true ->
            %send message
              T = rand:uniform(Delay),
              timer:send_after(T, Proposer, {promise, Round, Voted, Value}),
              %Proposer ! {promise, Round, Voted, Value}, % ... !  {promise, Round, Voted, Value}
              io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
            % Update gui
              Colour = case Value of na -> {0,0,0}; _ -> Value end,
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour}
          end,
          acceptor(Name, Round, Voted, Value, PanelId, Drop, Delay);
        false ->
          timer:send_after(delay, Proposer, {sorry, {prepare, Round}}),
          % Proposer ! {sorry, {prepare, Round}}, % {sorry, {prepare, Round}}
          acceptor(Name, Promised, Voted, Value, PanelId, Drop, Delay)
      end;
    {accept, Proposer, Round, Proposal} ->  % Proposal = {{N, I}, Value}
      case order:goe(Round, Promised) of
        true ->
          T = rand:uniform(Delay),
          timer:send_after(T, Proposer, {vote, Round}),
          % Proposer ! {vote, Round}, % {vote, Round}
          case order:goe(Round, Voted) of % 
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId, Drop, Delay);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId, Drop, Delay)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId, Drop, Delay)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.