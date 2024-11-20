-module(acceptor).
-export([start/2]).
% -export([start/2, init/2, acceptor/5]).


-define(delay, 500).
-define(drop, 1).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  pers:open(Name),
  %Promised = order:null(), 
  %Voted = order:null(),
  %Value = na,
  {Promised, Voted, Value, _} = pers:read(Name),  % Read stored state
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          P = rand:uniform(10),
          if P =< ?drop ->
              io:format("message dropped~n");
            true ->
            %send message
              T = rand:uniform(?delay),
              timer:send_after(T, Proposer, {promise, Round, Voted, Value}),
              %Proposer ! {promise, Round, Voted, Value}, % ... !  {promise, Round, Voted, Value}
              io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
            % Update gui
              Colour = case Value of na -> {0,0,0}; _ -> Value end,
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour}

              % pers:store(Name, Round, Voted, Value, PanelId)
          end,
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          T = rand:uniform(?delay),
          timer:send_after(T, Proposer, {sorry, {prepare, Round}}),
          % Proposer ! {sorry, {prepare, Round}}, % {sorry, {prepare, Round}}
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->  % Proposal = {{N, I}, Value}
      case order:goe(Round, Promised) of
        true ->
          T = rand:uniform(?delay),
          timer:send_after(T, Proposer, {vote, Round}),
          % Proposer ! {vote, Round}, % {vote, Round}
          case order:goe(Round, Voted) of % 
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              % pers:store(Name, Promised, Round, Proposal, PanelId),
              
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.