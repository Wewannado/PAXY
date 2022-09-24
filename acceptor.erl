-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Promised,{Round,Proposer}) of
        true ->
          Proposer ! {promise, {Round,Proposer}, Voted, Value},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Proposer, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round,Proposer]), Colour},
          acceptor(Name, [Round,Proposer], Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Promised, Proposal) of
        true ->
          Proposer ! {vote, Proposal},
          case order:goe(..., ...) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, ..., ...]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [...]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), ...},
              acceptor(Name, Promised, ..., ..., PanelId);
            false ->
              acceptor(Name, Promised, ..., ..., PanelId)
          end;                            
        false ->
          ... ! {sorry, {accept, ...}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
