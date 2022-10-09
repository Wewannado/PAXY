-module(acceptor).
-export([start/2]).

%-define(drop, 6).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  pers:open(Name),
  {Promised, Voted, Accepted, StoredPanelId} = pers:read(Name),
  %restore panelID if we came from a crash
  if PanelId == na ->
	%we're recovering from a crash
	io:format("Acceptor ~w recovering from crash~n attaching to panelID ~w",[Name,StoredPanelId]),
	acceptor(Name, Promised, Voted, Accepted, StoredPanelId);
  true ->
    io:format("Initializing acceptor ~w for first time and saving state~n",[Name]),
	pers:store(Name, Promised, Voted, Accepted, PanelId),
	acceptor(Name, Promised, Voted, Accepted, PanelId)
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
			%P = rand:uniform(10),
			%if P =< ?drop ->
			%	io:format("message dropped");	
			%true ->
			%State modified. Save it before notifying destination.
			pers:store(Name, Round, Voted, Value, PanelId),
			Proposer ! {promise, Round, Voted, Value},
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
			%P = rand:uniform(10),
			%if P =< ?drop ->
			%	io:format("message dropped");	
			%true ->
			%State modified. Save it before notifying destination.
			pers:store(Name, Promised, Round, Proposal, PanelId),
			Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
	pers:close(Name),
	pers:delete(Name),
      PanelId ! stop,
	  io:format("Clossing acceptor ~w ~n",[Name]),
      ok
  end.
