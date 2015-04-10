-module(temp).

-export([neBi/1,
	 create_biased_key_function/2]).

neBi(Id) ->
    MaxKey = 4,
    ReplicationFactor = 2,
    PercentageExternal = 0.0,
    NumDcs = 4,
    NodesPerDc = 2,
    Nodes = 8,
    
    


    NodeId = Id rem Nodes +1,
    io:format("NodeId ~w~n",[NodeId]),
    IdDc = ((NodeId - 1) div NodesPerDc) +1,
    io:format("IdDc ~w~n",[IdDc]),
    KeySpace = MaxKey div NumDcs,
    io:format("KeySpace ~w~n",[KeySpace]),
    RangeHere = ReplicationFactor,
    MinHere = IdDc,
    MinNotHere = (IdDc + ReplicationFactor) rem (NumDcs+1),
    io:format("MinNotHere ~w~n",[MinNotHere]),
    RangeNotHere = NumDcs - ReplicationFactor,
    io:format("RangeNotHere ~w~n",[RangeNotHere]),
    fun() -> DcNum = case random:uniform() > PercentageExternal of
			 false ->
			     Val = case (MinNotHere + (random:uniform(RangeNotHere)-1)) rem (NumDcs) of
				       0 ->
					   NumDcs;
				       Other ->
					   Other
				   end,
			     io:format("DcNum: ~w~n", [Val]),
			     Val;
			 true ->
			     case (MinHere + (random:uniform(RangeHere)-1)) rem (NumDcs) of
				 0 ->
				     NumDcs;
				 Other ->
				     Other
			     end
		     end,
	     (((random:uniform(KeySpace)-1) * NumDcs) + DcNum)
    end.



% Should return a list where each value is a sigle element tuple with the Dc number
create_biased_key_function(ReplicationFactor,NumDcs) ->
    fun(Key) ->
	    FirstDc = case Key rem NumDcs of
			  0 ->
			      NumDcs;
			  Else ->
			      Else
		      end,
	    ListFun = fun(Self,Count,Acc) ->
			      case Count of
				  ReplicationFactor ->
				      Acc;
				  _ ->
				      case (FirstDc + Count) rem NumDcs of
					  0 ->
					      Self(Self,Count + 1,Acc ++ [{NumDcs}]);
					  Other ->
					      Self(Self,Count+1,Acc ++ [{Other}])
				      end
			      end
		      end,
	    ListFun(ListFun,1,[{FirstDc}])
    end.
    
