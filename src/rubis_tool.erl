-module(rubis_tool).

-include("rubis.hrl").

-export([random_num/2, get_key/2, now_nsec/0, non_uniform_random/4, 
        create_item/10, create_bid/6, create_user/5, create_comment/6, create_buy_now/4, 
        load_transition/0, get_next_state/3, get_think_time/2, translate_op/1]).

get_next_state(PreviousStates, Dict, CurrentState) ->
    {_, T} = dict:fetch(CurrentState, Dict),
    Num = random:uniform(),
    %lager:info("T is ~p", [T]),
    S = find_next_state(T, Num, 0.0, 0, CurrentState),
    End = length(T),
    Previous = End,
    case S of
        Previous -> %% Go back to previous state
            case PreviousStates of [] ->  {[], home};
                                  [P1|P2] -> {P2, P1}
            end;
        End -> %% Terminate session
            {[], home}; 
        CurrentState ->
            {PreviousStates, CurrentState};
        _ ->
            case dict:fetch(S, Dict) of
                {back, _} -> {[CurrentState|PreviousStates], S};
                {not_back, _} -> {[], S} 
            end
    end.

find_next_state([], _Num, _ProbAcc, _Acc, Current) ->
    Current;
find_next_state([H|T], Num, ProbAcc, Acc, Current) ->
    case ProbAcc > Num of
        true -> translate_op(Acc);
        false -> find_next_state(T, Num, ProbAcc+H, Acc+1, Current)
    end.

get_think_time({_, OpTag}, T) ->
    ThinkTime = dict:fetch({sleep, OpTag}, T),
    %lager:info("In op ~p, going to think for ~w", [translate_op(OpTag), ThinkTime]),
    %ThinkTime div 8.
    ThinkTime.
    %0.

load_transition() ->
    FileName = basho_bench_config:get(transition_file),
    %NumCols = 27,
    NumRows = 29,
    {ok, Device} = file:open(FileName, [read]),
    io:get_line(Device, ""),
    io:get_line(Device, ""),
    io:get_line(Device, ""),
    _L = droplast(io:get_line(Device, "")),
    Dict = get_all_lines(Device, dict:new(), NumRows, 1),
    %lager:info("Info ~p", [dict:to_list(Dict)]),
    file:close(Device),
    {Transit, Sleep} = filter_dict(Dict),
    ND0 = lists:foldl(fun({Num, L}, D) -> 
               Sum = lists:sum(L),
               NewList = lists:foldl(fun(Elem, ProbList) -> [(Elem/Sum)|ProbList] end, [], L),
               dict:store(Num, lists:reverse(NewList), D)
                end, dict:new(), Transit),

    ND = dict:fold(fun(K, V, D) ->
            case lists:nth(NumRows-1, V) of
                0 -> dict:store(K, {not_back, V}, D);
                _ -> dict:store(K, {back, V}, D)
            end end, dict:new(), ND0),
    SleepDict = dict:from_list(Sleep),
    MergedDict = dict:merge(fun(_Key, V1, V2) -> V1 ++ V2 end, ND, SleepDict),
    dict:fold(fun(K, V, NewDict) ->
                case K of {sleep, Key} -> dict:store({sleep, translate_op(Key)}, V, NewDict);
                           Key -> dict:store(translate_op(Key), V, NewDict)
                end end, dict:new(), MergedDict).
    %lager:info("Merge Dict is ~w", [dict:to_list(MergeD)]),
    %MergeD.

filter_dict(Dict) ->
    dict:fold(fun(K, V, {L1, L2}) -> 
                case K of {sleep, _} -> {L1, [{K, V}|L2]};
                           _ -> {[{K, V}|L1], L2}
                end
              end, {[], []}, Dict).

get_all_lines(Device, Dict, NumRows, LineNum) when NumRows == LineNum ->
    case io:get_line(Device, "") of
        Line0 -> 
            %lager:info("Last line is ~p", [Line0]),
            Line = droplast(Line0),
            [_CurrentGo|Splitted0] = re:split(Line, "\\t", [{return, list}]),            
            Splitted = droplast(Splitted0),
            SleepTime = lists:last(Splitted0),
            %lager:info("Splitted ~p", [Splitted0]),
            {_, ND} = lists:foldl(fun(V, {Acc, D}) -> 
                    case V of
                        "0" -> {Acc+1, D}; 
                        _ -> List = dict:fetch(Acc, D),
                             [First|Rest] = List,
                             {Acc+1, dict:store(Acc, [(First+list_to_float(V))|Rest], D)}
                    end end, {1, Dict}, Splitted),
            dict:store({sleep, LineNum}, list_to_integer(SleepTime), ND)
    end;
get_all_lines(Device, Dict, NumRows, LineNum) ->
    case io:get_line(Device, "") of
        eof -> Dict;
        Line0 -> 
            %lager:info("Org line is ~p", [Line0]),
            Line = droplast(Line0),
            %lager:info("Dropped line is ~p", [Line]),
            [_CurrentGo|Splitted0] = re:split(Line, "\\t", [{return, list}]),            
            Splitted = droplast(Splitted0),
            SleepTime = lists:last(Splitted0),
            %lager:info("Sleep time is ~p", [SleepTime]),
            {_, ND} = lists:foldl(fun(V, {Acc, D}) -> 
                    %Header = lists:nth(Acc, Headers),
                    case V of
                        "0" -> {Acc+1, dict:append(Acc, 0, D)};
                        _ -> {Acc+1, dict:append(Acc, list_to_float(V), D)}
                    end end, {1, Dict}, Splitted),
            ND1 = dict:store({sleep, LineNum}, list_to_integer(SleepTime), ND),
            get_all_lines(Device, ND1, NumRows, LineNum+1)
    end.

non_uniform_random(Type, X, Min, Max) ->
    A = random_num(0, X),
    B = random_num(Min, Max),
    Bor = A bor B,
    (( Bor + Type) rem (Max - Min + 1)) + Min.

random_num(Start, End) ->
    random:uniform(End-Start+1) + Start - 1.

create_item(Name, _CommentFinalLength, InitialPrice, Quantity, ReservePrice, BuyNow, StartDate, EndDate, UserId, CategoryId) ->
    Description = "Don't buy it!!!",
    %DescriptionLength=1339,
    %StaticDescription = "This incredible item is exactly what you need !<br>It has a lot of very nice features including a coffee option.<p>It comes with a free license for the free RUBiS software, thats really cool. But RUBiS even if it is free, is <B>(C) Rice University/INRIA 2001</B>. It is really hard to write an interesting generic description for automatically generated items, but who will really read this ?<p>You can also check some cool software available on http://sci-serv.inrialpes.fr. There is a very cool DSM system called SciFS for SCI clusters, but you will need some SCI adapters to be able to run it ! Else you can still try CART, the amazing Cluster Administration and Reservation Tool. All those software are open source, so don't hesitate ! If you have a SCI Cluster you can also try the Whoops! clustered web server. Actually Whoops! stands for something ! Yes, it is a Web cache with tcp Handoff, On the fly cOmpression, parallel Pull-based lru for Sci clusters !! Ok, that was a lot of fun but now it is starting to be quite late and I'll have to go to bed very soon, so I think if you need more information, just go on <h1>http://sci-serv.inrialpes.fr</h1> or you can even try http://www.cs.rice.edu and try to find where Emmanuel Cecchet or Julie Marguerite are and you will maybe get fresh news about all that !!<p>",
    
    %CommentPartList = comment_part_list(DescriptionLength, random:uniform(CommentFinalLength)),
    %Description = lists:foldl(fun(Len, Acc) ->
    %                            Acc ++ lists:sublist(StaticDescription, Len)
    %                        end, [], CommentPartList), 
    
    #item{i_name=Name, i_description = Description, i_quantity = Quantity, i_init_price=InitialPrice, 
            i_reserve_price = ReservePrice, i_buy_now = BuyNow, i_nb_of_bids = 0, i_max_bid = 0, i_start_date = StartDate, 
            i_end_date = EndDate, i_seller = UserId, i_category = CategoryId}. 

%comment_part_list(DescriptionLength, TotalLength) when DescriptionLength < TotalLength->
%    [DescriptionLength|comment_part_list(DescriptionLength, TotalLength-DescriptionLength)];
%comment_part_list(_DescriptionLength, TotalLength)->
%    [TotalLength].

create_bid(UserId, ItemId, Qty, Bid, MaxBid, Now) ->
    #bid{b_user_id=UserId, b_item_id=ItemId, b_qty=Qty, b_bid=Bid, b_max_bid=MaxBid, b_date=Now}.

create_buy_now(BuyerId, ItemId, Qty, Now) ->
    #buy_now{bn_buyer_id=BuyerId, bn_item_id=ItemId, bn_qty=Qty, bn_date=Now}.

create_user(Index, Now, Rating, Balance, RegionId) ->
    %ListIndex = integer_to_list(Index),
    %FirstName = "Great" ++ ListIndex, 
    %LastName = "User"++ ListIndex,
    %NickName = "User"++ ListIndex,
    %Email = FirstName++"."++LastName++"@rubis.com",
    %Password = "password"++ ListIndex,
    FirstName = Index,
    LastName = Index,
    NickName = Index,
    Password = Index,
    Email = Index,

    #user{u_firstname=FirstName, u_lastname=LastName, u_nickname=NickName, u_password=Password, u_comment_nodes=[],
            u_email=Email, u_rating=Rating, u_creation_date=Now, u_balance=Balance, u_region=RegionId, u_num_comments=0, 
            u_bids= [], u_sellings = [], u_bought=[]}.

create_comment(FromId, ToId, _CommentFinalLength, ItemId, Rating, Now) ->
    Comment = "Very bad",
    %Comments = ["This is a very bad comment. Stay away from this seller !!<p>",
    %                           "This is a comment below average. I don't recommend this user !!<p>",
    %                           "This is a neutral comment. It is neither a good or a bad seller !!<p>",
    %                           "This is a comment above average. You can trust this seller even if it is not the best deal !!<p>",
    %                           "This is an excellent comment. You can make really great deals with this seller !!<p>"],
    %CommentToUse = lists:nth(Rating+3, Comments),
    %CommentPartList = comment_part_list(length(CommentToUse), random:uniform(CommentFinalLength)),
    %Comment = lists:foldl(fun(Len, Acc) ->
    %                            Acc ++ lists:sublist(CommentToUse, Len)
    %                        end, [], CommentPartList),
    #comment{c_item_id=ItemId, c_from_id=FromId, c_to_id=ToId, c_rating=Rating, c_date=Now, c_comment=Comment}.


get_key({{P0, P1}, P2}, Type) ->
    integer_to_list(P0)++"_"++integer_to_list(P1)++"_"++integer_to_list(P2)++type_to_str(Type);
get_key({P1, P2}, Type) ->
    integer_to_list(P1)++"_"++integer_to_list(P2)++type_to_str(Type);
get_key(P, Type) ->
    integer_to_list(P)++type_to_str(Type).

type_to_str(Type) ->
    case Type of
        item -> "_item";
        region -> "_region";
        category -> "_category";
        comment -> "_comment";
        user -> "_user";
        bid -> "_bid";
        buy_now -> "_buynow";
        regionnewitems -> "_regionnewitems";
        categorynewitems -> "_categorynewitems";
        lastbuynow -> "_lastbuynow";
        lastbid -> "_lastbid";
        lastitem -> "_lastitem"
    end.

%%%%%% private funs %%%%%%%%%%%

now_nsec() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

droplast(L)->
    droplast(L, []).

droplast([_], Acc) ->
    lists:reverse(Acc);
droplast([X|H], Acc) ->
    droplast(H, [X|Acc]).

translate_op(Op) ->
    case Op of
        1 ->    home;
        2 -> register;
        3 -> register_user;
        4 -> browse;
        5 -> browse_categories;
        6 -> search_items_in_category;
        7 -> browse_regions;
        8 -> browse_categories_in_region;
        9 -> search_items_in_region;
        10 -> view_item;
        11 -> view_user_info;
        12 -> view_bid_history;
        13 -> buy_now_auth;
        14 -> buy_now;
        15 -> store_buy_now;
        16 -> put_bid_auth;
        17 -> put_bid;
        18 -> store_bid;
        19 -> put_comment_auth;
        20 -> put_comment;
        21 -> store_comment;
        22 -> sell;
        23 -> select_category_to_sell_item;
        24 -> sell_item_form;
        25 -> register_item;
        26 -> about_me_auth;
        27 -> about_me;
        _ -> Op
    end.

