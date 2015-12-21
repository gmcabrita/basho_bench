-module(tpcc_tool).

-include("tpcc.hrl").

-export([random_num/2, get_key/1, c_last/1, now_nsec/0, non_uniform_random/4, last_name/1,
        create_item/1, create_warehouse/1, create_stock/2, create_district/2,
        create_customer/4, create_customer_lookup/3, create_history/3, create_history/8, 
		create_order/6, create_order/7, create_orderline/6, create_orderline/9, 
		create_neworder/3, get_key_by_param/2]).

last_name(Num) ->
    lists:nth((Num div 100) rem ?NUM_NAMES +1, ?NAMES) ++
	lists:nth((Num div 10) rem ?NUM_NAMES +1, ?NAMES) ++ 
	lists:nth(Num rem ?NUM_NAMES +1, ?NAMES).

get_ol_i_id(DcId) ->
    case ets:lookup(list_to_atom(integer_to_list(DcId)), pop_ol_i_id) of
        [] ->
            Rand = random_num(0, ?A_OL_I_ID), 
            ets:insert(list_to_atom(integer_to_list(DcId)), {pop_ol_i_id, Rand}),
            Rand;
        [{pop_ol_i_id, V}] ->
            V
    end.

get_c_last(DcId) ->
    case ets:lookup(list_to_atom(integer_to_list(DcId)), pop_c_last) of
        [] ->
            Rand = random_num(?MIN_C_LAST, ?A_C_LAST), 
            ets:insert(list_to_atom(integer_to_list(DcId)), {pop_c_last, Rand}),
            Rand;
        [{pop_c_last, V}] ->
            V
    end.

c_last(DcId) ->
    Number = non_uniform_random(get_c_last(DcId), ?A_C_LAST, ?MIN_C_LAST, ?MAX_C_LAST),
	Alea = integer_to_list(Number),
	NumZeros = 3 - length(Alea),
	NewAlea = case NumZeros > 0 of true -> lists:duplicate(NumZeros, 1) ++ Alea;
				  				  false -> Alea
			  end,
	lists:foldl(fun(N, Acc) ->
				Acc ++ lists:nth(N rem ?NUM_NAMES +1, ?NAMES)
				end, [],  NewAlea).


non_uniform_random(Type, X, Min, Max) ->
    A = random_num(0, X),
    B = random_num(Min, Max),
    Bor = A bor B,
    (( Bor + Type) rem (Max - Min + 1)) + Min.

random_num(Start, End) ->
    random:uniform(End-Start+1) + Start - 1.

random_len_string(Min, Max) ->
    Len = random_num(Min, Max),
    random_string(Len).

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

random_float(Min, Max, P) ->
    Pow = math:pow(10, P),
    AMin = Pow*Min,
    AMax = Pow*Max,
    R = random:uniform()*(AMax-AMin)+AMin,
    R / Pow.

random_data() ->
    Alea = random_len_string(?S_DATA_MINN, ?S_DATA_MAXN),
    case random_num(1, 10) of
        1 ->
            L = length(Alea),
            Number = random_num(0, L - 8),
            lists:sublist(Alea, 1, Number+1) ++ "ORIGINAL" ++ lists:sublist(Alea, Number + 9, L);
        _ ->
            Alea
    end.

create_item(ItemId) ->
    #item{i_id=ItemId, i_im_id=random_num(1, 10000), i_name=random_len_string(14,24), 
            i_price=random_float(1, 100, 2), i_data=random_data()}.

create_warehouse(WarehouseId) ->
    #warehouse{w_id=WarehouseId, w_name=random_len_string(6, 10), w_street1=random_len_string(10, 20),
    w_street2=random_len_string(10, 20), w_city=random_len_string(10, 20), w_state=random_string(2),
    w_zip = random_string(4) ++ "11111", w_tax=random_float(0.0, 0.2, 4)}. 

create_stock(StockId, WarehouseId) ->
    #stock{s_i_id=StockId, s_w_id=WarehouseId, s_quantity=random_num(10,100),
           s_dist_01=random_string(24), s_dist_02=random_string(24), s_dist_03=random_string(24),
           s_dist_04=random_string(24), s_dist_05=random_string(24), s_dist_06=random_string(24),
           s_dist_07=random_string(24), s_dist_08=random_string(24), s_dist_09=random_string(24),
           s_dist_10=random_string(24), s_ytd=0, s_order_cnt=0, s_remote_cnt=0, s_data=random_data()}.

create_district(DistrictId, WarehouseId) ->
    #district{d_id=DistrictId, d_w_id=WarehouseId, d_name=random_len_string(6, 10), d_street1=random_len_string(10,20),
                d_street2=random_len_string(10,20), d_city=random_len_string(10,20), d_state=random_string(2),
                d_zip=random_string(4) ++ "11111", d_tax=random_float(0.0, 0.2, 4), d_next_o_id=3001}.

create_customer(WarehouseId, DistrictId, CustomerId, CLast) ->
    #customer{
    c_d_id=DistrictId,
    c_w_id=WarehouseId,
    c_id=CustomerId,
    c_first=random_len_string(8, 16),
    c_middle="OE", 
    c_last=CLast, 
    c_street1=random_len_string(10, 20), 
    c_street2=random_len_string(10, 20), 
    c_city=random_len_string(10, 20), 
    c_state=random_string(2),
    c_zip=random_string(4)++"11111",
    c_phone=random_string(16),
    c_since=now_nsec(),
    c_credit=set_credit(),
    c_credit_lim =500000.0,
    c_discount=random_float(0.0, 0.5, 4),
    c_ytd_payment=10.0,
    c_payment_cnt=1,
    c_delivery_cnt=0,
    c_data=random_len_string(300,500)}.

create_customer_lookup(WarehouseId, DistrictId, CLast) ->
    #customer_lookup{c_w_id=WarehouseId, c_d_id=DistrictId, c_last=CLast, ids=[]}.

create_history(WarehouseId, DistrictId, CustomerId) ->
    #history{h_c_id=CustomerId, h_c_d_id=DistrictId, h_c_w_id=WarehouseId, h_d_id=DistrictId, h_w_id=WarehouseId, h_date=now_nsec(),
        h_amount=10, h_data=random_len_string(12, 24)}.

create_history(TWId, TDId, CWId, CDId, CId, Date, Payment, HData) ->
    #history{h_c_id=CId, h_c_d_id=CDId, h_c_w_id=CWId, h_d_id=TDId, h_w_id=TWId, h_date=Date,
        h_amount=Payment, h_data=HData}.

create_order(WarehouseId, DistrictId, OrderId, OOlCnt, CustomerId, Time) ->
    #order{o_id=OrderId, o_d_id=DistrictId, o_w_id=WarehouseId, o_entry_d=Time, o_ol_cnt=OOlCnt,
            o_c_id=CustomerId, o_carrier_id=get_carrier_id(OrderId), o_all_local=1}.

create_order(WarehouseId, DistrictId, OrderId, OOlCnt, CustomerId, Time, AllLocal) ->
    #order{o_id=OrderId, o_d_id=DistrictId, o_w_id=WarehouseId, o_entry_d=Time, o_ol_cnt=OOlCnt,
            o_c_id=CustomerId, o_carrier_id=get_carrier_id(OrderId), o_all_local=AllLocal}.

create_orderline(WarehouseId, DistrictId, OrderId, OrderlineId, DDate, Amount) ->
    #orderline{ol_o_id=OrderId, ol_w_id=WarehouseId, ol_d_id=DistrictId, 
            ol_number=OrderlineId, ol_i_id=non_uniform_random(get_ol_i_id(WarehouseId), ?A_OL_I_ID, 1, ?NB_MAX_ITEM),
            ol_supply_w_id=WarehouseId, ol_delivery_d=DDate, ol_quantity=5, ol_amount=Amount,ol_dist_info=random_len_string(12,24)
            }.

create_orderline(WarehouseId, DistrictId, SupplyWId, OrderId, OlIId, OlNumber, OlQuantity, Amount, OlDistInfo) ->
    #orderline{ol_o_id=OrderId, ol_w_id=WarehouseId, ol_d_id=DistrictId, 
            ol_number=OlNumber, ol_i_id=OlIId, ol_supply_w_id=SupplyWId, 
            ol_delivery_d=now_nsec(), ol_quantity=OlQuantity, ol_amount=Amount, ol_dist_info=OlDistInfo
            }.

create_neworder(WarehouseId, DistrictId, OrderId) ->
    #neworder{no_o_id = OrderId, no_d_id = DistrictId, no_w_id = WarehouseId}.

get_key_by_param(Param, Type) ->
    case Type of
        customer ->
            {WarehouseId, DistrictId, CustomerId} = Param,
            "CUSTOMER_"++integer_to_list(WarehouseId)++"_"++integer_to_list(DistrictId)
                ++"_"++integer_to_list(CustomerId);
        warehouse ->
            {WarehouseId} = Param,
            "WAREHOUSE_"++ integer_to_list(WarehouseId);
        district ->
            {WarehouseId, DistrictId} = Param,
            "DISTRICT_"++integer_to_list(WarehouseId)++"_"++integer_to_list(DistrictId);
        neworder ->
            {WarehouseId, DistrictId, OId} = Param,
            "NEWORDER_" ++ integer_to_list(WarehouseId) ++ "_" ++ integer_to_list(DistrictId)
                ++ "_" ++ integer_to_list(OId);
        order ->
            {WarehouseId, DistrictId, OId} = Param,
            "ORDER_" ++ integer_to_list(WarehouseId) ++ "_" ++ integer_to_list(DistrictId) ++
                "_" ++ integer_to_list(OId);
        item ->
            {ItemId} = Param,
            "ITEM_"++ integer_to_list(ItemId);
        stock ->
            {WarehouseId, ItemId} = Param,
            "STOCK_"++integer_to_list(WarehouseId)++"_"++integer_to_list(ItemId);
		customer_lookup ->
			{WarehouseId, DistrictId, LastName} = Param,
            "CUSTOMER_LOOKUP_"++LastName++"_"++
            integer_to_list(WarehouseId)++"_"++integer_to_list(DistrictId);
		orderline ->
			{OWId, ODId, OlOId, OlNumber} = Param,
            "ORDERLINE_" ++ integer_to_list(OWId) ++ "_" ++ integer_to_list(ODId) ++ 
                "_" ++ integer_to_list(OlOId) ++ "_" ++ integer_to_list(OlNumber)
    end.

get_key(Obj) ->
    case Obj of
        #item{} ->
            "ITEM_"++ integer_to_list(Obj#item.i_id);
        #warehouse{} ->
            "WAREHOUSE_"++ integer_to_list(Obj#warehouse.w_id);
        #stock{} ->
            "STOCK_"++integer_to_list(Obj#stock.s_w_id)++"_"++integer_to_list(Obj#stock.s_i_id);
        #district{} ->
            "DISTRICT_"++integer_to_list(Obj#district.d_w_id)++"_"++integer_to_list(Obj#district.d_id);
        #customer{} ->
            "CUSTOMER_"++integer_to_list(Obj#customer.c_w_id)++"_"++integer_to_list(Obj#customer.c_d_id)
                ++"_"++integer_to_list(Obj#customer.c_id);
        #customer_lookup{} ->
            "CUSTOMER_LOOKUP_"++Obj#customer_lookup.c_last++"_"++
            integer_to_list(Obj#customer_lookup.c_w_id)++"_"++integer_to_list(Obj#customer_lookup.c_d_id);
        #history{} ->
            "HISTORY_"++integer_to_list(Obj#history.h_c_id);
        #order{} ->
            "ORDER_" ++ integer_to_list(Obj#order.o_w_id) ++ "_" ++ integer_to_list(Obj#order.o_d_id) ++ 
                "_" ++ integer_to_list(Obj#order.o_id);
        #orderline{} ->
            "ORDERLINE_" ++ integer_to_list(Obj#orderline.ol_w_id) ++ "_" ++ integer_to_list(Obj#orderline.ol_d_id) ++ 
                "_" ++ integer_to_list(Obj#orderline.ol_o_id) ++ "_" ++ integer_to_list(Obj#orderline.ol_number);
        #neworder{} ->
            "NEWORDER_" ++ integer_to_list(Obj#neworder.no_w_id) ++ "_" ++ integer_to_list(Obj#neworder.no_d_id)
                ++ "_" ++ integer_to_list(Obj#neworder.no_o_id)
    end.

%%%%%% private funs %%%%%%%%%%%

set_credit() ->
    case random_num(1, 10) of
        1 ->
            "BC";
        _ ->
            "GC"
    end.

now_nsec() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

get_carrier_id(OrderId) ->
    case OrderId < ?LIMIT_ORDER of
        true ->
            random_num(1,10);
        false ->
            0
    end.
