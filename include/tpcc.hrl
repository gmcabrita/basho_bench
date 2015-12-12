-define(A_C_LAST, 255).
-define(A_C_ID, 1023).
-define(A_OL_I_ID, 8191).
-define(NB_MAX_ITEM, 10000).
%-define(NB_MAX_CUSTOMER, 100).
%-define(NB_MAX_ORDER, 100).
-define(NB_MAX_CUSTOMER, 100).
-define(NB_MAX_ORDER, 100).
-define(NB_MAX_DISTRICT, 10).
-define(S_DATA_MINN, 26).
-define(S_DATA_MAXN, 50).
-define(WAREHOUSE_YTD, 300000).
-define(MIN_C_LAST, 0).
-define(MAX_C_LAST, 999).
-define(LIMIT_ORDER, 2101).
-define(MIN_ITEM, 5).
-define(MAX_ITEM, 15).
-define(NUM_NAMES, 2).
-define(NAMES, ["BAR","OUGHT"]).
%% I added that!! (To prevent metadata growing..)
-define(MAX_NEW_ORDER, 4000).

-record(item, {
    i_id :: non_neg_integer(),
    i_im_id :: non_neg_integer(),
    i_name :: [],
    i_price :: float(),
    i_data :: []}).

-record(warehouse, {
    w_id :: integer(),
    w_name :: [],
    w_street1 :: [], 
    w_street2 :: [], 
    w_city :: [],
    w_state :: [],
    w_zip :: [],
    w_tax :: float()}).

-record(stock, {
    s_i_id :: integer(), 
    s_w_id :: integer(),
    s_quantity :: integer(), 
    s_dist_01 :: [],
    s_dist_02 :: [],
    s_dist_03 :: [],
    s_dist_04 :: [],
    s_dist_05 :: [],
    s_dist_06 :: [],
    s_dist_07 :: [],
    s_dist_08 :: [],
    s_dist_09 :: [],
    s_dist_10 :: [],
    s_ytd :: integer(),
    s_order_cnt :: integer(), 
    s_remote_cnt :: integer(), 
    s_data :: []}). 

-record(district, {
    d_id :: integer(),
    d_w_id :: integer(),
    d_name :: [],
    d_street1 :: [],
    d_street2 :: [],
    d_city :: [],
    d_state :: [],
    d_zip :: [],
    d_tax :: float(),
    d_next_o_id :: integer()}).

-record(customer, {
    c_d_id :: integer(),
    c_w_id :: integer(),
    c_id :: integer(),
    c_first :: [],
    c_middle :: [],
    c_last :: [],
    c_street1 :: [],
    c_street2 :: [],
    c_city :: [],
    c_state :: [],
    c_zip :: [],
    c_phone :: [],
    c_since :: integer(),
    c_credit :: [],
    c_credit_lim :: float(),
    c_discount :: float(),
    c_ytd_payment :: float(),
    c_payment_cnt :: integer(),
    c_delivery_cnt :: integer(),
    c_data :: []}).

-record(customer_lookup, {
    c_w_id :: integer(),
    c_d_id :: integer(),
    c_last :: [],
    ids :: [integer()]}).

-record(history, {
    h_c_id :: integer(),
    h_c_d_id :: integer(),
    h_c_w_id :: integer(),
    h_d_id :: integer(),
    h_w_id :: integer(),
    h_date :: integer(),
    h_amount :: float(),
    h_data :: []}). 

-record(order, {
    o_id :: integer(),
    o_d_id :: integer(),
    o_w_id :: integer(),
    o_c_id :: integer(),
    o_entry_d :: integer(),
    o_carrier_id :: integer(),
    o_ol_cnt :: integer(),
    o_all_local :: integer()}).

-record(orderline, {
    ol_o_id :: integer(),
    ol_d_id :: integer(),
    ol_w_id :: integer(),
    ol_number :: integer(),
    ol_i_id :: integer(),
    ol_supply_w_id :: integer(),
    ol_delivery_d :: integer(),
    ol_quantity :: integer(),
    ol_amount :: float(),
    ol_dist_info :: []}).

-record(neworder, {
    no_o_id :: integer(),
    no_w_id :: integer(),
    no_d_id :: integer()}).
