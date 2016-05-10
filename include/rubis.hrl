-define(BID_NUM, 10).
-define(SELLING_NUM, 10).
-define(COMMENT_NUM, 10).
-define(BOUGHT_NUM, 10).
-define(DURATION, 10000).

-define(CATEGORY_NEW_ITEMS, 20).
-define(REGION_NEW_ITEMS, 20).

-record(item, {
    %i_id :: {integer(), integer()},
    i_name :: [],
    i_description :: [],
    i_quantity :: non_neg_integer(),
    i_init_price :: float(),
    i_reserve_price :: float(),
    i_buy_now :: float(),
    i_nb_of_bids :: non_neg_integer(),
    i_max_bid :: float(),
    i_start_date :: integer(),
    i_end_date :: integer(),
    i_seller :: {integer(), integer()},
    i_category :: integer(),
    %%%%%% Additional data
    i_bid_ids = [] :: [{integer(), integer()}]
    }).

-record(bid, {
    %b_id :: {integer(), integer()},
    b_user_id :: {integer(), integer()}, 
    b_item_id :: {integer(), integer()},
    b_qty :: integer(),
    b_bid :: float(),
    b_max_bid :: float(),
    b_date :: integer()}).

-record(comment, {
    c_from_id :: {integer(), integer()},
    c_to_id :: {integer(), integer()}, 
    c_item_id :: {integer(), integer()},
    c_rating :: float(),
    c_date :: integer(),
    c_comment :: []}).

-record(user, {
    %u_id :: {integer(), integer()},
    u_firstname :: [], 
    u_lastname :: [],
    u_nickname :: [],
    u_password :: [],
    u_email :: [],
    u_rating :: integer(),
    u_balance :: float(),
    u_creation_date :: integer(),
    u_region :: integer(),
    %%% Additional data
    u_num_comments :: non_neg_integer(),
    u_bids :: [{integer(), integer()}],
    u_sellings :: [{integer(), integer()}],
    u_bought :: [{integer(), integer()}]}).

-record(buy_now, {
    bn_buyer_id :: {integer(), integer()},
    bn_item_id :: {integer(), integer()},
    bn_qty :: integer(),
    bn_date :: integer()}).


-record(category, {
    c_id :: integer(),
    c_name :: []}).

-record(region, {
    r_id :: integer(),
    r_name :: []}).


%%%%%%%%%%%%%%%% Additional datastructures
