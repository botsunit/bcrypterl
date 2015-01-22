-module(bcrypt_engine).
-export([
         hash_secret/2,
         generate_salt/0,
         generate_salt/1,
         valid_salt/1,
         calibrate/1,
         autodetect_cost/1
        ]).

-define(DEFAULT_COST, 10).
-define(MIN_COST, 4).
-define(MAX_SALT_LENGTH, 16).

% @doc
% Given a secret and a valid salt (see bcrypt_engine:generate_salt/1) calculates
% a bcrypt() password hash.
% @end
-spec hash_secret(string(), string()) -> string() | {error, any()}.
hash_secret(Secret, Salt) ->
  case valid_salt(Salt) of
    false -> {error, invalid_salt};
    true -> bcrypterl_nif:bc_crypt(Secret, Salt)
  end.

% @equiv generate_salt(DEFAULT_COST)
-spec generate_salt() -> string() | {error, any()}.
generate_salt() ->
  generate_salt(?DEFAULT_COST).
% @doc
% Generates a random salt with a given computational cost.
% @end
-spec generate_salt(integer()) -> string() | {error, any()}.
generate_salt(Cost) ->
  Cost1 = max(Cost, ?MIN_COST),
  Prefix = "$2a$05$CCCCCCCCCCCCCCCCCCCCC.E5YPO9kmyuRGyh0XouQYb4YMJKvyOeW",
  bcrypterl_nif:bc_salt(Prefix, Cost1, binary_to_list(crypto:strong_rand_bytes(?MAX_SALT_LENGTH))).

% @doc
% Returns true if <tt>Salt</tt> is a valid bcrypt() salt, false if not.
% @end
-spec valid_salt(string()) -> true | false.
valid_salt(Salt) ->
  case re:run(Salt, "^\\$[0-9a-z]{2,}\\$[0-9]{2,}\\$[A-Za-z0-9\\.\\/]{22,}$") of
    {match, _} -> true;
    nomatch -> false
  end.

% @doc
% Returns the cost factor which will result in computation times less than +upper_time_limit_in_ms+.
% @end
-spec autodetect_cost(string()) -> integer() | {error, invalid_salt}.
autodetect_cost(Salt) ->
  case valid_salt(Salt) of
    false -> {error, invalid_salt};
    true -> list_to_integer(string:substr(Salt, 5, 2))
  end.

% @doc
% Autodetects the cost from the salt string.
% @end
-spec calibrate(integer()) -> integer().
calibrate(Limit) ->
  Res = [0 | lists:takewhile(fun(I) ->
                  {MS, _} = timer:tc(bcrypt_password, create, ["testing testing", [{cost, I}]]),
                  (MS * 0.001) < Limit
              end, lists:seq(1, 40))],
  [Result|_] = lists:reverse(Res),
  Result.
