-module(bcrypt_password).

-export([
         create/1,
         create/2,
         valid_hash/1,
         split_hash/1,
         is/2
        ]).

% @equiv create(Secret, [])
-spec create(string()) -> string() | {error, any()}.
create(Secret) ->
  create(Secret, []).
% @doc
% Hashes a secret, returning a hash. Takes an optional <tt>cost</tt> option, which is a
% logarithmic variable which determines how computational expensive the hash is to
% calculate (a <tt>cost</tt> of 4 is twice as much work as a <tt>cost</tt> of 3). 
% The higher the <tt>cost</tt> the harder it becomes for attackers to try to guess 
% passwords (even if a copy of your database is stolen), but the slower it is to check
% users' passwords.
% @end
-spec create(string(), [] | [{cost, integer()}]) -> string() | {error, any()}.
create(Secret, Options) ->
  case lists:keyfind(cost, 1, Options) of
    {cost, Cost} -> bcrypt_engine:hash_secret(Secret, bcrypt_engine:generate_salt(Cost));
    _ -> bcrypt_engine:hash_secret(Secret, bcrypt_engine:generate_salt())
  end.

% @doc
% Returns true if the given hash is a valid hash.
% @end
-spec valid_hash(string()) -> true | false.
valid_hash(H) ->
  case re:run(H, "^\\$[0-9a-z]{2}\\$[0-9]{2}\\$[A-Za-z0-9\\.\\/]{53}$") of
    {match, _} -> true;
    nomatch -> false
  end.

% @doc
% Splits the given hash into version, cost, salt, and checksum and returns them in a list of tuples.
% @end
-spec split_hash(string()) -> list().
split_hash(H) ->
  [V, C, Mash] = string:tokens(H, "$"),
  [{version, V}, 
   {cost, list_to_integer(C)},
   {salt, string:substr(H, 1, 29)},
   {checksum, string:substr(Mash, length(Mash) -30, 31)}].

% @doc
% Compares a potential secret against the hash. Returns true if the secret is the original secret, false otherwise.
% @end
-spec is(string(), string()) -> true | false.
is(H, Password) ->
  case lists:keyfind(salt, 1, split_hash(H)) of
    {salt, Salt} -> bcrypt_engine:hash_secret(Password, Salt) =:= H;
    _ -> false
  end.
