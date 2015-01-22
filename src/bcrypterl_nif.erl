% @hidden
-module(bcrypterl_nif).

-export([
         bc_salt/2, 
         bc_salt/3,
         bc_crypt/2
        ]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
  erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
  PrivDir = case code:priv_dir(?MODULE) of
              {error, bad_name} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
              Path ->
                Path
            end,
  erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

bc_salt(_Prefix, _Count, _Input) ->
  ?nif_stub.
bc_salt(_Prefix, _Count) ->
  ?nif_stub.

bc_crypt(_Key, _Setting) ->
  ?nif_stub.
