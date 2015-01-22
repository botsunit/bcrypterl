#include "erl_nif.h"
#include "nif_helpers.h"
#include "ow-crypt.h"

NIF(bcrypterl_nif_bc_salt) {
  char *salt = NULL;
  ERL_NIF_TERM result = enif_make_badarg(env);

  unsigned int length;
  char *prefix = NULL;
  unsigned int count;
  char *input = NULL;

  VALIDATE_ARITY(2);

  GET_ARG_STRING(0, prefix, length, enif_make_atom(env, "invalid_prefix"), result, TERMINATE);
  GET_ARG_UINT(1, count, enif_make_atom(env, "invalid_count"), result, TERMINATE);

  length = 0;
  if(argc == 3) {
    GET_ARG_STRING(2, input, length, enif_make_atom(env, "invalid_input"), result, TERMINATE);
  }

  salt = crypt_gensalt_ra(prefix, count, input, length);
  if(!salt) {
    result = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "salt_error"));
    goto TERMINATE;
  }
  result = enif_make_string(env, salt, ERL_NIF_LATIN1);

TERMINATE:
  FREE(prefix);
  FREE(input);
  FREE(salt);
  return result;
}

NIF(bcrypterl_nif_bc_crypt) {
  char *value = NULL;
  void *data = NULL;
  int size = 0xDEADBEEF;
  ERL_NIF_TERM result = enif_make_badarg(env);

  unsigned int length;
  char *key = NULL;
  char *setting = NULL;

  VALIDATE_ARITY(2);

  GET_ARG_STRING(0, key, length, enif_make_atom(env, "invalid_key"), result, TERMINATE);
  GET_ARG_STRING(1, setting, length, enif_make_atom(env, "invalid_setting"), result, TERMINATE);

  value = crypt_ra(key, setting, &data, &size);
  if(!value) {
    result = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "crypt_error"));
    goto TERMINATE;
  }

  result = enif_make_string_len(env, data, size - 1, ERL_NIF_LATIN1);

TERMINATE:
  FREE(key);
  FREE(setting);
  FREE(data);

  return result;
}


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static ErlNifFunc nif_funcs[] = {
  {"bc_salt", 3, bcrypterl_nif_bc_salt},
  {"bc_salt", 2, bcrypterl_nif_bc_salt},
  {"bc_crypt", 2, bcrypterl_nif_bc_crypt}
};

ERL_NIF_INIT(bcrypterl_nif, nif_funcs, &on_load, NULL, NULL, NULL);
