#include <nginx.h>
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include <string.h>
#include <zauth.h>
#include <stdbool.h>

// Configuration setup
static void * create_srv_conf (ngx_conf_t *);
static void * create_loc_conf (ngx_conf_t *);
static char * merge_loc_conf  (ngx_conf_t *, void *, void *);
static char * merge_srv_conf  (ngx_conf_t *, void *, void *);
static char * load_keystore   (ngx_conf_t *, ngx_command_t *, void *);
static char * load_acl        (ngx_conf_t *, ngx_command_t *, void *);
static void   delete_srv_conf (void *);

// Module setup
static ngx_int_t zauth_init           (ngx_conf_t *);
static ngx_int_t zauth_parse_request  (ngx_http_request_t *);
static ngx_int_t zauth_handle_request (ngx_http_request_t *);

// Request Inspection
static ZauthResult token_from_header (ngx_str_t const *, ZauthToken **);
static ZauthResult token_from_query  (ngx_str_t const *, ZauthToken **);
static void        delete_token      (void *);

// Variable manipulation
static ngx_int_t zauth_variables      (ngx_conf_t *);
static ngx_int_t zauth_token_var      (ngx_http_request_t *, ngx_http_variable_value_t *, uintptr_t);
static ngx_int_t zauth_token_var_conn (ngx_http_request_t *, ngx_http_variable_value_t *, uintptr_t);
static ngx_int_t zauth_token_var_conv (ngx_http_request_t *, ngx_http_variable_value_t *, uintptr_t);
static ngx_int_t zauth_token_typeinfo (ngx_http_request_t *, ngx_http_variable_value_t *, uintptr_t);
static ngx_int_t zauth_set_var        (ngx_pool_t *, ngx_http_variable_value_t *, Range);
static void      zauth_empty_val      (ngx_http_variable_value_t *);

typedef struct {
        ZauthKeystore * keystore;
        ZauthAcl *      acl;
} ZauthServerConf;

typedef struct {
        ngx_flag_t toggle;
} ZauthLocationConf;

static ngx_http_module_t zauth_module_ctx = {
        zauth_variables // pre-configuration
      , zauth_init      // postconfiguration
      , NULL            // create main configuration
      , NULL            // init main configuration
      , create_srv_conf // create server configuration
      , merge_srv_conf  // merge server configuration
      , create_loc_conf // create location configuration
      , merge_loc_conf  // merge location configuration
};

static ngx_command_t zauth_commands [] = {
        { ngx_string ("zauth")
        , NGX_HTTP_LOC_CONF | NGX_CONF_TAKE1
        , ngx_conf_set_flag_slot
        , NGX_HTTP_LOC_CONF_OFFSET
        , offsetof (ZauthLocationConf, toggle)
        , NULL
        }

      , { ngx_string ("zauth_keystore")
        , NGX_HTTP_SRV_CONF | NGX_CONF_TAKE1
        , load_keystore
        , NGX_HTTP_SRV_CONF_OFFSET
        , 0
        , NULL
        }

      , { ngx_string ("zauth_acl")
        , NGX_HTTP_SRV_CONF | NGX_CONF_TAKE1
        , load_acl
        , NGX_HTTP_SRV_CONF_OFFSET
        , 0
        , NULL
        }

      , ngx_null_command
};

ngx_module_t zauth_module = {
        NGX_MODULE_V1
      , &zauth_module_ctx     // module context
      , zauth_commands        // module directives
      , NGX_HTTP_MODULE       // module type
      , NULL                  // init master
      , NULL                  // init module
      , NULL                  // init process
      , NULL                  // init thread
      , NULL                  // exit thread
      , NULL                  // exit process
      , NULL                  // exit master
      , NGX_MODULE_V1_PADDING
};

// Configuration setup //////////////////////////////////////////////////////

static void * create_srv_conf (ngx_conf_t * conf) {
        ZauthServerConf * c =
                ngx_pcalloc(conf->pool, sizeof(ZauthServerConf));

        if (c == NULL) {
                return NGX_CONF_ERROR;
        }

        c->keystore = NULL;
        c->acl      = NULL;

        ngx_pool_cleanup_t * finaliser =
                ngx_pool_cleanup_add(conf->pool, 0);

        if (finaliser == NULL) {
                return NULL;
        }

        finaliser->handler = delete_srv_conf;
        finaliser->data    = c;

        return c;
}

static char * merge_srv_conf (ngx_conf_t * c, void * pc, void * cc) {
        ZauthServerConf * parent = pc;
        ZauthServerConf * child  = cc;

        if (child->keystore == NULL) {
                child->keystore = parent->keystore;
        }

        if (child->acl == NULL) {
                child->acl = parent->acl;
        }

        if (child->keystore == NULL) {
                ngx_conf_log_error(NGX_LOG_EMERG, c, 0, "missing 'zauth_keystore'");
                return NGX_CONF_ERROR;
        }

        if (child->acl == NULL) {
                ngx_conf_log_error(NGX_LOG_EMERG, c, 0, "missing 'zauth_acl'");
                return NGX_CONF_ERROR;
        }

        return NGX_CONF_OK;
}

static void delete_srv_conf (void * data) {
        ZauthServerConf * c = data;
        if (c->keystore != NULL) {
                zauth_keystore_delete(c->keystore);
        }
        if (c->acl != NULL) {
                zauth_acl_delete(c->acl);
        }
}

static void * create_loc_conf (ngx_conf_t * conf) {
        ZauthLocationConf * lc =
                ngx_pcalloc(conf->pool, sizeof(ZauthLocationConf));

        if (lc == NULL) {
                return NGX_CONF_ERROR;
        }

        lc->toggle = NGX_CONF_UNSET;

        return lc;
}

static char * merge_loc_conf (ngx_conf_t * _, void * pc, void * cc) {
        ZauthLocationConf * parent = pc;
        ZauthLocationConf * child  = cc;
        ngx_conf_merge_off_value(child->toggle, parent->toggle, 1);
        return NGX_CONF_OK;
}

static char * load_keystore (ngx_conf_t * conf, ngx_command_t * cmd, void * data) {
        ZauthServerConf * sc = data;
        if (sc == NULL) {
                return NGX_CONF_ERROR;
        }

        ngx_str_t * const fname = conf->args->elts;
        ZauthResult e = zauth_keystore_open(fname[1].data, fname[1].len, &sc->keystore);

        if (e != ZAUTH_OK || sc->keystore == NULL) {
                ngx_conf_log_error(NGX_LOG_EMERG, conf, 0, "failed to load keystore [%d]", e);
                return NGX_CONF_ERROR;
        }

        return NGX_CONF_OK;
}

static char * load_acl (ngx_conf_t * conf, ngx_command_t * cmd, void * data) {
        ZauthServerConf * sc = data;
        if (sc == NULL) {
                return NGX_CONF_ERROR;
        }

        ngx_str_t * const fname = conf->args->elts;
        ZauthResult e = zauth_acl_open(fname[1].data, fname[1].len, &sc->acl);

        if (e != ZAUTH_OK || sc->acl == NULL) {
                ngx_conf_log_error(NGX_LOG_EMERG, conf, 0, "failed to load acl [%d]", e);
                return NGX_CONF_ERROR;
        }

        return NGX_CONF_OK;
}

// Module setup /////////////////////////////////////////////////////////////

static ngx_int_t zauth_init (ngx_conf_t * conf) {
        ngx_http_core_main_conf_t * mconf =
                ngx_http_conf_get_module_main_conf(conf, ngx_http_core_module);

        ngx_http_handler_pt * h1 =
                ngx_array_push(&mconf->phases[NGX_HTTP_POST_READ_PHASE].handlers);

        if (h1 == NULL) {
                ngx_log_error(NGX_LOG_ERR, conf->log, 0, "no ngx_http_handler_pt");
                return NGX_ERROR;
        }

        *h1 = zauth_parse_request;

        ngx_http_handler_pt * h2 =
                ngx_array_push(&mconf->phases[NGX_HTTP_ACCESS_PHASE].handlers);

        if (h2 == NULL) {
                ngx_log_error(NGX_LOG_ERR, conf->log, 0, "no ngx_http_handler_pt");
                return NGX_ERROR;
        }

        *h2 = zauth_handle_request;

        return NGX_OK;
}

// Request Processing ///////////////////////////////////////////////////////

static ngx_int_t zauth_handle_request (ngx_http_request_t * r) {
        ZauthServerConf const * sc =
                ngx_http_get_module_srv_conf(r, zauth_module);

        if (sc == NULL || sc->keystore == NULL || sc->acl == NULL) {
                return NGX_ERROR;
        }

        ZauthLocationConf const * lc =
                ngx_http_get_module_loc_conf(r, zauth_module);

        if (lc == NULL || lc->toggle != 1) {
                return NGX_DECLINED;
        }

        ZauthToken const * tkn = ngx_http_get_module_ctx(r, zauth_module);

        // internal redirects clear module contexts => try to parse again
        if (tkn == NULL && r->internal) {
                ngx_int_t status = zauth_parse_request(r);
                if (status != NGX_OK) {
                        return status;
                } else {
                        tkn = ngx_http_get_module_ctx(r, zauth_module);
                }
        }

        if (tkn == NULL) {
                return NGX_HTTP_UNAUTHORIZED;
        }

        ZauthResult res = zauth_token_verify(tkn, sc->keystore);

        if (res != ZAUTH_OK) {
                ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0, "unauthorised token [%d]", res);
                return NGX_HTTP_UNAUTHORIZED;
        }

        uint8_t is_allowed = 0;
        res = zauth_token_allowed(tkn, sc->acl, r->uri.data, r->uri.len, &is_allowed);

        if (is_allowed == 0 || res != ZAUTH_OK) {
                ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0, "access denied [%d]", res);
                return NGX_HTTP_FORBIDDEN;
        }

        return NGX_OK;
}

static void delete_token (void * data) {
        zauth_token_delete((ZauthToken *) data);
}

static ngx_int_t zauth_parse_request (ngx_http_request_t * r) {
        ZauthToken* tkn = NULL;
        ZauthResult res = ZAUTH_OK;

        if (r->headers_in.authorization != NULL) {
                res = token_from_header(&r->headers_in.authorization->value, &tkn);
        } else if (r->args.len > 0) {
                ngx_str_t query;
                query.data = ngx_pnalloc(r->pool, r->args.len);
                if (query.data == NULL) {
                        return NGX_ERROR;
                }
                u_char* writer = query.data;
                u_char* reqargs = r->args.data;
                ngx_unescape_uri(&writer, &reqargs, r->args.len, 0);
                query.len = writer - query.data;
                res = token_from_query(&query, &tkn);
        } else {
                ngx_str_t name   = ngx_string("zprovider");
                ngx_str_t cookie = ngx_null_string;
                ngx_int_t index  = ngx_http_parse_multi_header_lines(&r->headers_in.cookies, &name, &cookie);
                if (index != NGX_DECLINED) {
                        res = zauth_token_parse(cookie.data, cookie.len, &tkn);
                }
        }

        if (res == ZAUTH_OK && tkn != NULL) {
                ngx_pool_cleanup_t * finaliser = ngx_pool_cleanup_add(r->pool, 0);
                if (finaliser == NULL) {
                        return NGX_ERROR;
                }
                finaliser->handler = delete_token;
                finaliser->data    = tkn;
                ngx_http_set_ctx(r, tkn, zauth_module);
                return NGX_OK;
        }

        if (res != ZAUTH_OK) {
                ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0, "failed to parse token [%d]", res);
        }

        return NGX_OK;
}

static ZauthResult token_from_header (ngx_str_t const * hdr, ZauthToken ** t) {
        if (strncmp((char const *) hdr->data, "Bearer ", 7) == 0) {
                return zauth_token_parse(&hdr->data[7], hdr->len - 7, t);
        } else {
                return ZAUTH_PARSE_ERROR;
        }
}

static ZauthResult token_from_query (ngx_str_t const * query, ZauthToken ** t) {
        uint8_t const * start = memmem(query->data, query->len, "access_token=", 13);

        if (start == NULL) {
                return ZAUTH_PARSE_ERROR;
        }

        uint8_t const * token_start = start + 13; // length of "access_token="
        size_t          token_len   = query->len - (token_start - query->data);
        uint8_t const * token_end   = memchr(token_start, '&', token_len);

        return token_end == NULL
                ? zauth_token_parse(token_start, token_len, t)
                : zauth_token_parse(token_start, token_end - token_start, t);
}

// Variables ////////////////////////////////////////////////////////////////

static ngx_int_t zauth_variables (ngx_conf_t * conf) {
        ngx_str_t z_type_id = ngx_string("zauth_type");
        ngx_str_t z_prov_id = ngx_string("zauth_provider");
        ngx_str_t z_bot_id  = ngx_string("zauth_bot");
        ngx_str_t z_user_id = ngx_string("zauth_user");
        ngx_str_t z_conn_id = ngx_string("zauth_connection");
        ngx_str_t z_conv_id = ngx_string("zauth_conversation");

        ngx_http_variable_t * z_type_var =
                ngx_http_add_variable(conf, &z_type_id, NGX_HTTP_VAR_NOHASH);

        ngx_http_variable_t * z_prov_var =
                ngx_http_add_variable(conf, &z_prov_id, NGX_HTTP_VAR_NOHASH);

        ngx_http_variable_t * z_bot_var =
                ngx_http_add_variable(conf, &z_bot_id, NGX_HTTP_VAR_NOHASH);

        ngx_http_variable_t * z_user_var =
                ngx_http_add_variable(conf, &z_user_id, NGX_HTTP_VAR_NOHASH);

        ngx_http_variable_t * z_conn_var =
                ngx_http_add_variable(conf, &z_conn_id, NGX_HTTP_VAR_NOHASH);

        ngx_http_variable_t * z_conv_var =
                ngx_http_add_variable(conf, &z_conv_id, NGX_HTTP_VAR_NOHASH);

        if ( z_type_var == NULL || z_prov_var == NULL || z_bot_var == NULL ||
             z_user_var == NULL || z_conn_var == NULL || z_conv_var == NULL )
        {
                return NGX_ERROR;
        }

        z_type_var->get_handler = zauth_token_typeinfo;
        z_bot_var->get_handler  = zauth_token_var;
        z_bot_var->data         = 'b';
        z_user_var->get_handler = zauth_token_var;
        z_user_var->data        = 'u';
        z_prov_var->get_handler = zauth_token_var;
        z_prov_var->data        = 'p';
        z_conn_var->get_handler = zauth_token_var_conn;
        z_conv_var->get_handler = zauth_token_var_conv;

        return NGX_OK;
}

static ngx_int_t zauth_token_typeinfo (ngx_http_request_t * r, ngx_http_variable_value_t * v, uintptr_t _) {
        ZauthToken const * t = ngx_http_get_module_ctx(r, zauth_module);
        if (t == NULL) {
                return NGX_ERROR;
        }
        switch (zauth_token_type(t)) {
                case ZAUTH_TOKEN_TYPE_BOT: {
                        Range range = { (u_char*) "bot", 3 };
                        return zauth_set_var(r->pool, v, range);
                }
                case ZAUTH_TOKEN_TYPE_ACCESS: {
                        Range range = {(u_char*)  "access", 6 };
                        return zauth_set_var(r->pool, v, range);
                }
                case ZAUTH_TOKEN_TYPE_USER: {
                        Range range = { (u_char*) "user", 4 };
                        return zauth_set_var(r->pool, v, range);
                }
                case ZAUTH_TOKEN_TYPE_LEGAL_HOLD_ACCESS: {
                        Range range = {(u_char*)  "legal_hold_access", 9 };
                        return zauth_set_var(r->pool, v, range);
                }
                case ZAUTH_TOKEN_TYPE_LEGAL_HOLD_USER: {
                        Range range = { (u_char*) "legal_hold_user", 10 };
                        return zauth_set_var(r->pool, v, range);
                }
                case ZAUTH_TOKEN_TYPE_PROVIDER: {
                        Range range = { (u_char*) "provider", 8 };
                        return zauth_set_var(r->pool, v, range);
                }
                default: {
                        Range range = { (u_char*) "unknown", 7 };
                        return zauth_set_var(r->pool, v, range);
                }
        }
}

static ngx_int_t zauth_token_var (ngx_http_request_t * r, ngx_http_variable_value_t * v, uintptr_t data) {
        ZauthToken const * t = ngx_http_get_module_ctx(r, zauth_module);
        if (t == NULL) {
                return NGX_ERROR;
        }
        return zauth_set_var(r->pool, v, zauth_token_lookup(t, data));
}

static ngx_int_t zauth_token_var_conn (ngx_http_request_t * r, ngx_http_variable_value_t * v, uintptr_t _) {
        ZauthToken const * t = ngx_http_get_module_ctx(r, zauth_module);
        if (t == NULL) {
                return NGX_ERROR;
        }
        if (zauth_token_type(t) == ZAUTH_TOKEN_TYPE_ACCESS || zauth_token_type(t) == ZAUTH_TOKEN_TYPE_LEGAL_HOLD_ACCESS) {
                return zauth_set_var(r->pool, v, zauth_token_lookup(t, 'c'));
        } else {
                zauth_empty_val(v);
                return NGX_OK;
        }
}

static ngx_int_t zauth_token_var_conv (ngx_http_request_t * r, ngx_http_variable_value_t * v, uintptr_t _) {
        ZauthToken const * t = ngx_http_get_module_ctx(r, zauth_module);
        if (t == NULL) {
                return NGX_ERROR;
        }
        if (zauth_token_type(t) == ZAUTH_TOKEN_TYPE_BOT) {
                return zauth_set_var(r->pool, v, zauth_token_lookup(t, 'c'));
        } else {
                zauth_empty_val(v);
                return NGX_OK;
        }
}

static void zauth_empty_val (ngx_http_variable_value_t * v) {
        v->len          = 0;
        v->valid        = 0;
        v->no_cacheable = 1;
        v->not_found    = 1;
        v->data         = NULL;
}

static ngx_int_t zauth_set_var (ngx_pool_t * pool, ngx_http_variable_value_t * v, Range r) {
        if (r.len == 0 || r.ptr == NULL) {
                zauth_empty_val(v);
                return NGX_OK;
        }

        u_char * value = ngx_pnalloc(pool, r.len);

        if (value == NULL) {
                return NGX_ERROR;
        }

        ngx_memcpy(value, r.ptr, r.len);

        v->len          = r.len;
        v->valid        = 0;
        v->no_cacheable = 1;
        v->not_found    = 0;
        v->data         = value;

        return NGX_OK;
}
