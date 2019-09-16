#ifndef __ZAUTH_H_INCLUDED__
#define __ZAUTH_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

typedef struct {
        uint8_t const * ptr;
        size_t          len;
} Range;

typedef enum {
        ZAUTH_OK              = 0,
        ZAUTH_BASE64_ERROR    = 1,
        ZAUTH_EXPIRED         = 2,
        ZAUTH_INVALID_ATTR    = 3,
        ZAUTH_IO_ERROR        = 4,
        ZAUTH_MISSING_ATTR    = 5,
        ZAUTH_NULL_ARG        = 6,
        ZAUTH_PARSE_ERROR     = 7,
        ZAUTH_SIGNATURE_MISS  = 8,
        ZAUTH_UNKNOWN_KEY     = 9,
        ZAUTH_UTF8_ERROR      = 10,
        ZAUTH_ACL_ERROR       = 11,
        ZAUTH_PANIC           = 99
} ZauthResult;

typedef enum {
        ZAUTH_TOKEN_TYPE_USER               = 0,
        ZAUTH_TOKEN_TYPE_BOT                = 1,
        ZAUTH_TOKEN_TYPE_ACCESS             = 2,
        ZAUTH_TOKEN_TYPE_UNKNOWN            = 3,
        ZAUTH_TOKEN_TYPE_PROVIDER           = 4,
        ZAUTH_TOKEN_TYPE_LEGAL_HOLD_USER    = 5,
        ZAUTH_TOKEN_TYPE_LEGAL_HOLD_ACCESS  = 6,
} ZauthTokenType;

typedef struct ZauthAcl      ZauthAcl;
typedef struct ZauthKeystore ZauthKeystore;
typedef struct ZauthToken    ZauthToken;

ZauthResult zauth_keystore_open(uint8_t const * fname, size_t len, ZauthKeystore **);
void        zauth_keystore_delete(ZauthKeystore * store);

ZauthResult zauth_acl_open(uint8_t const * fname, size_t len, ZauthAcl **);
void        zauth_acl_delete(ZauthAcl * store);

ZauthResult    zauth_token_parse(uint8_t const * str, size_t len, ZauthToken **);
ZauthResult    zauth_token_verify(ZauthToken const *, ZauthKeystore const *);
ZauthTokenType zauth_token_type(ZauthToken const *);
long           zauth_token_time(ZauthToken const *);
uint8_t        zauth_token_version(ZauthToken const *);
Range          zauth_token_lookup(ZauthToken const *, uint8_t);
ZauthResult    zauth_token_allowed(ZauthToken const *, ZauthAcl const *, uint8_t const * path, size_t len, uint8_t * result);
void           zauth_token_delete(ZauthToken *);

#ifdef __cplusplus
}
#endif

#endif // __ZAUTH_H_INCLUDED__
