/**
 * @file zrest.c Zeta REST-based authentication
 *
 * Copyright (c) 2018 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <string.h>
#include <time.h>
#include <re.h>
#include <re_sha.h>
#include <restund.h>
#include <openssl/hmac.h>
#include <openssl/err.h>


/*
 * This module implements a REST-based authentication mechanism
 * using ephemeral (i.e. time-limited) credentials.
 *
 * A shared secret must be configured in the config file, and can then
 * be shared with a HTTP REST-based service.
 *
 * Format:
 *
 *     username = <timestamp>.s.<random string>
 *     password = HMAC_SHA512(secret, username)
 */


static struct {
	char secret[256];
	size_t secret_len;

	struct http_sock *http_sock;  /* optional */
} zrest;


#if 0
static void generate_username_v0(char *user, size_t sz, uint32_t ttl)
{
	char x[42];
	time_t now = time(NULL);

	rand_str(x, sizeof(x));

	re_snprintf(user, sz,
		    "%llu.s.%s", (uint64_t)(now + ttl), x);
}
#endif


static void generate_username_v1(char *user, size_t sz, uint32_t ttl)
{
	char x[42];
	time_t now = time(NULL);

	rand_str(x, sizeof(x));

	re_snprintf(user, sz,
		    "d=%llu.v=1.k=0.t=s.r=%s",
		    (uint64_t)(now + ttl), x);
}


static int generate_password(char *pass, size_t *passlen, const char *user)
{
	uint8_t digest[SHA512_DIGEST_LENGTH];
	unsigned int md_len = sizeof(digest);
	int err;

	if (!HMAC(EVP_sha512(),
		  zrest.secret, (int)zrest.secret_len,
		  (void *)user, (int)strlen(user),
		  digest, &md_len)) {

		restund_warning("zrest: HMAC failed\n");
		ERR_clear_error();
		return EINVAL;
	}

	err = base64_encode(digest, sizeof(digest), pass, passlen);
	if (err)
		return err;

	return 0;
}


static int auth_handler(const char *user, uint8_t *ha1)
{
	struct pl expires;
	time_t expi;
	char pass[256];
	size_t passlen = sizeof(pass);
	struct pl pl_keyindex;
	uint32_t keyindex = 0;
	int err;

	if (0 == re_regex(user, strlen(user),
			  "d=[0-9]+.v=1.k=[0-9]+.t=s.r=[a-z0-9]*",
			  &expires, &pl_keyindex, NULL)) {

		keyindex = pl_u32(&pl_keyindex);

		restund_debug("zrest: auth version 1 (keyindex=%u)\n",
			     keyindex);
	}
	else if (0 == re_regex(user, strlen(user),
			       "[0-9]+.s.[0-9]*", &expires, NULL)) {

		restund_info("zrest: auth version 0\n");
	}
	else {
		restund_info("zrest: could not parse username (%s)\n", user);
		return EPROTO;
	}

	expi = (time_t)pl_u64(&expires);
	if (expi < time(NULL)) {
		restund_debug("zrest: username expired %lli seconds ago\n",
			      time(NULL) - pl_u64(&expires));
		return ETIMEDOUT;
	}

	err = generate_password(pass, &passlen, user);
	if (err) {
		restund_warning("zrest: failed to generated password (%m)\n",
				err);
		return err;
	}

	restund_debug("zrest: VALID username token :)\n");

	return md5_printf(ha1, "%s:%s:%b",
			  user, restund_realm(), pass, passlen);
}


static void http_req_handler(struct http_conn *conn,
			     const struct http_msg *msg, void *arg)
{
	struct pl username;
	char tsuser[256];
	uint32_t ttl = 86400;
	char pass[256];
	size_t passlen = sizeof(pass);
	int err;
	(void)arg;
	struct sa stun_addr;

	if (re_regex(msg->prm.p, msg->prm.l, "username=[^&]+", &username)) {

		restund_warning("zrest: missing username parameter\n");
		http_ereply(conn, 400, "Bad Request");
		return;
	}

	generate_username_v1(tsuser, sizeof(tsuser), ttl);

	err = generate_password(pass, &passlen, tsuser);
	if (err) {
		restund_warning("zrest: could not generate password"
				" for use '%s' (%m)\n", tsuser, err);
		http_ereply(conn, 500, "Server Error");
	}

	restund_udp_socket(&stun_addr, NULL, false, false);

	http_creply(conn, 200, "OK", "application/json",

		    "{\r\n"
		    " \"username\" : \"%s\",\r\n"
		    " \"password\" : \"%b\",\r\n"
		    " \"ttl\" : %u,\r\n"
		    " \"uris\" : [\r\n"
		    "   \"turn:%J?transport=udp\",\r\n"
		    "   ]\r\n"
		    "}\r\n"
		    ,
		    tsuser,
		    pass, passlen,
		    ttl,
		    &stun_addr
		    );
}


static int module_init(void)
{
	char addr[64];
	int err;

	err = conf_get_str(restund_conf(), "zrest_secret", zrest.secret,
			   sizeof(zrest.secret));
	if (err) {
		restund_error("zrest: missing config 'rest_secret'\n");
		return err;
	}

	zrest.secret_len = strlen(zrest.secret);
	if (zrest.secret_len == 0) {
		restund_error("zrest: config 'zrest_secret' is empty\n");
		return EINVAL;
	}

	restund_db_set_auth_handler(auth_handler);

	/* selftest */
	if (1) {
		char user[256], pass[256];
		size_t passlen = sizeof(pass);
		generate_username_v1(user, sizeof(user), 60);
		err = generate_password(pass, &passlen, user);
		if (err) {
			restund_error("zrest: failed to generate password"
				      " for user='%s' (%m)\n", user, err);
			return err;
		}

		restund_info("zrest: selftest passed (pass=%b)\n",
			     pass, passlen);
	}

	if (0 == conf_get_str(restund_conf(), "zrest_listen",
			      addr, sizeof(addr))) {

		struct sa http_addr;

		err = sa_set_str(&http_addr, addr, 8000);
		if (err) {
			restund_warning("zrest: invalid address (%s)\n", addr);
			return err;
		}

		err = http_listen(&zrest.http_sock, &http_addr,
				  http_req_handler, NULL);
		if (err) {
			restund_warning("zrest: failed to listen on %J (%m)\n",
					&http_addr, err);
			return err;
		}

		restund_info("zrest: HTTP server listening on %J\n",
			     &http_addr);
	}

	restund_debug("zrest: module loaded\n");

	return 0;
}


static int module_close(void)
{
	zrest.http_sock = mem_deref(zrest.http_sock);

	restund_db_set_auth_handler(NULL);

	restund_debug("zrest: module closed\n");

	return 0;
}


const struct mod_export exports = {
	.name  = "zrest",
	.type  = "auth",
	.init  = module_init,
	.close = module_close
};
