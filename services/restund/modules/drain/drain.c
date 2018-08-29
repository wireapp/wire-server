/**
 * @file drain.c
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

#include <re.h>
#include <restund.h>


static bool is_draining = false;

/*
 * Iff is_draining == true, prevents new allocations and denies allocation
 * refresh by replying with a '508 Insufficient Capacity' message.
 */
static bool request_handler(struct restund_msgctx *ctx, int proto, void *sock,
                const struct sa *src, const struct sa *dst,
                const struct stun_msg *msg)
{
    int err;
    struct stun_attr *lt;

    if (is_draining) {
        switch (stun_msg_method(msg)) {

        case STUN_METHOD_ALLOCATE:
            restund_info("received ALLOCATE request while in drain mode\n");
            goto unavailable;

        case STUN_METHOD_REFRESH:
            lt = stun_msg_attr(msg, STUN_ATTR_LIFETIME);

            if (lt && lt->v.lifetime > 0) {
                restund_info("received REFRESH request while in drain mode\n");
                goto unavailable;
            }

            break;

        default:
            break;
        }
    }

    return false;

    unavailable:
        err = stun_ereply(proto, sock, src, 0, msg,
                  508, "Draining",
                  NULL, 0, ctx->fp, 1,
                  STUN_ATTR_SOFTWARE, restund_software);

        if (err) {
            restund_warning("drain reply error: %m\n", err);
        }

        return true;
}

static struct restund_stun stun = {
    .reqh = request_handler
};

// commands

static void drain_print(struct mbuf *mb)
{
    (void)mbuf_printf(mb, "is_draining: %d\n", is_draining);
}

static void drain_enable(struct mbuf *mb)
{
    is_draining = true;
    drain_print(mb);
}

static void drain_disable(struct mbuf *mb)
{
    is_draining = false;
    drain_print(mb);
}

static struct restund_cmdsub cmd_drain_print = {
    .cmdh = drain_print,
    .cmd  = "drain_state",
};

static struct restund_cmdsub cmd_drain_enable = {
    .cmdh = drain_enable,
    .cmd  = "drain_enable",
};

static struct restund_cmdsub cmd_drain_disable = {
    .cmdh = drain_disable,
    .cmd  = "drain_disable",
};


// module

static int module_init(void)
{
    restund_stun_register_handler(&stun);
    restund_cmd_subscribe(&cmd_drain_print);
    restund_cmd_subscribe(&cmd_drain_enable);
    restund_cmd_subscribe(&cmd_drain_disable);

    restund_debug("drain: module loaded\n");

    return 0;
}

static int module_close(void)
{
    restund_cmd_unsubscribe(&cmd_drain_enable);
    restund_cmd_unsubscribe(&cmd_drain_disable);
    restund_cmd_unsubscribe(&cmd_drain_print);
    restund_stun_unregister_handler(&stun);

    restund_debug("drain: module closed\n");

    return 0;
}

const struct mod_export DECL_EXPORTS(drain) = {
    .name = "drain",
    .type = "stun",
    .init = module_init,
    .close = module_close,
};
