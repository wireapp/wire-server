#!/usr/bin/env python3

# curl 'https://nginz-https.bund-next-column-1.wire.link/v4/conversations/bund-next-column-2.wire.link/eabb40cc-bf99-5a50-bd56-60c120830235/proteus/messages' \
#   -H 'authority: nginz-https.bund-next-column-1.wire.link' \
#   -H 'accept: application/json, text/plain, */*' \
#   -H 'accept-language: en-US,en;q=0.9,de;q=0.8' \
#   -H 'authorization: Bearer K6fM3RIJl5KXz7ZqFbEbdF-bZxie458do_ZKq1Uc9kT9zqdJ7mDK-8xN288kTQK7R_aEVI5DeXf2WdlbhCaZCg==.v=1.k=1.d=1695289238.t=a.l=.u=13cfb002-6f07-434a-90fa-1422e8141a30.i=139da7a7e0034030.c=4695780460598352338' \
#   -H 'cache-control: no-cache' \
#   -H 'content-type: application/x-protobuf' \
#   -H 'origin: https://webapp.bund-next-column-1.wire.link' \
#   -H 'pragma: no-cache' \
#   -H 'sec-ch-ua: "Google Chrome";v="117", "Not;A=Brand";v="8", "Chromium";v="117"' \
#   -H 'sec-ch-ua-mobile: ?0' \
#   -H 'sec-ch-ua-platform: "Linux"' \
#   -H 'sec-fetch-dest: empty' \
#   -H 'sec-fetch-mode: cors' \
#   -H 'sec-fetch-site: same-site' \
#   -H 'user-agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36' \
#   --data-raw $'\n\n\u0008°\u0080\u008d\u0080þôéÎ\u0013\u0012Ý\u0004\n\u001cbund-next-column-2.wire.link\u0012¼\u0004\n\u0012\n\u0010ðà~\u0083µsF\u0089³f^úJ\u0085\u009ar\u0012µ\u0001\n\n\u0008\u009eûâ±ÎÛ¬ê\u001b\u0012¦\u0001£\u0000\u0001\u0001¡\u0000X þºÁ¿©\u0014êÜÉþÚ|\u009c7Ä\u008aò\u008fZ\u008d\u0019x\u0084Ù\u001aÃ¼Å\u009b¾bÑ\u0002X{\u0001¥\u0000P]Èx>x{üú*\u009fÏ{"l(¹\u0001\u0018\u001b\u0002\u0001\u0003¡\u0000X y*z.\u0081a±;þ\u009a¯ä\u0008$j\u009aLáf\u009dì<\u0089/ùy6|QWû\u0088\u0004X:\u001f\u0017X³\u009aðT7ØÚ\u00802pÚ=²\u008e\u0005Y\u0004\u0018õ{»2G`ÿ4M \u0007ÂÜ\u009e<7¹\u0004öÃ\u0014|Ëìx{\u009f\u0095\u008e\'/\u000c\r\u0091á÷m\u0012´\u0001\n\n\u0008»ýð×Ã\u009a®\u008em\u0012¥\u0001£\u0000\u0001\u0001¡\u0000X s\u0093ªqGë0 èñwfò\u0098U\u008d)«y\u008e\u0004üîb\u009dV\u0084+Ve=\u0014\u0002Xz\u0001¥\u0000P©6±@}\u0097\u000bbj\u008e\u008eK½\u0014Ó«\u0001\u0002\u0002\u0005\u0003¡\u0000X 2ÊÆÜyþêÅ\u000c¦ª¯/×Æý\u001bª79½·ø\u0015\u0005ßâ&ô\u0018³Ó\u0004X:5Á5.þÞ¡jäâWÿÝ$)í=ÊJ½Ñ@\u0089\u001bÜÿ?\u0089ùòèÄaÉß7*äh.û¶\u0094Gài\u0016\u008d\u007fÚ\u0019j.\u0000\u0093Òrñ\u0012¶\u0001\n\u000b\u0008çµ\u0093\u0096Ýðéåí\u0001\u0012¦\u0001£\u0000\u0001\u0001¡\u0000X \u000f÷\u001eßxäÒ,æBár\r[R,A78Þ3kÿ\u0094\u0018\u0098Äì¬Q¤*\u0002X{\u0001¥\u0000P\u0015Òè*\u0098ì\u0010¾¸\u001f,å\u000fÛ 8\u0001\u0018\'\u0002\u0003\u0003¡\u0000X Ð\u0003Ô+\u009eå¹\u0021_~\u000f\u0083¯0øÇ¦«\u008c\u0085åQÜbrî\u0086´\u0097\u0003MÃ\u0004X:uÖ¤á¢^\u0099=Ê{\u0094,Å\u0085[\u008e:\u0087\u0000\u0099\u0014?î\u007f³\u0010\u0086\u0086m\u0014\u00955Gy\r\u0005\u000e\u008a\u0087Â¢\u0083G§v5â\u001fÀ\u000cÙÓNêù <j\u0018\u0001:\u0000' \
#   --compressed

users = {
    1: {'idx': 1, 'id': 'foo'},
    2: {'idx': 2, 'id': '13cfb002-6f07-434a-90fa-1422e8141a30' },
    3: {'idx': 3, 'id': '336da3f3-4c4e-4aa2-8cf4-d91e94c66add' },
}

domains = {
    1: {'domain': 'bund-next-column-1.wire.link', 'galley_port': 6085, 'cannon_port': 6086},
    2: {'domain': 'bund-next-column-2.wire.link', 'galley_port': 7085, 'cannon_port': 7086},
    3: {'domain': 'bund-next-column-offline-web.wire.link'}
}

convs = [
    {
        'user_idxs': set([1, 2]),
        'conv_id': 'eabb40cc-bf99-5a50-bd56-60c120830235',
        'domain_idx': 2
    },
    {
        'user_idxs': set([3, 2]),
        'conv_id': 'd29cca43-5421-562c-8d39-2ee74b6e01eb',
        'domain_idx': 3
    }
]

def send_msg(user_from, conv):
    domain_conv = domains[conv['domain_idx']]
    url = f'localhost:{domain["galley_port"]}/v4/conversations/{domain_conv["domain"]}/{conv["conv_id"]}/proteus/messages'
    print(url)

def get_conv(user_from, user_to):
    for c in convs:
        if set([user_from, user_to]) == c['user_idxs']:
            return c
    return ValueError('could not find cov')

def open_websocket(user_id):
    user = users[user_id]
    print(f'open web socket for user {user["id"]}')
    domain = domains[user['idx']]
    url = f'wss://localhost:{domain["cannon_port"]}/await'
    print(url)
    headers = {"Z-User": user["id"]}
    print(headers)

def main():
    user_from = 1
    user_to = 2
    conv = get_conv(user_from, user_to)
    send_msg(user_from, conv)

if __name__ == '__main__':
    main()
