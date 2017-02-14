Bilge - Library for HTTP request/response tests
===============================================

This library provides declarative ways to compose HTTP requests and
assert on responses.

Example
-------

```haskell
main =
    assert (get (host "www.google.de" . debug Head)) $ do
        const 200         === statusCode
        const "*Wrong*"   === statusMessage
        const "text/html" =~= getHeader' "Content-Type"
```

```
ghci> Main.main
GET http://www.google.de/ HTTP/1.1
Host: <default>
User-Agent: http-streams/0.6.0.1
Accept-Encoding: gzip


-
HTTP/1.1 200 OK
Set-Cookie: PREF=ID=3ddd2c16edf255e6:FF=0:TM=1367832849:LM=1367832849:S=riZKOYI-N1jCR-kr; expires=Wed, 06-May-2015 09:34:09 GMT; path=/; domain=.google.de,NID=67=Kj8YyTHq3ad39YPZpsnrMK-B2THJ_uv738Y5wwdRcNPaXRuOcyk7kpt67gXSf_CMW5po_iLqmfF1PPyRIvqP4BIThmf05o5YtL3ns7vMNNhOdndiApPUlc39KzgvFZqM; expires=Tue, 05-Nov-2013 09:34:09 GMT; path=/; domain=.google.de; HttpOnly
X-Frame-Options: SAMEORIGIN
Transfer-Encoding: chunked
X-XSS-Protection: 1; mode=block
Date: Mon, 06 May 2013 09:34:09 GMT
Cache-Control: private, max-age=0
Server: gws
Expires: -1
P3P: CP="This is not a P3P policy! See http://www.google.com/support/accounts/bin/answer.py?hl=en&answer=151657 for more info."
Content-Encoding: gzip
Content-Type: text/html; charset=ISO-8859-1


--
*** Exception: Assertions failed:
 2: "*Wrong*" =/= "OK"
```
