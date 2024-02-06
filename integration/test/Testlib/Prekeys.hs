module Testlib.Prekeys
  ( somePrekeys,
    someLastPrekeys,
    somePrekeysRendered,
    someLastPrekeysRendered,
    someCert,
    somePrivKey,
    somePubKey,
  )
where

import Data.Aeson
import Data.String
import Data.Word
import Prelude

-- | FUTUREWORK: client ids are calculated from prekeys in brig, so we should have a more
-- robust mechanism to pick them, to avoid id clashes as well as running out of list.  just
-- call cryptobox?  (or fake it, find out where the id is encoded in the key payload, count
-- that inside an MVar, and return the same key with different id every time?)
somePrekeys :: [String]
somePrekeys =
  [ "pQABAQECoQBYIOjl7hw0D8YRNqkkBQETCxyr7/ywE/2R5RWcUPM+GJACA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQICoQBYIGoXawUQWQ9ZW+MXhvuo9ALOBUjLff8S5VdAokN29C1OA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQMCoQBYIEjdt+YWd3lHmG8pamULLMubAMZw556IO8kW7s1MLFytA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQQCoQBYIPIaOA3Xqfk4Lh2/pU88Owd2eW5eplHpywr+Mx4QGyiMA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQUCoQBYIHnafNR4Gh3ID71lYzToewEVag4EKskDFq+gaeraOlSJA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQYCoQBYIFXUkVftE7kK22waAzhOjOmJVex3EBTU8RHZFx2o1Ed8A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQgCoQBYIJH1ewvIVV3yGqQvdr/QM9HARzMgo5ksOTRyKEuN2aZzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQkCoQBYIFcAnXdx0M1Q1hoDDfgMK9r+Zchn8YlVHHaQwQYhRk1dA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQoCoQBYIGs3vyxwmzEZ+qKNy4wpFkxc+Bgkb0D76ZEbxeeh/9DVA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQsCoQBYIGUiBeOJALP5dkMduUZ/u6MDhHNrsrBUa3f0YlSSWZbzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQwCoQBYIMp6QNNTPDZgL3DSSD/QWWnBI7LsTZp2RhY/HLqnIwRZA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ0CoQBYIJXSSUrE5RCNyB5pg+m6vGwK7RvJ+rs9dsdHitxnfDhuA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ4CoQBYIHmtOX7jCKBHFDysb4H0z/QWoCSaEyjerZaT/HOP8bgDA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ8CoQBYIIaMCTcPKj2HuYQ7i9ZaxUw9j5Bz8TPjoAaTZ5eB0w1kA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARACoQBYIHWAOacKuWH81moJVveJ0FSfipWocfspOIBhaU6VLWUsA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARECoQBYIA8XtUXtnMxQslULnNAeHBIivlLRe/+qdh2j6nTfDAchA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARICoQBYIGgzg6SzgTTOgnk48pa6y2Rgjy004DkeBo4CMld3Jlr6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARMCoQBYIEoEFiIpCHgn74CAD+GhIfIgbQtdCqQqkOXHWxRlG6Y6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARQCoQBYINVEwTRxNSe0rxZxon4Rifz2l4rtQZn7mHtKYCiFAK9IA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARUCoQBYIN3aeX2Ayi2rPFbiaYb+O2rdHUpFhzRs2j28pCmbGpflA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARYCoQBYIJe5OJ17YKQrNmIH3sE++r++4Z5ld36axqAMjjQ3jtQWA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
  ]

someLastPrekeys :: [String]
someLastPrekeys =
  [ "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggwO2any+CjiGP8XFYrY67zHPvLgp+ysY5k7vci57aaLwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggoChErA5oTI5JT769hJV+VINmU8kougGdYqGd2U7hPa8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggPLk4BBJ8THVLGm7r0K7EJITRlJnt6bpNzM9GTNRYcCcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggqHASsRlZ1i8dESXRXBL2OvR+0yGUtqK9vJfzol1E+osDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggx/N1YhKXSJYJQxhWgHSA4ASaJKIHDJfmEnojfnp9VQ8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggVL6QIpoqmtKxmB8HToiAPxfjSDEzJEUAoFKfhXou06YDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggRs74/ViOrHN+aS2RbGCwC0sJv1Sp/Q0pmRB15s9DCBMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggtNO/hrwzt9M/1X6eK2sG6YFmA7BDqlFMEipbZOsg0vcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg1rZEY6vbAnEz+Ern5kRny/uKiIrXTb/usQxGnceV2HADoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg2647mOAVeOdhW57Q1zXDigDxRz/hB8ITFSZ7uo+pXH4DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggjddbHizABYOY0T6rvJeZCvV20dvTT9BYv95ri9bqSb8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggCKT/GspZquUY6vKC4TFvaFqTH1QGG1ptauiaulnfqkUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggv7bf/kEsTKFDGSgswsywq6AIxBq5AqZbLjDYDHfGjrcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggUbjGhhh8EwZEPSz+Y31rYNUu7jsRR8dy1F5FSiJXfXEDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg/4nz1uHiPBVGFvYjTMwGQ31bSFNctbU0r2nBtpsK9kcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggwbJDyKl7T3+3Ihc0YF06Dz2J11My5qn7JKG+U+ti8lQDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgglc6nCoZR2/qjLp0tr7vRyuXqb7ugdHHDadjX7zSl4uMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg5ER8h0/bIADXjBXe/XPKdzekgv6nhJ4hp3vJ3jtTSbUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggsgV6jq+GuNuvXk+ctHh570cNqEmfPhz34wcYCMCf9xIDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggdQdlPqkBw6+phKhohp3YaWQL710euZDnyMLFwf2cS0oDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggKlsI/snuQMoYcZRw/kN+BobPV5gwYeBClp0Wx9btTGUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggtruFBClEgdPKvjpHsYLlWMev9L4OmYZwlxbY0NwvzOwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggRUdh4cuYtFNL46RLnPy65goYInyreStKwsEcY3pPlLkDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggQtT7lLZzH171F4jCbHNwxEAt28FwdQ8Kt2tbxFzPgC0DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggQeUPM119c+6zRsEupA8zshTfrZiLpXx1Ji0UMMumq9IDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
  ]

render :: [Word16] -> [String] -> [Value]
render is = zipWith (\i k -> object [fromString "id" .= i, fromString "key" .= k]) is

somePrekeysRendered :: [Value]
somePrekeysRendered = render [1 ..] somePrekeys

someLastPrekeysRendered :: [Value]
someLastPrekeysRendered = render (repeat maxBound) someLastPrekeys

somePubKey :: String
somePubKey =
  "-----BEGIN PUBLIC KEY-----\n\
  \MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0\n\
  \G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH\n\
  \WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV\n\
  \VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS\n\
  \bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8\n\
  \7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la\n\
  \nQIDAQAB\n\
  \-----END PUBLIC KEY-----"

somePrivKey :: String
somePrivKey =
  "-----BEGIN RSA PRIVATE KEY-----\n\
  \MIIEpAIBAAKCAQEAu+Kg/PHHU3atXrUbKnw0G06FliXcNt3lMwl2os5twEDcPPFw\n\
  \/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPHWvUBdiLfGrZqJO223DB6D8K2Su/o\n\
  \dmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKVVPOaOzgtAB21XKRiQ4ermqgi3/nj\n\
  \r03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiSbUKr/BeArYRcjzr/h5m1In6fG/if\n\
  \9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg87X883H+LA/d6X5CTiPv1VMxXdBUi\n\
  \GPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7lanQIDAQABAoIBAQC0doVy7zgpLsBv\n\
  \Sz0AnbPe1pjxEwRlntRbJSfSULySALqJvs5s4adSVGUBHX3z/LousAP1SRpCppuU\n\
  \8wrLBFgjQVlaAzyQB84EEl+lNtrG8Jrvd2es9R/4sJDkqy50+yuPN5wnzWPFIjhg\n\
  \3jP5CHDu29y0LMzsY5yjkzDe9B0bueXEZVU+guRjhpwHHKOFeAr9J9bugFUwgeAr\n\
  \jF0TztzFAb0fsUNPiQAho1J5PyjSVgItaPfAPv/p30ROG+rz+Rd5NSSvBC5F+yOo\n\
  \azb84zzwCg/knAfIz7SOMRrmBh2qhGZFZ8gXdq65UaYv+cpT/qo28mpAT2vOkyeD\n\
  \aPZp0ysBAoGBAOQROoDipe/5BTHBcXYuUE1qa4RIj3wgql5I8igXr4K6ppYBmaOg\n\
  \DL2rrnqD86chv0P4l/XOomKFwYhVGXtqRkeYnk6mQXwNVkgqcGbY5PSNyMg5+ekq\n\
  \jSOOPHGzzTWKzYuUDUpB/Lf6jbTv8fq2GYW3ZYiqQ/xiugOvglZrTE7NAoGBANLl\n\
  \irjByfxAWGhzCrDx0x5MBpsetadI9wUA8u1BDdymsRg73FDn3z7NipVUAMDXMGVj\n\
  \lqbCRlHESO2yP4GaPEA4FM+MbTZSuhAYV+SY07mEPLHF64/nJas83Zp91r5rhaqJ\n\
  \L9rWCl3KJ5OUnr3YizCnHIW72FxjwtpjxHJLupsRAoGAGIbhy8qUHeKh9F/hW9xP\n\
  \NoQjW+6Rv7+jktA1eqpRbbW1BJzXcQldVWiJMxPNuEOg1iZ98SlvvTi1P3wnaWZc\n\
  \eIapP7wRfs3QYaJuxCC/Pq2g0ieqALFazGAXkALOJtvujvw1Ea9XBlIjuzmyxEuh\n\
  \Iwg+Gxx0g0f6yTquwax4YGECgYEAnpAK3qKFNO1ECzQDo8oNy0ep59MNDPtlDhQK\n\
  \katJus5xdCD9oq7TQKrVOTTxZAvmzTQ1PqfuqueDVYOhD9Zg2n/P1cRlEGTek99Z\n\
  \pfvppB/yak6+r3FA9yBKFS/r1zuMQg3nNweav62QV/tz5pT7AdeDMGFtaPlwtTYx\n\
  \qyWY5aECgYBPySbPccNj+xxQzxcti2y/UXjC04RgOA/Hm1D0exa0vBqS9uxlOdG8\n\
  \F47rKenpBrslvdfTVsCDB1xyP2ebWVzp6EqMycw6OLPxgo3fBfZ4pi6P+rByh0Cc\n\
  \Lhfh+ET0CPnKCxtop3lUrn4ZvqchS0j3J+M0pDuqoWF5hfKxFhkEIw==\n\
  \-----END RSA PRIVATE KEY-----"

someCert :: String
someCert =
  "-----BEGIN CERTIFICATE-----\n\
  \MIIDdjCCAl4CCQCm0AiwERR/qjANBgkqhkiG9w0BAQsFADB9MQswCQYDVQQGEwJE\n\
  \RTEPMA0GA1UECAwGQmVybGluMQ8wDQYDVQQHDAZCZXJsaW4xGDAWBgNVBAoMD1dp\n\
  \cmUgU3dpc3MgR21iSDERMA8GA1UEAwwId2lyZS5jb20xHzAdBgkqhkiG9w0BCQEW\n\
  \EGJhY2tlbmRAd2lyZS5jb20wHhcNMTYwODA0MTMxNDQyWhcNMzYwNzMwMTMxNDQy\n\
  \WjB9MQswCQYDVQQGEwJERTEPMA0GA1UECAwGQmVybGluMQ8wDQYDVQQHDAZCZXJs\n\
  \aW4xGDAWBgNVBAoMD1dpcmUgU3dpc3MgR21iSDERMA8GA1UEAwwId2lyZS5jb20x\n\
  \HzAdBgkqhkiG9w0BCQEWEGJhY2tlbmRAd2lyZS5jb20wggEiMA0GCSqGSIb3DQEB\n\
  \AQUAA4IBDwAwggEKAoIBAQC74qD88cdTdq1etRsqfDQbToWWJdw23eUzCXaizm3A\n\
  \QNw88XD994aIArKbGn7smpkOux5LkP1Mcatb45BEg8da9QF2It8atmok7bbcMHoP\n\
  \wrZK7+h2aeNknbPbeuFegQCtOmW74OD0r5zYtV5dMpVU85o7OC0AHbVcpGJDh6ua\n\
  \qCLf+eOvTetfKr+o2S413q01yD4cB8bF8a+8JJgF+JJtQqv8F4CthFyPOv+HmbUi\n\
  \fp8b+J/0YQjqbx3EdP0ltjnfCKSyjDLpqMK6qyQgWDztfzzcf4sD93pfkJOI+/VU\n\
  \zFd0FSIY+4L0hP/oI1DX8sW3Q/ftrHnz4sZiVoWjuVqdAgMBAAEwDQYJKoZIhvcN\n\
  \AQELBQADggEBAEuwlHElIGR56KVC1dJiw238mDGjMfQzSP76Wi4zWS6/zZwJUuog\n\
  \BkC+vacfju8UAMvL+vdqkjOVUHor84/2wuq0qn91AjOITD7tRAZB+XLXxsikKv/v\n\
  \OXE3A/lCiNi882NegPyXAfFPp/71CIiTQZps1eQkAvhD5t5WiFYPESxDlvEJrHFY\n\
  \XP4+pp8fL8YPS7iZNIq+z+P8yVIw+B/Hs0ht7wFIYN0xACbU8m9+Rs08JMoT16c+\n\
  \hZMuK3BWD3fzkQVfW0yMwz6fWRXB483ZmekGkgndOTDoJQMdJXZxHpI3t2FcxQYj\n\
  \T45GXxRd18neXtuYa/OoAw9UQFDN5XfXN0g=\n\
  \-----END CERTIFICATE-----"
