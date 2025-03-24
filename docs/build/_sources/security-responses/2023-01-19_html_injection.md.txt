# 2023-01-19 - Security Advisory: HTML Injection in wire.com

Last updated: 2023-01-31

## Introduction
On the 16st January, 2023, we were informed about a possible vulnerability in our website wire.com. A get-parameter on the page https://wire.com/en/pricing/ was vulnerable to HTML injection. As the website wire.com is not directly maintained by Wire, the service provider was directly informed about the disclosed issue. The patch that fixed that vulnerability was rolled out on the 18th of January.

## Impact
An adversary would have been able to inject arbitrary HTML code through the parameter and send that link to someone else which would show them e.g., a defaced website or potentially inject JavaScript.

## Are Wire installations affected?
Wire/wire-server is not affected by this vulnerability as our website wire.com is completely separated from the Wire backend.

## Are Wire clients affected?
Wire clients are not affected by this vulnerability.

## Credits
We thank [Umar Ahmed](https://linkedin.com/in/theumar9) for reporting this vulnerability.
