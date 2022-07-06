# Inbucket chart

[*Inbucket*](https://www.inbucket.org/) is a fake SMTP server that provides all
captured eMails via a webapp and a REST API. At *Wire* it is used in testing
environments to not have to deal with concrete SMTP server configurations.
Especially, it saves us to care about topics like *SPAM filters* and *server
grey & black listing*.

This chart exists to adjust the [`inbucket/inbucket`
chart](https://artifacthub.io/packages/helm/inbucket/inbucket) to our needs.
