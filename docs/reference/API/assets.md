# Assets

Assets are images, videos or any types of files that are posted and viewed in
the context of a conversation or attached to user profiles.

## Contents

<!-- vim-markdown-toc GFM -->

* [Authentication](#authentication)
* [Authorization](#authorization)
* [Uploading Assets](#uploading-assets)
    * [Simple Upload](#simple-upload)
        * [Body Part 1: Metadata](#body-part-1-metadata)
        * [Body Part 2: Asset data](#body-part-2-asset-data)
        * [Simple Upload Example](#simple-upload-example)
    * [Resumable Upload](#resumable-upload)
        * [Prepare](#prepare)
            * [Example](#example)
        * [Upload](#upload)
            * [Example](#example-1)
    * [Upload Errors](#upload-errors)
* [Downloading Assets](#downloading-assets)
    * [Download Request Example](#download-request-example)
    * [Download Errors](#download-errors)
* [Deleting Assets](#deleting-assets)
* [Retention](#retention)
* [Asset Tokens](#asset-tokens)
* [Profile Assets](#profile-assets)

<!-- vim-markdown-toc -->

## Authentication

All requests mentioned here must include appropriate access tokens as
required by all authenticated API endpoints.

## Authorization

  * Any authenticated user is authorized to upload public or private assets.
  * Any authenticated user is authorized to download public assets.
  * Any authenticated user is authorized to download any private asset for which
    he possesses a valid asset token.

## Uploading Assets

Assets can be uploaded in one of two ways:

  * A simple, non-resumable upload with a single request.
  * A resumable upload that is performed in multiple steps
    and hence with multiple HTTP requests.

> Note: The currently allowed maximum size of any asset is 25MB.

### Simple Upload

The request body of a simple upload request must be a valid MIME multipart message of
type [multipart/mixed](http://tools.ietf.org/html/rfc2046#section-5.1.3). Thereby the first part specifies
asset metadata which can influence server-side processing or access restrictions and the second
part is the actual asset data.

#### Body Part 1: Metadata

The following headers are required for this body part:

  * `Content-Type`, with a value of `application/json`.
  * `Content-Length`, specifying the length of the JSON payload.

The following JSON metadata fields are supported:

  * `public` - Boolean - Default `false`  
    Whether the asset should be public, i.e. accessible without an [asset token](#tokens).
  * `retention` - String - Default `"persistent"`  
    The desired [retention](#retention).

#### Body Part 2: Asset data

The second body part contains the actual asset data.

The following headers are required for this body part:

  * `Content-Type`, with any MIME type.
  * `Content-Length`, specifying the length of the payload.
  * `Content-MD5`, specifying the MD5 of the payload.

The MD5 is required to verify consistency of the uploaded data and needs to be encoded
according to [RFC 2616](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.15).

#### Simple Upload Example

    > POST /assets/v3
    > Content-Length: 12345
    > Content-Type: multipart/mixed; boundary=frontier
    >
    > --frontier
    > Content-Type: application/json
    > Content-Length: 16
    >
    > {"public":false, "retention": "persistent"}
    > --frontier
    > Content-Type: image/jpeg
    > Content-Length: 1234
    > Content-MD5: sQqNsWTgdUEFt6mb5y4/5Q==
    >
    > ...
    > --frontier--

    < 201 Created
    < Location: /assets/v3/3-1-cb21f0ce-6bd4-400e-9776-26ad5dd7cd62
    < Content-Type: application/json
    <
    < {
    <   "key": "3-1-cb21f0ce-6bd4-400e-9776-26ad5dd7cd62",
    <   "expires": "2017-05-27T11:30:33.034Z",
    <   "token": "qJ8JPFLsiYGx7fnrlL+7Yk9="
    < }

The response body contains information about the newly created asset, namely the
unique `key`, the earliest date when the asset `expires` (see [retention](#retention))
and the `token` (see [asset tokens](#tokens)) needed for downloading the asset by
anyone other than the creator.

If `public` is set to `true`, no asset token is returned and the asset can be
accessed without an asset token (only a valid API access token, of course).

### Resumable Upload

A resumable upload is performed in two or more steps:

  1. Prepare the upload by creating a resumable resource.
  2. Upload the asset data in one or more requests.

The API is based on (and compatible with) the [TUS core protocol](http://tus.io/protocols/resumable-upload.html#core-protocol),
including the following extensions:

  * [Creation Extension](http://tus.io/protocols/resumable-upload.html#creation)
  * [Expiration Extension](http://tus.io/protocols/resumable-upload.html#expiration)

#### Prepare

In this step the resumable upload is prepared by creating a resumable resource, including metadata and
configuration options in the request. It conforms to the TUS creation extension and expects
a JSON request body with the following attributes:

 * `type` - MIME type of the file to upload
 * `public` - Boolean - Default `false`  
    Whether the asset should be public, i.e. accessible without an [asset token](#tokens).
 * `retention` - String - Default `"persistent"`  
    The desired [retention](#retention).

Note: The current version of the API does not support the optional [Upload-Defer-Length](http://tus.io/protocols/resumable-upload.html#upload-defer-length)
header and thus mandates usage of the [Upload-Length](http://tus.io/protocols/resumable-upload.html#upload-length) header.

##### Example

    > POST /assets/v3/resumable HTTP/1.1
    > Content-Length: 110
    > Content-Type: application/json
    > Upload-Length: 26214400
    >
    > {
    >     "retention":"persistent",
    >     "type":"application/octet-stream",
    >     "public":false
    > }

    < HTTP/1.1 201 Created
    < Location: /assets/v3/resumable/3-2-8054d06d-c85a-4941-a602-7e0e0ffece3e
    < Date: Mon, 15 Aug 2016 12:51:38 GMT
    < Upload-Expires: Tue, 16 August 2016 12:51:39 GMT
	< Tus-Resumable: 1.0.0
    < Content-Type: application/json
    <
    < {
    <    "expires": "2016-09-12T12:51:39.047Z",
    <    "chunk_size": 1048576,
    <    "asset": {
    <       "key":"3-2-8054d06d-c85a-4941-a602-7e0e0ffece3e",
    <       "expires":"2017-08-15T12:51:39.047Z",
    <       "token":"x2LWnt9NkVqB5HAl-sA6Bw=="
    <    }
    < }

The JSON response contains the following fields:

  * `expires`: The expiry of the resumable resource.
  * `chunk_size`: The minimum size for subsequent [upload](#upload-resumable-upload) requests
    that the server will accept. This is also the granularity at which uploads can be resumed.
  * `asset`: The unique key, expiry and optional token of the final asset,
    just like in a [simple upload](#upload-simple) response.

The expiry of the resumable resource is also returned in the TUS `Upload-Expires` header,
as defined by the [expiration extension](http://tus.io/protocols/resumable-upload.html#expiration).

The `Location` header contains the location of the created resumable resource and should
be used for all requests related to the resumable upload.

#### Upload

In order to upload the actual asset data, one or more `PATCH` requests must be sent to
the resumable resource location, as defined in the [TUS core protocol](http://tus.io/protocols/resumable-upload.html#core-protocol).
Thereby clients should send all the remaining bytes of an upload in a single request, but may
also perform an incremental upload with multiple requests. In either case the server will store
intermediate chunks of size `chunk_size`. Therefore, if an upload is performed with multiple
requests on purpose, all but the last request must contain a payload that is at least as large
as a single `chunk_size` and should always be a multiple of the `chunk_size`.

Once all bytes are uploaded, the server assembles all chunks into the final asset.
Depending on the size of the asset, this step may take a little while, so the client should
use a response timeout that is generous enough (e.g. 60 seconds).

If an upload was interrupted, e.g. due to connection issues, the client can obtain the
current status of the resumable upload via a `HEAD` request to the resumable resource,
as defined in the TUS core protocol, and resume the upload from the current offset
reported by the server.

##### Example

	> PATCH /assets/v3/resumable/3-2-8054d06d-c85a-4941-a602-7e0e0ffece3e HTTP/1.1
	> Content-Length: 26214400
	> Upload-Offset: 0
	> Content-Type: application/offset+octet-stream

	< HTTP/1.1 204 No Content
	< Date: Mon, 15 Aug 2016 12:51:37 GMT
	< Upload-Expires: Tue, 16 August 2016 12:51:37 GMT
	< Upload-Offset: 26214400
	< Tus-Resumable: 1.0.0

### Upload Errors

The most common error responses when uploading are as follows:

 * `400 Bad Request` - Invalid input was provided.
 * `408 Request Timeout` - The upload is progressing too slowly or has stalled
    while the server is still expecting more data.
 * `413 Request Entity Too Large` - The payload exceeds the currently allowed
    maximum size.

## Downloading Assets

Downloading an asset is performed with an HTTP `GET` request that yields a
redirection to a short-lived, signed [CloudFront](http://aws.amazon.com/cloudfront/) URL.
The [asset token](#tokens) must be provided via the `Asset-Token` header in order to download
an asset that is not public.

For example:

    GET /assets/v3/{key}
    Asset-Token: {token}

If the asset is public, no asset token needs to be provided.

### Download Request Example

    > GET /assets/v3/3-1-02be406e-32bf-43f3-833b-5b0de3a7b9a5 HTTP/1.1
    > Asset-Token: IIqAlR9geLFai6L4HIZcZg==
    > ...

    < HTTP/1.1 302 FOUND
    < Location: https://abdn1jiyxyzja.cloudfront.net/02be406e-32bf-43f3-833b-5b0de3a7b9a5?Expires=1379506422&Signature=Xy1lTwIguALIvgDb9IQvi7M-0nWuSQgpMUMuCLVLGpQB4eeibOYxpPFDOWB4-iHihQy1CYbhDzGENY9ishTAJYQKLuJYQmQbFG9thn7S2rsp6A01xo2g3Jb5QlTM-GRN45FlCYEOxnXxdzyt~VuoR1-uAi1sR3-o7Jxf00zeSLmhb8~ok3n4uRCvbZ0Rv9je8~K5sxhaBkIPX4A5zG~DIRHRJ-Nigs-NAexNP01ljaVNv5U6AeqVvvgvOx9pxKsD5JyYp1izhYIT2vXL0ay-sLCjdX0QpJKaJoPtvSbfA92HcTA1v9SJXpgQPYhuOi4wgNHCYL3EyhCjUX~6tioCrw__&Key-Pair-Id=APPOJNHT4IP4WQ2CDLZQ
    < Content-Length: 0

By following the returned link, the asset data can be downloaded. Such a request/response might look
like this:

    > GET /2c8f26dd-1532-46da-a37a-3249b0c5815d?Expires=1379431083&Signature=LWL01VdG5yDP9hTD0c5dk~UPyu993y6Yr6O7N732x0c2gj2dzfTui8tbVpvVAqZzSasdi3ELIoMXGgU6XZ9iSUStDAia2cZZIgZMjAAjdOr-DzjFds-reoEMmjuuTLrLqZ1hG2QO-2QmUvKCOsFlRAe7c9NduRWkTTtkCEw89tcCu1RI1IYzI4Aa2BKGkf1N9s4~ZO6o6gQVMU6Qw6P7HE4iEoN3YlN~MLOu6Zbbf7VBzQoix5Zkbso6jBtJsMdtXUXrxC3e4rDqXJK-5EwMMURdUUhebDR5MAsMi9KB56CD~uwE6Xto5bt0tstMdazq3D-RIE7VtVSsojdduiaDDg__&Key-Pair-Id=APKAJNHT4IP4WQ4CDUYQ HTTP/1.1
    > User-Agent: curl/7.24.0 (x86_64-apple-darwin12.0) libcurl/7.24.0 OpenSSL/0.9.8x zlib/1.2.5
    > Host: xbdn3ijyabcjm.cloudfront.net
    > Accept: */*

    < HTTP/1.1 200 OK
    < Content-Type: image/jpeg
    < Content-Length: 1234
    < Connection: keep-alive
    < Date: Tue, 17 Sep 2013 15:17:38 GMT
    < Last-Modified: Tue, 17 Sep 2013 15:17:04 GMT
    < ETag: "b10a8db164e0754105b7a99be72e3fe5"
    < Accept-Ranges: bytes
    < Server: AmazonS3
    < Age: 9
    < Via: 1.0 90f58e12db428210bf083bd4ee216ecf.cloudfront.net (CloudFront)
    < X-Cache: Hit from cloudfront
    < X-Amz-Cf-Id: Xmo-gZJJvgg9aANQbqT4Bxi9njsOwq3nB--aOjIHs5G0QlFGvUPfZA==
    <
    < ... data ...

### Download Errors

The most common error responses when requesting a download are as follows:

 * `400 Bad Request` - Invalid input was provided.
 * `404 Not Found` - An asset with the given key and token does not exist.

## Deleting Assets

Any asset may be explicitly deleted exclusively by the creator. This is done via an
HTTP `DELETE` request using the asset key as in the following example:

    > DELETE /assets/v3/3-1-cb21f0ce-6bd4-400e-9776-26ad5dd7cd62

    < 200 OK

## Retention

Every asset is subject to one of four retention policies:

    `volatile`: expire after 28 days
    `persistent`: move to Infrequent Access (IA) storage after 30 days, never deleted (don't use this anymore)
    `eternal`: none
    `expiring`: move to Infrequent Access (IA) storage after 30 days, delete after 1 year
    `eternal-infrequent_access`: move to Infrequent Access (IA) storage after 30 days, never deleted


The default retention for newly uploaded assets is `persistent`. We may want
or need to limit the use of and / or charge for extensive use of `eternal`
and `persistent` storage in the future.

Assets used as profile pictures or other [profile assets](#profile-assets)
should typically be uploaded with an `eternal` retention.

Any asset may be explicitly [deleted](#delete) by the creator before the retention
period expires.

## Asset Tokens

An *asset token* is a sequence of 16 cryptographically strong random bytes
(Base64-encoded for use in JSON and HTTP headers). Every uploaded asset that
is not public gets a distinct token.

The following are important characteristics of asset tokens:

  * They are not guessable.
  * One token cannot be derived from another, i.e. one token does not contain
    any clues as to what another valid token might be.
  * They are not part of the request URI for downloading an asset. A "hard to
    guess" URL alone is not sufficient for restricting access to a private asset,
    since URLs leak easily in caches, logs, etc.

Asset tokens can be (re)generated by the owner (i.e. the user who uploaded the asset) at any
time via a separate HTTP request:

    > POST /assets/v3/{key}/token

    < 200 OK
    < Content-Type: application/json
    <
    < {"token": "IIqAlR9geLFai6L4HIZcZg=="}

Such a request effectively revokes access to the asset for anyone with whom
the prior token has been shared. The asset key remains unchanged. If the asset
was previously public (i.e. did not have a token) then it is effectively
private after generating a token. If a private asset is supposed to be made
public, the asset token can be deleted:

    > DELETE /assets/v3/{key}/token

    < 200 OK

## Profile Assets

Uploaded assets can be connected to the user profile via a `PUT /self` HTTP
request. Thereby the user profile attribute `assets` contains a list
of objects that look as follows:

    {
        "key": "{key}",
        "type": "image"
    }

Thereby:

  * `key` is the unique asset key obtained through a successful upload.
  * `type` is a fixed enumeration of profile asset types. Currently the
    only supported asset type is `image` but [additional types may
    be added](RFC-Enhanced-Profile-Assets) in the future.
