// This file is part of the Wire Server implementation.
//
// Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License along
// with this program. If not, see <https://www.gnu.org/licenses/>.

use base64::Engine;
use jwt_simple::prelude::*;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct OAuthToken {
    pub scope: String,
}

/// Verify a token against the deprecated `oauth_scope` directive, which names
/// only the base of a scope (`conversations_code`) and leaves the tier to the
/// request method.
pub fn verify_oauth_token(
    jwk: &str,
    token: &str,
    required_scope: &str,
    method: &str,
) -> Result<String, OauthError> {
    let (subject, scopes) = verify_token(jwk, token)?;
    verify_scope(&scopes, required_scope, method)?;
    Ok(subject)
}

/// Verify a token against the `oauth_scopes` directive, which lists whole
/// scopes (`read:conversations_code write-only:conversations_code`), tier
/// included.
pub fn verify_oauth_token_scopes(
    jwk: &str,
    token: &str,
    required_scopes: &str,
    method: &str,
) -> Result<String, OauthError> {
    let (subject, scopes) = verify_token(jwk, token)?;
    verify_scopes(&scopes, required_scopes, method)?;
    Ok(subject)
}

/// Check the signature and return `(subject, scope claim)`.
fn verify_token(jwk: &str, token: &str) -> Result<(String, String), OauthError> {
    let jwk = serde_json::from_str::<Jwk>(jwk)?;
    let key = try_from_jwk(&jwk)?;
    let options = VerificationOptions {
        time_tolerance: Some(Duration::from_secs(1)),
        ..Default::default()
    };
    let claims = key.verify_token::<OAuthToken>(token, Some(options))?;
    let subject = claims.subject.ok_or(OauthError::InvalidJwtNoSubject)?;
    Ok((subject, claims.custom.scope))
}

/// Compatibility: `authorized_scopes` must be new syntax,
/// `required_scopes` can be old or new.
fn verify_scopes(
    authorized_scopes: &str,
    required_scopes: &str,
    method: &str,
) -> Result<(), OauthError> {
    let tier = required_tier(method)?;
    let required = required_scopes
        .split_whitespace()
        .filter(|s| scope_tier(s) == Some(tier))
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    verify_any(authorized_scopes, &required)
}

fn scope_tier(scope: &str) -> Option<&str> {
    scope.split_once(':').map(|(tier, _)| tier)
}

/// This is deprecated, use tiers "read", "write-only", "delete-only"
/// instead.  See `verify_scopes` below.
///
/// Deprecated behavior:
///
/// if method is GET, authorized scopes must contain either read:_, write:_, or admin:_
/// if method is POST, authorized scopes must contain either write:_ or admin:_
/// if method is PUT, authorized scopes must contain either write:_ or admin:_
/// if method is DELETE, authorized scopes must contain admin:_
///
/// Compatibility: `authorized_scopes` may be old or new syntax;
/// `required_scopes` must be a bare base name (old syntax), the tier
/// is added implicitly.
fn verify_scope(
    authorized_scopes: &str,
    required_scope: &str,
    method: &str,
) -> Result<(), OauthError> {
    let required = vec![format!("{}:{}", required_tier(method)?, required_scope)];
    verify_any(authorized_scopes, &required)
}

fn required_tier(method: &str) -> Result<&'static str, OauthError> {
    match method.to_uppercase().as_str() {
        "GET" => Ok("read"),
        "POST" => Ok("write-only"),
        "PUT" => Ok("write-only"),
        "DELETE" => Ok("delete-only"),
        _ => Err(OauthError::InvalidScope),
    }
}

fn verify_any(authorized_scopes: &str, required: &[String]) -> Result<(), OauthError> {
    let valid = authorized_scopes
        .split_whitespace()
        .flat_map(granted_scopes)
        .any(|granted| required.contains(&granted));

    if !valid {
        return Err(OauthError::InvalidScope);
    }
    Ok(())
}

/// Which scopes does a scope carried by a token (old or new syntax)
/// grant, in the vocabulary [`required_tier`] speaks?
fn granted_scopes(scope: &str) -> Vec<String> {
    let Some((tier, base)) = scope.split_once(':') else {
        return Vec::new();
    };
    let tiers: &[&str] = match tier {
        "read" => &["read"],
        "write" => &["read", "write-only"],
        "admin" => &["read", "write-only", "delete-only"],
        "write-only" => &["write-only"],
        "delete-only" => &["delete-only"],
        _ => &[],
    };
    tiers.iter().map(|t| format!("{}:{}", t, base)).collect()
}

fn try_from_jwk(jwk: &Jwk) -> Result<Ed25519PublicKey, OauthError> {
    Ok(match &jwk.algorithm {
        AlgorithmParameters::OctetKeyPair(p) => {
            let x = base64::prelude::BASE64_URL_SAFE_NO_PAD.decode(&p.x)?;
            Ed25519PublicKey::from_bytes(&x)?
        }
        _ => return Err(OauthError::InvalidJwk),
    })
}

#[derive(Debug, thiserror::Error)]
pub enum OauthError {
    /// Json error
    #[error(transparent)]
    JsonError(#[from] serde_json::Error),
    /// JWT error from jwt-simple crate
    #[error(transparent)]
    JwtSimpleError(#[from] jwt_simple::Error),
    /// Base64 decoding error
    #[error(transparent)]
    Base64DecodeError(#[from] base64::DecodeError),
    /// Invalid JWK
    #[error("invalid jwk")]
    InvalidJwk,
    /// Invalid JWT missing subject
    #[error("missing subject")]
    InvalidJwtNoSubject,
    /// Invalid scope
    #[error("invalid scope")]
    InvalidScope,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_verify_scope_get() {
        assert!(verify_scope("read:self foo bar", "self", "GET").is_ok());
        assert!(verify_scope("write:self foo bar", "self", "GET").is_ok());
        assert!(verify_scope("admin:self foo bar", "self", "GET").is_ok());
        assert!(verify_scope("foo bar", "self", "GET").is_err());
    }

    #[test]
    fn should_verify_scope_post() {
        assert!(verify_scope("write:self foo bar", "self", "POST").is_ok());
        assert!(verify_scope("admin:self foo bar", "self", "POST").is_ok());
        assert!(verify_scope("read:self foo bar", "self", "POST").is_err());
        assert!(verify_scope("foo bar", "self", "POST").is_err());
    }

    #[test]
    fn should_verify_scope_put() {
        assert!(verify_scope("write:self foo bar", "self", "PUT").is_ok());
        assert!(verify_scope("admin:self foo bar", "self", "PUT").is_ok());
        assert!(verify_scope("read:self foo bar", "self", "PUT").is_err());
        assert!(verify_scope("foo bar", "self", "PUT").is_err());
    }

    #[test]
    fn should_verify_scope_delete() {
        assert!(verify_scope("admin:self foo bar", "self", "DELETE").is_ok());
        assert!(verify_scope("write:self foo bar", "self", "DELETE").is_err());
        assert!(verify_scope("read:self foo bar", "self", "DELETE").is_err());
        assert!(verify_scope("foo bar", "self", "DELETE").is_err());
    }

    #[test]
    fn should_grant_scopes() {
        assert_eq!(granted_scopes("read:self"), vec!["read:self"]);
        assert_eq!(
            granted_scopes("write:self"),
            vec!["read:self", "write-only:self"]
        );
        assert_eq!(
            granted_scopes("admin:self"),
            vec!["read:self", "write-only:self", "delete-only:self"]
        );
        assert_eq!(granted_scopes("write-only:self"), vec!["write-only:self"]);
        assert_eq!(granted_scopes("delete-only:self"), vec!["delete-only:self"]);
        assert!(granted_scopes("self").is_empty());
        assert!(granted_scopes("nonsense:self").is_empty());
    }

    // The deprecated directive with scopes as they are handed out now.
    #[test]
    fn should_verify_scope_with_new_scopes() {
        assert!(verify_scope("read:self foo bar", "self", "GET").is_ok());
        assert!(verify_scope("write-only:self", "self", "GET").is_err()); // this is the interesting case.
        assert!(verify_scope("write-only:self", "self", "POST").is_ok());
        assert!(verify_scope("write-only:self", "self", "PUT").is_ok());
        assert!(verify_scope("write-only:self", "self", "DELETE").is_err());
        assert!(verify_scope("delete-only:self", "self", "DELETE").is_ok());
        assert!(verify_scope("delete-only:self", "self", "POST").is_err());
        assert!(verify_scope("read:other write-only:other", "self", "GET").is_err());
    }

    #[test]
    fn should_verify_scopes() {
        let cfg = "read:self write-only:self";
        assert!(verify_scopes("read:self", cfg, "GET").is_ok());
        assert!(verify_scopes("write-only:self", cfg, "POST").is_ok());
        assert!(verify_scopes("write-only:self", cfg, "PUT").is_ok());
        assert!(verify_scopes("read:self", cfg, "POST").is_err());
        assert!(verify_scopes("write-only:self", cfg, "GET").is_err());
        assert!(verify_scopes("read:self write-only:self", cfg, "DELETE").is_err());
        assert!(verify_scopes("read:other", cfg, "GET").is_err());
        assert!(verify_scopes("foo bar", cfg, "GET").is_err());
    }

    // A list without a scope of the tier the method needs lets nobody in, and
    // neither does a location with no scope configured at all.
    #[test]
    fn should_verify_scopes_closed() {
        assert!(verify_scopes("read:self write-only:self admin:self", "", "GET").is_err());
        assert!(verify_scopes("delete-only:self", "read:self write-only:self", "DELETE").is_err());
        assert!(verify_scopes("read:self", "read:self", "PATCH").is_err());
    }

    // Tokens handed out before the migration carry cumulative scopes and have
    // to keep working against a migrated location.
    #[test]
    fn should_verify_scopes_with_old_scopes() {
        let cfg = "read:conversations_code write-only:conversations_code";
        assert!(verify_scopes("write:conversations_code", cfg, "GET").is_ok());
        assert!(verify_scopes("write:conversations_code", cfg, "POST").is_ok());
        assert!(verify_scopes("read:conversations_code", cfg, "GET").is_ok());
        assert!(verify_scopes("read:conversations_code", cfg, "POST").is_err());
        assert!(verify_scopes("admin:conversations_code", cfg, "GET").is_ok());
        assert!(verify_scopes("admin:conversations_code", cfg, "POST").is_ok());
        // ... but only as far as the location goes: nothing here is deletable.
        assert!(verify_scopes("admin:conversations_code", cfg, "DELETE").is_err());
    }

    #[test]
    fn should_verify_oauth_token() {
        let uid = "842ddbc8-56ec-408d-9fa8-7a8c37ad22a7";
        let key = Ed25519KeyPair::generate();
        let jwk = mk_jwk(key.public_key());
        let token = Claims::with_custom_claims(
            OAuthToken {
                scope: "write:foo read:test admin:bar".to_string(),
            },
            Duration::from_secs(3600),
        )
        .with_subject(uid);
        let jwt = key.sign::<OAuthToken>(token).unwrap();
        let subject =
            verify_oauth_token(&serde_json::to_string(&jwk).unwrap(), &jwt, "test", "GET").unwrap();
        assert_eq!(&subject, uid);
    }

    #[test]
    fn should_verify_oauth_token_scopes() {
        let uid = "842ddbc8-56ec-408d-9fa8-7a8c37ad22a7";
        let key = Ed25519KeyPair::generate();
        let jwk = mk_jwk(key.public_key());
        let token = Claims::with_custom_claims(
            OAuthToken {
                scope: "write-only:foo read:test".to_string(),
            },
            Duration::from_secs(3600),
        )
        .with_subject(uid);
        let jwt = key.sign::<OAuthToken>(token).unwrap();
        let jwk = serde_json::to_string(&jwk).unwrap();
        let subject =
            verify_oauth_token_scopes(&jwk, &jwt, "read:test write-only:test", "GET").unwrap();
        assert_eq!(&subject, uid);
        assert!(verify_oauth_token_scopes(&jwk, &jwt, "read:test write-only:test", "POST").is_err());
        assert!(verify_oauth_token_scopes(&jwk, &jwt, "write-only:foo", "POST").is_ok());
    }

    fn mk_jwk(key: Ed25519PublicKey) -> Jwk {
        let x = base64::prelude::BASE64_URL_SAFE_NO_PAD.encode(&key.to_bytes());
        Jwk {
            common: CommonParameters::default(),
            algorithm: AlgorithmParameters::OctetKeyPair(OctetKeyPairParameters {
                key_type: OctetKeyPairType::OctetKeyPair,
                curve: EdwardCurve::Ed25519,
                x,
            }),
        }
    }
}
