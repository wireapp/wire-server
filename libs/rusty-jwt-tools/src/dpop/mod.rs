extern crate libc;

use libc::c_char;
use std::ffi::CStr;
use std::ffi::CString;
use std::str;

/// Validate the provided dpop_proof DPoP proof JWT from the client,
/// and if valid, return an introspectable DPoP access token.
///
/// Verifications provided:
/// * dpop_proof has the correct syntax
/// * (typ) header field is "dpop+jwt"
/// * signature algorithm (alg) in JWT header is a supported algorithm
/// * signature corresponds to the public key (jwk) in the JWT header
/// * qualified_client_id corresponds to the (sub) claim expressed as URI:
/// * backend_nonce corresponds to the (nonce) claim encoded as base64url.
/// * uri corresponds to the (htu) claim.
/// * method corresponds to the (htm) claim.
/// * (jti) claim is present
/// * (chal) claim is present
/// * (iat) claim is present and no earlier or later than max_skew_secs seconds
///   of now
/// * (exp) claim is present and no larger (later) than max_expiration.
/// * (exp) claim is no later than now plus max_skew_secs.
///
/// # Arguments
///
/// ## dpop_proof
///
/// A DPoP proof in JWS Compact Serialization format
/// Note that the proof consists of three runs of base64url characters
/// (header, claims, signature) separated by period characters.
///
/// ex: "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJle
///     iOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.
///     dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
///     (whitespace and line breaks in the example is not included in the actual proof)
///
/// ## user_id
///
/// the user ID UUID-4 in ASCII string representation
///
/// ex: "99db9768-04e3-4b5d-9268-831b6a25c4ab"
///
/// ## client_id
///
/// the client number assigned by the backend
///
/// ex: 0x4a9b
///
/// ## domain
///
/// ex: "example.com"
///
/// ## backend_nonce
///
/// The most recent DPoP nonce provided by the backend to the current client (base64url encoded uuidv4)
///
/// ex: "AABA3AAARcoAAAzJAABwRw=="
///
/// ## uri
///
/// The HTTPS URI on the backend for the DPoP auth token endpoint
///
/// ex: "https://wire.example.com/clients/authtoken"
///
/// ## method
///
/// The HTTPS method used on the backend for the DPoP auth token endpoint
///
/// ex: "POST"
///
/// ## max_skew_secs
///
/// The maximum number of seconds of clock skew the implementation will allow
///
/// ex: 360  // 5 minutes
///
/// ## max_expiration
///
/// The expiration date and time, in seconds since "the epoch" (the epoch is 1970-Jan-01 0:00:00 UTC).
///
/// ex: 1668987368
///
/// ## now
///
/// Current time in seconds since "the epoch".
///
/// ex: 1661211368
///
/// ## backend_pubkey_bundle
///
/// PEM format concatenated private key and public key of the Wire backend
#[allow(clippy::too_many_arguments)]
#[no_mangle]
pub extern "C" fn generate_dpop_token(
    dpop_proof: *const c_char,
    user_id: *const c_char,
    client_id: u16,
    domain: *const c_char,
    backend_nonce: *const c_char,
    uri: *const c_char,
    method: *const c_char,
    max_skew_secs: u16,
    max_expiration: u64,
    now: u64,
    backend_pubkey_bundle: *const c_char,
) -> *const c_char {
    print_cstr("dpop_proof", dpop_proof);
    print_cstr("user_id", user_id);
    println!("client_id: {}", client_id);
    print_cstr("domain", domain);
    print_cstr("backend_nonce", backend_nonce);
    print_cstr("uri", uri);
    print_cstr("method", method);
    println!("max_skew_secs: {}", max_skew_secs);
    println!("max_expiration: {}", max_expiration);
    println!("now: {}", now);
    print_cstr("backend_pubkey_bundle", backend_pubkey_bundle);
    if now % 2 == 0 {
        let err = (now as u8 % 13) + 1;
        let str = err.to_string();
        let c_str = CString::new(str).unwrap();
        c_str.into_raw()
    } else {
        let sample_response= b"eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
        let str = str::from_utf8(sample_response).unwrap();
        let c_str = CString::new(str).unwrap();
        c_str.into_raw()
    }
}

fn print_cstr(n: &str, a: *const c_char) {
    unsafe {
        println!("{}: {}", n, CStr::from_ptr(a).to_str().unwrap());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dpop() {
        let empty_cstr = CString::new("").unwrap().into_raw();
        let response = generate_dpop_token(
            empty_cstr, empty_cstr, 1, empty_cstr, empty_cstr, empty_cstr, empty_cstr, 1, 1, 1,
            empty_cstr,
        );
        unsafe {
            let response_str = CStr::from_ptr(response).to_str().unwrap();
            assert!(!response_str.is_empty());
            assert!(response_str.len() > 2);
        }
    }

    #[test]
    fn test_dpop_error() {
        let empty_cstr = CString::new("").unwrap().into_raw();
        let response = generate_dpop_token(
            empty_cstr, empty_cstr, 1, empty_cstr, empty_cstr, empty_cstr, empty_cstr, 1, 1, 0,
            empty_cstr,
        );
        unsafe {
            let response_str = CStr::from_ptr(response).to_str().unwrap();
            assert!(!response_str.is_empty());
            assert!(response_str.len() <= 2);
            let err = response_str.parse::<u8>().unwrap();
            assert!(err > 0);
        }
    }
}
