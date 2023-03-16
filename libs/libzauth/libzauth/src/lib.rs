// This file is part of the Wire Server implementation.
//
// Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

extern crate asexp;
extern crate lazy_static;
extern crate regex;
extern crate rustc_serialize;
extern crate sodiumoxide;
extern crate jwt_simple;
extern crate serde_json;
extern crate serde;
extern crate thiserror;
extern crate base64;

pub mod acl;
pub mod error;
pub mod zauth;
pub mod oauth;

mod matcher;

pub use acl::Acl;
pub use error::Error;
pub use zauth::{Keystore, Token, TokenType, TokenVerification};
pub use oauth::{verify_oauth_token, OauthError};
