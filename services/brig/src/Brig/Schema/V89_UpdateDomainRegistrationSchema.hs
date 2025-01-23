{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.Schema.V89_UpdateDomainRegistrationSchema
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 89 "add auth token hash column" $ do
    schema'
      [r|
        ALTER TABLE domain_registration ADD
          ( auth_token_hash blob,
            authorized_team uuid
          )
      |]
    schema'
      [r|
        CREATE TABLE IF NOT EXISTS domain_registration_challenge
          ( id uuid PRIMARY KEY,
            challenge_token_hash blob,
            domain text,
            dns_verification_token ascii
          )
      |]
