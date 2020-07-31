{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Phone
  ( SMSMessage (..),
    PhoneException (..),
    sendCall,
    sendSms,

    -- * Validation
    validatePhone,

    -- * Unique Keys
    PhoneKey,
    mkPhoneKey,
    phoneKeyUniq,
    phoneKeyOrig,

    -- * Re-exports
    Phone (..),
  )
where

import Bilge.Retry (httpHandlers)
import Brig.App
import Brig.Budget
import Brig.Types
import Control.Lens (view)
import Control.Monad.Catch
import Control.Retry
import Data.Char (isSpace)
import Data.LanguageCodes
import qualified Data.Metrics as Metrics
import qualified Data.Text as Text
import Data.Time.Clock
import Imports
import Network.HTTP.Client (HttpException, Manager)
import qualified Ropes.Nexmo as Nexmo
import Ropes.Twilio (LookupDetail (..))
import qualified Ropes.Twilio as Twilio
import qualified System.Logger.Class as Log
import System.Logger.Message (field, msg, val, (~~))

-------------------------------------------------------------------------------
-- Sending SMS and Voice Calls

data SMSMessage = SMSMessage
  { smsFrom :: !Text,
    smsTo :: !Text,
    smsText :: !Text
  }

data PhoneException
  = PhoneNumberUnreachable
  | PhoneNumberBarred
  | PhoneBudgetExhausted NominalDiffTime
  deriving (Show, Typeable)

instance Exception PhoneException

sendCall :: Nexmo.Call -> AppIO ()
sendCall call = unless (isTestPhone $ Nexmo.callTo call) $ do
  m <- view httpManager
  cred <- view nexmoCreds
  withCallBudget (Nexmo.callTo call) $ do
    r <-
      liftIO . try @_ @Nexmo.CallErrorResponse . recovering x3 nexmoHandlers $
        const $
          Nexmo.sendCall cred m call
    case r of
      Left ex -> case Nexmo.caStatus ex of
        Nexmo.CallDestinationNotPermitted -> unreachable ex
        Nexmo.CallInvalidDestinationAddress -> unreachable ex
        Nexmo.CallUnroutable -> unreachable ex
        Nexmo.CallDestinationBarred -> barred ex
        _ -> throwM ex
      Right _ -> return ()
  where
    nexmoHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Nexmo.CallErrorResponse) ->
               return $ case Nexmo.caStatus ex of
                 Nexmo.CallThrottled -> True
                 Nexmo.CallInternal -> True
                 _ -> False
           ]
    unreachable :: Nexmo.CallErrorResponse -> AppT IO ()
    unreachable ex = warn (toException ex) >> throwM PhoneNumberUnreachable
    barred :: Nexmo.CallErrorResponse -> AppT IO ()
    barred ex = warn (toException ex) >> throwM PhoneNumberBarred
    warn ex =
      Log.warn $
        msg (val "Voice call failed.")
          ~~ field "error" (show ex)
          ~~ field "phone" (Nexmo.callTo call)

sendSms :: Locale -> SMSMessage -> AppIO ()
sendSms loc SMSMessage {..} = unless (isTestPhone smsTo) $ do
  m <- view httpManager
  withSmsBudget smsTo $ do
    -- We try Nexmo first (cheaper and specialised to SMS)
    f <- (sendNexmoSms m *> pure Nothing) `catches` nexmoFailed
    for_ f $ \ex -> do
      warn ex
      r <- try @_ @Twilio.ErrorResponse $ sendTwilioSms m
      case r of
        Left ex' -> case Twilio.errStatus ex' of
          -- Invalid "To" number for SMS
          14101 -> unreachable ex'
          -- 'To' number is not a valid mobile number
          21614 -> unreachable ex'
          -- "To" number is not currently reachable
          21612 -> unreachable ex'
          -- Customer replied with "STOP"
          21610 -> barred ex'
          -- A real problem
          _ -> throwM ex'
        Right () -> return ()
  where
    sendNexmoSms :: Manager -> AppIO ()
    sendNexmoSms mgr = do
      crd <- view nexmoCreds
      void . liftIO . recovering x3 nexmoHandlers $
        const $
          Nexmo.sendMessage crd mgr $
            Nexmo.Message "Wire" smsTo smsText (toNexmoCharset loc)
    toNexmoCharset :: Locale -> Nexmo.Charset
    toNexmoCharset l = case fromLanguage (lLanguage l) of
      RU -> Nexmo.UCS2
      AR -> Nexmo.UCS2
      UK -> Nexmo.UCS2
      FA -> Nexmo.UCS2
      TR -> Nexmo.UCS2
      ES -> Nexmo.UCS2
      ZH -> Nexmo.UCS2
      _ -> Nexmo.GSM7
    sendTwilioSms :: Manager -> AppIO ()
    sendTwilioSms mgr = do
      crd <- view twilioCreds
      void . liftIO . recovering x3 twilioHandlers $
        const $
          Twilio.sendMessage crd mgr (Twilio.Message smsFrom smsTo smsText)
    nexmoFailed =
      [ Handler $ \(ex :: HttpException) ->
          return (Just (SomeException ex)),
        Handler $ \(ex :: Nexmo.MessageErrorResponse) ->
          return (Just (SomeException ex))
      ]
    nexmoHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Nexmo.MessageErrorResponse) ->
               return $ case Nexmo.erStatus ex of
                 Nexmo.MessageThrottled -> True
                 Nexmo.MessageInternal -> True
                 Nexmo.MessageCommunicationFailed -> True
                 _ -> False
           ]
    twilioHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Twilio.ErrorResponse) ->
               return $ case Twilio.errStatus ex of
                 20429 -> True -- Too Many Requests
                 20500 -> True -- Internal Server Error
                 20503 -> True -- Temporarily Unavailable
                 _ -> False
           ]
    unreachable :: Twilio.ErrorResponse -> AppT IO ()
    unreachable ex = warn (toException ex) >> throwM PhoneNumberUnreachable
    barred :: Twilio.ErrorResponse -> AppT IO ()
    barred ex = warn (toException ex) >> throwM PhoneNumberBarred
    warn ex =
      Log.warn $
        msg (val "SMS failed.")
          ~~ field "error" (show ex)
          ~~ field "phone" smsTo

-------------------------------------------------------------------------------
-- Phone Number Validation

-- | Validate a phone number. Returns the canonical
-- E.164 format of the given phone number on success.
validatePhone :: Phone -> AppIO (Maybe Phone)
validatePhone (Phone p)
  | isTestPhone p = return (Just (Phone p))
  | otherwise = do
    c <- view twilioCreds
    m <- view httpManager
    r <-
      liftIO . try @_ @Twilio.ErrorResponse $
        recovering x3 httpHandlers $
          const $
            Twilio.lookupPhone c m p LookupNoDetail Nothing
    case r of
      Right x -> return (Just (Phone (Twilio.lookupE164 x)))
      Left e | Twilio.errStatus e == 404 -> return Nothing
      Left e -> throwM e

isTestPhone :: Text -> Bool
isTestPhone = Text.isPrefixOf "+0"

--------------------------------------------------------------------------------
-- SMS Budgeting

smsBudget :: Budget
smsBudget =
  Budget
    { budgetTimeout = 3600 * 24, -- 24 hours
      budgetValue = 5 -- # of SMS within timeout
    }

withSmsBudget :: Text -> AppIO a -> AppIO a
withSmsBudget phone go = do
  let k = BudgetKey ("sms#" <> phone)
  r <- withBudget k smsBudget go
  case r of
    BudgetExhausted t -> do
      Log.info $
        msg (val "SMS budget exhausted.")
          ~~ field "phone" phone
      Metrics.counterIncr (Metrics.path "budget.sms.exhausted") =<< view metrics
      throwM (PhoneBudgetExhausted t)
    BudgetedValue a b -> do
      Log.debug $
        msg (val "SMS budget deducted.")
          ~~ field "budget" b
          ~~ field "phone" phone
      return a

--------------------------------------------------------------------------------
-- Voice Call Budgeting

callBudget :: Budget
callBudget =
  Budget
    { budgetTimeout = 3600 * 24 * 7, -- 7 days
      budgetValue = 2 -- # of voice calls within timeout
    }

withCallBudget :: Text -> AppIO a -> AppIO a
withCallBudget phone go = do
  let k = BudgetKey ("call#" <> phone)
  r <- withBudget k callBudget go
  case r of
    BudgetExhausted t -> do
      Log.info $
        msg (val "Voice call budget exhausted.")
          ~~ field "phone" phone
      Metrics.counterIncr (Metrics.path "budget.call.exhausted") =<< view metrics
      throwM (PhoneBudgetExhausted t)
    BudgetedValue a b -> do
      Log.debug $
        msg (val "Voice call budget deducted.")
          ~~ field "budget" b
          ~~ field "phone" phone
      return a

--------------------------------------------------------------------------------
-- Unique Keys

data PhoneKey = PhoneKey
  { -- | canonical form of 'phoneKeyOrig', without whitespace.
    phoneKeyUniq :: !Text,
    -- | phone number with whitespace.
    phoneKeyOrig :: !Phone
  }

instance Show PhoneKey where
  showsPrec _ = shows . phoneKeyUniq

instance Eq PhoneKey where
  (PhoneKey k _) == (PhoneKey k' _) = k == k'

mkPhoneKey :: Phone -> PhoneKey
mkPhoneKey orig =
  let uniq = Text.filter (not . isSpace) (fromPhone orig)
   in PhoneKey uniq orig

-------------------------------------------------------------------------------
-- Retry Settings

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
