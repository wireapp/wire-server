{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Cassandra (MonadClient)
import Control.Lens (view)
import Control.Monad.Catch
import Control.Retry
import Data.LanguageCodes
import Data.Text qualified as Text
import Data.Time.Clock
import Imports
import Network.HTTP.Client (HttpException, Manager)
import Prometheus qualified as Prom
import Ropes.Nexmo qualified as Nexmo
import Ropes.Twilio (LookupDetail (..))
import Ropes.Twilio qualified as Twilio
import System.Logger.Class qualified as Log
import System.Logger.Message (field, msg, val, (~~))
import Wire.API.User

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

sendCall ::
  (MonadClient m, MonadReader Env m, Log.MonadLogger m, Prom.MonadMonitor m) =>
  Nexmo.Call ->
  m ()
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
      Right _ -> pure ()
  where
    nexmoHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Nexmo.CallErrorResponse) ->
               pure $ case Nexmo.caStatus ex of
                 Nexmo.CallThrottled -> True
                 Nexmo.CallInternal -> True
                 _ -> False
           ]
    unreachable ex = warn (toException ex) >> throwM PhoneNumberUnreachable
    barred ex = warn (toException ex) >> throwM PhoneNumberBarred
    warn ex =
      Log.warn $
        msg (val "Voice call failed.")
          ~~ field "error" (show ex)
          ~~ field "phone" (Nexmo.callTo call)

sendSms ::
  ( MonadClient m,
    MonadCatch m,
    Log.MonadLogger m,
    MonadReader Env m,
    Prom.MonadMonitor m
  ) =>
  Locale ->
  SMSMessage ->
  m ()
sendSms loc SMSMessage {..} = unless (isTestPhone smsTo) $ do
  m <- view httpManager
  withSmsBudget smsTo $ do
    -- We try Nexmo first (cheaper and specialised to SMS)
    f <- (sendNexmoSms m $> Nothing) `catches` nexmoFailed
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
        Right () -> pure ()
  where
    sendNexmoSms :: (MonadIO f, MonadReader Env f) => Manager -> f ()
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
    sendTwilioSms :: (MonadIO f, MonadReader Env f) => Manager -> f ()
    sendTwilioSms mgr = do
      crd <- view twilioCreds
      void . liftIO . recovering x3 twilioHandlers $
        const $
          Twilio.sendMessage crd mgr (Twilio.Message smsFrom smsTo smsText)
    nexmoFailed =
      [ Handler $ \(ex :: HttpException) ->
          pure (Just (SomeException ex)),
        Handler $ \(ex :: Nexmo.MessageErrorResponse) ->
          pure (Just (SomeException ex))
      ]
    nexmoHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Nexmo.MessageErrorResponse) ->
               pure $ case Nexmo.erStatus ex of
                 Nexmo.MessageThrottled -> True
                 Nexmo.MessageInternal -> True
                 Nexmo.MessageCommunicationFailed -> True
                 _ -> False
           ]
    twilioHandlers =
      httpHandlers
        ++ [ const . Handler $ \(ex :: Twilio.ErrorResponse) ->
               pure $ case Twilio.errStatus ex of
                 20429 -> True -- Too Many Requests
                 20500 -> True -- Internal Server Error
                 20503 -> True -- Temporarily Unavailable
                 _ -> False
           ]
    unreachable ex = warn (toException ex) >> throwM PhoneNumberUnreachable
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
validatePhone :: (MonadClient m, MonadReader Env m) => Phone -> m (Maybe Phone)
validatePhone (Phone p)
  | isTestPhone p = pure (Just (Phone p))
  | otherwise = do
      c <- view twilioCreds
      m <- view httpManager
      r <-
        liftIO . try @_ @Twilio.ErrorResponse $
          recovering x3 httpHandlers $
            const $
              Twilio.lookupPhone c m p LookupNoDetail Nothing
      case r of
        Right x -> pure (Just (Phone (Twilio.lookupE164 x)))
        Left e | Twilio.errStatus e == 404 -> pure Nothing
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

withSmsBudget ::
  ( MonadClient m,
    Log.MonadLogger m,
    Prom.MonadMonitor m
  ) =>
  Text ->
  m a ->
  m a
withSmsBudget phone go = do
  let k = BudgetKey ("sms#" <> phone)
  r <- withBudget k smsBudget go
  case r of
    BudgetExhausted t -> do
      Log.info $
        msg (val "SMS budget exhausted.")
          ~~ field "phone" phone
      Prom.incCounter smsBudgetExhaustedCounter
      throwM (PhoneBudgetExhausted t)
    BudgetedValue a b -> do
      Log.debug $
        msg (val "SMS budget deducted.")
          ~~ field "budget" b
          ~~ field "phone" phone
      pure a

--------------------------------------------------------------------------------
-- Voice Call Budgeting

callBudget :: Budget
callBudget =
  Budget
    { budgetTimeout = 3600 * 24 * 7, -- 7 days
      budgetValue = 2 -- # of voice calls within timeout
    }

withCallBudget ::
  ( MonadClient m,
    Log.MonadLogger m,
    Prom.MonadMonitor m
  ) =>
  Text ->
  m a ->
  m a
withCallBudget phone go = do
  let k = BudgetKey ("call#" <> phone)
  r <- withBudget k callBudget go
  case r of
    BudgetExhausted t -> do
      Log.info $
        msg (val "Voice call budget exhausted.")
          ~~ field "phone" phone
      Prom.incCounter callBudgetExhaustedCounter
      throwM (PhoneBudgetExhausted t)
    BudgetedValue a b -> do
      Log.debug $
        msg (val "Voice call budget deducted.")
          ~~ field "budget" b
          ~~ field "phone" phone
      pure a

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

-------------------------------------------------------------------------------
-- Metrics

{-# NOINLINE callBudgetExhaustedCounter #-}
callBudgetExhaustedCounter :: Prom.Counter
callBudgetExhaustedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "budget.call.exhausted",
          Prom.metricHelp = "Number of times budget for calls got exhausted"
        }

{-# NOINLINE smsBudgetExhaustedCounter #-}
smsBudgetExhaustedCounter :: Prom.Counter
smsBudgetExhaustedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "budget.sms.exhausted",
          Prom.metricHelp = "Number of times budget for sending SMS got exhausted"
        }
