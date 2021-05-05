{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordResetCode_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_PasswordResetCode_1 :: PasswordResetCode
testObject_PasswordResetCode_1 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("UKBMU4Dh8esUoiqzB3fJOT6Xexxgtfw9UD2a2DF-fBg=d=9mZu1y85B8xsgZP3alxHRb744oYkTKu589aMeNVHyNxMBDWuWthLnGQnUs2VdfoFt8z5yWUHs7o9ISvprD5QLkc7=Qhc4ujPOUS-o0PF8v62jXNAHnkJz54tf0=Ngo1AAUnmunxYpmEL-jWykDIlIW=yuurlE7KcUpNh5bb6xmnL1S6BiplYKdX0=2K_ad5924JG8yLOMj5qzdQWUEwO5VJ0oMIKJ6N3mwlFCeURKJPBPYxbvuzI9Qegfz-xGTWYzsf--OeeT8sTrMpcqwZ7s74JSdN96hD9YQ2iDNC=7uc=4cjKu2RrS6Es4x-=PNxrpgu0xsLaTP46oDEauQTJ=dxV4DcFrfkqcu24yrrozCH1L2tNWyIC59A=K0VqGnvxxwb2BGKZWo0fX_AtzeExnXYtd9F-wuC5afMA-Z081_bhymSN=3petzmiqRDVRKgxoCYYRRv0wgA2p5cdScKXeokpkDr6EhUsAi620IFFqFKP6bqGJG2KswZSBcxscI")))}
testObject_PasswordResetCode_2 :: PasswordResetCode
testObject_PasswordResetCode_2 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("jZq4c33cZiOIHnDAGudG0nYu7yD5qk-1TeERRpEfsmAsOZSS5BwO7=S8tBY_cedZ1L7dyqwNvUFwTSejMOWeWZi-2WqFBW_kGuXRixy=5NWuDPz=ThvlDpkh_1rzyrIWI3Qs-hbhsZClkZydpzZKACQ=fO1j5eUX7hXmxD=4Hs8TLlSEq5WJVq5ci2KBAw=B0_bJCRh7CSWJ7L-PKriUSILRGwqXiOPtVL0jKSuXw-_QUwpFCTjWY_99HIa1Xn5x1qHw4Rl6nJj8NQQwd5ONRlhO2lzh3xbk3RtXCE02iVmsHaRF4aOXBlsZulJXXV48ZuGcjzSpHOjciY=QiHEcw8=HbaNE01nHd1PAIyGrd_h1fvSWwIKgZeGO7cPa0oBwXciqYvam9NTQsBRnXDqAqZL352Q_KULbKJ-yON6gBoJDYA=qWfoAdJMLi_6RzZohxHjnF55aV")))}
testObject_PasswordResetCode_3 :: PasswordResetCode
testObject_PasswordResetCode_3 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("HkZsITA-dNp1C7km61fJz31B_ux0Z9WmrAjrkHHzNl4fChGP_nItAzo3n8YjFO7pGK-zd4kGK5qGaQQuxOAi-J8r1reLL-Ln-Z-YomG-WANwPgT6p0pHORZUihajVTj8dhUh_us7SAR6nht5RhTxKdokF2XxXa52fo0pYjy5fkb4hyTmf8Vg2TnYzB7HXGwWcjfWV8Xxr6wqE4KZ7G9xk7=fl_x=halWhDAHdUPjYBn08ZSOSxFG6qriRrFba6RYtIKKy9EOTeVTPJqWwuK782rlpZ9S_tWDk2njuXBeC03xPISRdxXAKOp=QcHaP6v71VqTuMhn_KxzsLOiWx94dLaYBW1TBgbq95_Q2lLqje7WFY7dZk4KBZr8qMrh1V-ruqY9ICTMBRDHktcpTARpRl8FtZJ1tjjHqVKoROwNHG-GQ_3Gbhen=QPY0ha7oz3boc2_8xnurzLhrqVjvbjk9MBgZdVn7oqxz60T9d1DpC0suucwwbZ=AITpg5G494SujazYR0uhSLBb5JlEkMciEGK-9zPhFaqMQqBMh-fXJv7Yn1rGmQkkvudpFQ4sWPkMWQVuvYiYD=SZ_ffX-jb5eFZKJz-i4NY2pB79QzHi5pyqf3a4sL_5zGBQ72_4ynzcxoUQRbh=2l8BAup2o_jYSSuLg19aRAXWpRzHOYuSSTYchVFRX4-hbbyn9tSZBGYCwc4kif1RKpnEKfXcC8xWGefOuvY59qq=pPO1bALwpr-_2jewnEkFkwYba7Twr10_3EN0Vrwk4fWca9KT02sFRyEkQu4t6O8kI=LgxU9RobRgkRXnho6LNBjvAz26EhNZ2lbMWcrrmjUeWJqOgI0h8=sRuvf=co-Fw_8ND6je9Ecvpbqk14ZY4JqWJRx6XL_mas1CwLIR19ba3Q8AhQhkN9ECl2VT6=M9P7ot=ye7Klu_A68OQkKSB_N4q90u")))}
testObject_PasswordResetCode_4 :: PasswordResetCode
testObject_PasswordResetCode_4 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("kFFlmRWbX9FQ3ZeRgu_wtXNBAQXOFTpZxwlA6B_l=rFzfMwqYYbKL5ZMaGu32G0zD_vlG7qtg5vXUBnGxYpFlwlyYvfuGRNsG6cgXnWElv-LuojsUaTCIp0wMdCHz15DGJhJAjDNjcLkFEQ_ezI3k52=6CVZTxIaSRST7q6Yt=Ixj4cqDAJ-XZvX_stbbfOfXKL1ghr4Dh_C-SFqj8rT5aj0xd6cEy5v-jwovEgXFOesPX08Uw6yRFzzq2_JtAkUp19AQYyTZSIvywmo4KCNOj1-=WWYJ01NZtXinqXkgdP1NTd--REgkeBOeBtoIXiVqXpYAcFXTUgNLo4WllL_gJ0_qA0K6K=mTHb1wxDbLR7aZH1hAU75iB")))}
testObject_PasswordResetCode_5 :: PasswordResetCode
testObject_PasswordResetCode_5 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("8cE6DsnRAofpkx9eUBIgntyG9ngjy-CKEAv1X40m3vriWsr6jT7CMt59NQJcftOdDVmXm3j-8vKMbQri5BuP6nOHJVjbwAYBWj1zLclp9f6LoVRNVsJ6zmnk5aZb3l1TdJq37losRxksIYoEyc0wz8N21Rzdxh")))}
