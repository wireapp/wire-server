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
testObject_PasswordResetCode_user_1 :: PasswordResetCode
testObject_PasswordResetCode_user_1 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("Y7CWjDGc20ROHwy1MVQRdM1=_nqMMdAlSUs9wmWquXPW6NNqm6KqR3DGN8kae3nGkihdWQWII5c__YPHJ86QiYUnOeEcKhIkUaExsCSZzsuMMcq7jgF09o2coMrLzAA21I03QBqCmb4RzaZkLluCaq1Ubt6ArGdCXxrdadvmYvpoorEiDkf3TkX9DnBl=s=YqZFrPiNUqmm29c36WZ0iLlj9WZJeA9SPSY_0tnTlj=e6OLuqoq=hLbrt2gRfq70PiQ1rhFBlBkG7DgaljABwDJ5YNorQA5=KlubKHt3s9YeuQ-5Y=UZPl6Rr7Rg2NMiNkV3SmLVxpmGVtShmuZp9-MPj5IQqCKhFyagJ0jota2LVqO2u2sT2Ky4nHZS8wW06tFmLt3ntdm2_hEJZ0O6_Z4W4hlpUREbG625f5ijd_D4ndr5CQ1ZD8OMd_SZCD72VHTqgecdS97JPXfOYSw-cBL8dGrdoa6hH0R4O-XeZ88-HXAV0YaJzg5IYXcCyoK2lU=FHyH00cjZagq11eJr4tAzKeEEh-mQJzgNJY4dwCVSK3OvJRq7NNGawnt0G03jK-xYbyTAV2psOfGGCbiaIajtqjoib60afcfMT4nFB0EcfhAI9JHcSEOVfE2lUW0D4Jbsj=Dm8199tsQQgzjBRQ-NTfPP-50IRWm1DQGAG=5laMlC0baIT=cMpc8B0A-hIW1zLIpEZN2dIBzkT_iw7Fck2g7HvYbxoiMiuKJABHYjDlvOC4nKF-w7sFo4FZNwh")))}
testObject_PasswordResetCode_user_2 :: PasswordResetCode
testObject_PasswordResetCode_user_2 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("ZBo2PJz1zo6jwxCSIyz=CdbJK6FVAj0bRkNYY_ftJc9dnBCyt1vPt3Q9AH-AzVrXfxozVBlCL5RxB5qLmoP4UE_rBCxZvWiiB_6Vmdyc=Cncd1DyPohbwJ9S-ocqV88uu1E1rEkBEFG98Jlc36p41hVawXCmIoMLn1DYFMZgSTcF8lebzWITUaYKqFiR=zEU3Yre65DKlbUbmmJR8Mb_whvApEM1kRNFnkokhBynSZvvbzpq2GELr60eO=aHCBoizMEdRZmscgpve9Iu94QB_0m2t")))}
testObject_PasswordResetCode_user_3 :: PasswordResetCode
testObject_PasswordResetCode_user_3 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("WvRvIhwO49r")))}
testObject_PasswordResetCode_user_4 :: PasswordResetCode
testObject_PasswordResetCode_user_4 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("Usz7jj1B8T-90njpstgmNy=v9d0t4rtqcutpbmqVmQQDRUdlkM62JtV0VH_PXIgVMgt87Ruckvjg=r2A5Qxnc3U1J4j6VtRj8ReF2kXWLmbalnOXeGBt2j6t_3dj8cBKDWiAv_SOYM_6MKE9HYOEgHp-olk9P_0HBPCbesFzDRM5S=DSh0Zi3WTJy=aFamjN6gh=Dyo0hkXa5f8husEcY6KiQhNnYXVB8Aa8VOCWsfvkTELFoiGANbFwMKuz=tijDQU4j=Fu_4h0RdudCx=2Y3xFf6ktsHKHyS_f2")))}
testObject_PasswordResetCode_user_5 :: PasswordResetCode
testObject_PasswordResetCode_user_5 = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("oELDXd2503445H4aBawhSifXYiwOtTRuOuQq0SfJ8gI2dzczXUcGl-V-01E3W1hJqyqi3XRAvLJ3zC9ZEHbxagtPGTEfRwpJMX=6zc3rXuROFVrOttRSbG4xwDNL4msRhICOAQKT8YbwOUQIk0wp=dJ16C0XacQw5-UXgmvKhs6-3pF0yU2PA8ibkNAx-UrlmHjYIoFGX7xeB5FMujsOa26z_EsbBX-KVXGTLKL8WJZ6_En9ZfHTM1f-1i6-YZJJmqWTd71N8EbFG2_6wqmaQQ4lO3wYwTJ=C21T1OKf=tot0de_EQN7SzHwDQvI5DQ6n3B9j2VPzCJbkkur=MahKLhHdaoubT3-DCmq0Mf0iKj7omsANFcF0_=GCoVyc9U7n_M2WI3HN0YlDXxtZTvZTmChvrQj=HWN-PoG1YArdRstyuBqiHVvMddW-WSl244aqOe2fRwyqGec-Xqf7x=QZejNf1s_=KCC0a3UY10xtm4DO8ve7cSgYaa7KwpFD3Tz5Epm4eOE14G24oR2WS5Z4211NGwXpr_HTViIWPwPvabcgZ1ZJhmYNFZ1B1w=mhlaQTTjscgp24gRtY6pgoBTlzEelsa40HcvD5_qmUk-g64BxNDwsKGvnVCWf6qhp9ZPm9reqOxkWu56D4XW3wKhUgS_k4UKsHL8-qlgWcqPLcR-Mcn2KCSJwZ=eoHGsyki-Nm63VGmlG3JW26EMZ0z3OOrm06tjFLOT6A_-QEVN2nxuuNGY7ACf3QTe=cmb7Xifv1r0eZXo5LWj0YU3CPKigmsccxO9m95SDyXhQNX8QggjGas-e-ArUKANpLYnJNXkr2xPAKpanWuk4Mo1cpB51gUfLbLOxKwOXLmm5aSH10t1kiFUEUGfXoq0gyb7Zdzym10N-wRIG1-K2SUhGiLONpPsIXVNGkxUBi6is1jI-l6rHDCY2njp94fj8dkj5bPWDLXRot77nC2V5uBWHjQ")))}
