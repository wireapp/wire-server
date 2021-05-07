{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.Cookie_20_28_29_user where

import Imports (Maybe (Just, Nothing), read)
import Wire.API.User.Auth
  ( Cookie (Cookie),
    CookieId (CookieId, cookieIdNum),
    CookieLabel (CookieLabel, cookieLabelText),
    CookieType (PersistentCookie, SessionCookie),
  )

testObject_Cookie_20_28_29_user_1 :: Cookie ()
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-13 05:47:44.953325209615 UTC")) (read ("1864-05-05 23:11:41.080048429153 UTC")) (Nothing) (Nothing) (()))

testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-11 05:25:35.472438946148 UTC")) (read ("1864-05-13 13:29:31.539239953694 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))

testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 06:32:09.653354599176 UTC")) (read ("1864-05-07 07:38:14.515001504525 UTC")) (Just (CookieLabel {cookieLabelText = "\"\ETB\ETX"})) (Just (CookieId {cookieIdNum = 1})) (()))

testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-12 17:39:22.647800906939 UTC")) (read ("1864-05-08 21:05:44.689352987872 UTC")) (Just (CookieLabel {cookieLabelText = "\SOH\STX"})) (Just (CookieId {cookieIdNum = 0})) (()))

testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-05 18:31:27.854562456661 UTC")) (read ("1864-05-07 20:47:39.585530890253 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))

testObject_Cookie_20_28_29_user_6 :: Cookie ()
testObject_Cookie_20_28_29_user_6 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-09 21:11:41.006743014266 UTC")) (read ("1864-05-11 13:07:04.231169675877 UTC")) (Just (CookieLabel {cookieLabelText = "x"})) (Just (CookieId {cookieIdNum = 0})) (()))

testObject_Cookie_20_28_29_user_7 :: Cookie ()
testObject_Cookie_20_28_29_user_7 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-10 10:07:45.191235538251 UTC")) (read ("1864-05-08 11:48:36.288367238761 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 3})) (()))

testObject_Cookie_20_28_29_user_8 :: Cookie ()
testObject_Cookie_20_28_29_user_8 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-13 23:20:18.620984948327 UTC")) (read ("1864-05-10 17:19:51.999573387671 UTC")) (Just (CookieLabel {cookieLabelText = "W\1095116"})) (Nothing) (()))

testObject_Cookie_20_28_29_user_9 :: Cookie ()
testObject_Cookie_20_28_29_user_9 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-10 21:07:17.237535753229 UTC")) (read ("1864-05-07 13:26:23.632337100061 UTC")) (Just (CookieLabel {cookieLabelText = "_"})) (Just (CookieId {cookieIdNum = 3})) (()))

testObject_Cookie_20_28_29_user_10 :: Cookie ()
testObject_Cookie_20_28_29_user_10 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-05 13:10:26.655350748893 UTC")) (read ("1864-05-11 07:40:26.20362225993 UTC")) (Just (CookieLabel {cookieLabelText = "@\129045f"})) (Just (CookieId {cookieIdNum = 2})) (()))

testObject_Cookie_20_28_29_user_11 :: Cookie ()
testObject_Cookie_20_28_29_user_11 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-05 18:46:43.751100514127 UTC")) (read ("1864-05-05 20:09:58.51051779151 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 2})) (()))

testObject_Cookie_20_28_29_user_12 :: Cookie ()
testObject_Cookie_20_28_29_user_12 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-08 10:13:20.99278185582 UTC")) (read ("1864-05-13 09:17:06.972542913972 UTC")) (Just (CookieLabel {cookieLabelText = "0i"})) (Just (CookieId {cookieIdNum = 1})) (()))

testObject_Cookie_20_28_29_user_13 :: Cookie ()
testObject_Cookie_20_28_29_user_13 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-08 13:32:34.77859094095 UTC")) (read ("1864-05-11 23:26:06.481608900736 UTC")) (Just (CookieLabel {cookieLabelText = "\SI"})) (Just (CookieId {cookieIdNum = 2})) (()))

testObject_Cookie_20_28_29_user_14 :: Cookie ()
testObject_Cookie_20_28_29_user_14 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-13 05:03:36.689760525241 UTC")) (read ("1864-05-13 09:20:52.214909900547 UTC")) (Just (CookieLabel {cookieLabelText = "\a5"})) (Just (CookieId {cookieIdNum = 2})) (()))

testObject_Cookie_20_28_29_user_15 :: Cookie ()
testObject_Cookie_20_28_29_user_15 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-13 15:06:06.162467079651 UTC")) (read ("1864-05-07 20:56:24.910663768998 UTC")) (Nothing) (Nothing) (()))

testObject_Cookie_20_28_29_user_16 :: Cookie ()
testObject_Cookie_20_28_29_user_16 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-11 01:41:37.159116274364 UTC")) (read ("1864-05-08 08:29:26.712811058187 UTC")) (Nothing) (Nothing) (()))

testObject_Cookie_20_28_29_user_17 :: Cookie ()
testObject_Cookie_20_28_29_user_17 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-12 11:59:56.901830591377 UTC")) (read ("1864-05-10 21:32:23.833192157326 UTC")) (Just (CookieLabel {cookieLabelText = "\13875"})) (Nothing) (()))

testObject_Cookie_20_28_29_user_18 :: Cookie ()
testObject_Cookie_20_28_29_user_18 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-13 18:38:28.752407147796 UTC")) (read ("1864-05-12 15:17:29.299354245486 UTC")) (Just (CookieLabel {cookieLabelText = "\1070053"})) (Just (CookieId {cookieIdNum = 0})) (()))

testObject_Cookie_20_28_29_user_19 :: Cookie ()
testObject_Cookie_20_28_29_user_19 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-13 07:03:36.619050229877 UTC")) (read ("1864-05-10 10:06:17.906037443659 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 3})) (()))

testObject_Cookie_20_28_29_user_20 :: Cookie ()
testObject_Cookie_20_28_29_user_20 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-13 12:22:12.980555635796 UTC")) (read ("1864-05-06 11:24:34.525397249315 UTC")) (Just (CookieLabel {cookieLabelText = "\1081398\&0\DC4W"})) (Just (CookieId {cookieIdNum = 0})) (()))
