{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TurnURI_user where
import Data.Misc ( IpAddr(IpAddr) )
import Imports ( Maybe(Just, Nothing), read )
import Wire.API.Call.Config
    ( TurnURI,
      turnURI,
      Scheme(SchemeTurn, SchemeTurns),
      Transport(TransportUDP, TransportTCP),
      TurnHost(TurnHostName, TurnHostIp) )

testObject_TurnURI_user_1 :: TurnURI
testObject_TurnURI_user_1 = (turnURI (SchemeTurn) (TurnHostName "a-c") (read "6") (Just TransportUDP))
testObject_TurnURI_user_2 :: TurnURI
testObject_TurnURI_user_2 = (turnURI (SchemeTurn) (TurnHostName "123") (read "6") (Just TransportUDP))
testObject_TurnURI_user_3 :: TurnURI
testObject_TurnURI_user_3 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "152.68.214.63"))) (read "4") (Nothing))
testObject_TurnURI_user_4 :: TurnURI
testObject_TurnURI_user_4 = (turnURI (SchemeTurn) (TurnHostName "host.name") (read "4") (Just TransportTCP))
testObject_TurnURI_user_5 :: TurnURI
testObject_TurnURI_user_5 = (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP))
testObject_TurnURI_user_6 :: TurnURI
testObject_TurnURI_user_6 = (turnURI (SchemeTurns) (TurnHostName "a-c") (read "3") (Just TransportTCP))
testObject_TurnURI_user_7 :: TurnURI
testObject_TurnURI_user_7 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "178.197.109.141"))) (read "3") (Nothing))
testObject_TurnURI_user_8 :: TurnURI
testObject_TurnURI_user_8 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "8.111.84.240"))) (read "1") (Just TransportTCP))
testObject_TurnURI_user_9 :: TurnURI
testObject_TurnURI_user_9 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "168.109.82.167"))) (read "6") (Just TransportUDP))
testObject_TurnURI_user_10 :: TurnURI
testObject_TurnURI_user_10 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "63.20.176.208"))) (read "6") (Nothing))
testObject_TurnURI_user_11 :: TurnURI
testObject_TurnURI_user_11 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "147.17.78.174"))) (read "4") (Nothing))
testObject_TurnURI_user_12 :: TurnURI
testObject_TurnURI_user_12 = (turnURI (SchemeTurn) (TurnHostName "007.com") (read "8") (Just TransportUDP))
testObject_TurnURI_user_13 :: TurnURI
testObject_TurnURI_user_13 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "201.189.49.151"))) (read "7") (Just TransportUDP))
testObject_TurnURI_user_14 :: TurnURI
testObject_TurnURI_user_14 = (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
testObject_TurnURI_user_15 :: TurnURI
testObject_TurnURI_user_15 = (turnURI (SchemeTurns) (TurnHostName "a-c") (read "7") (Nothing))
testObject_TurnURI_user_16 :: TurnURI
testObject_TurnURI_user_16 = (turnURI (SchemeTurn) (TurnHostName "123") (read "5") (Just TransportTCP))
testObject_TurnURI_user_17 :: TurnURI
testObject_TurnURI_user_17 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "51.19.249.192"))) (read "0") (Just TransportUDP))
testObject_TurnURI_user_18 :: TurnURI
testObject_TurnURI_user_18 = (turnURI (SchemeTurn) (TurnHostName "123") (read "6") (Nothing))
testObject_TurnURI_user_19 :: TurnURI
testObject_TurnURI_user_19 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "30.214.101.194"))) (read "1") (Just TransportTCP))
testObject_TurnURI_user_20 :: TurnURI
testObject_TurnURI_user_20 = (turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportUDP))
