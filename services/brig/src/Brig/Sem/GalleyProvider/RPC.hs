{-# OPTIONS_GHC -Wno-unused-matches #-}

module Brig.Sem.GalleyProvider.RPC where

import Brig.Sem.GalleyProvider
import Brig.Sem.RPC
import Polysemy
import Imports
import Bilge (Request)


interpretGalleyProviderToRPC :: Member RPC r => Request -> Sem (GalleyProvider ': r) a -> Sem r a
interpretGalleyProviderToRPC req = interpret undefined
   -- CreateSelfConv uid                         -> undefined
   -- CreateLocalConnectConv qwt qwt' m_txt m_ci -> undefined
   -- AcceptLocalConnectConv qwt m_ci uid        -> undefined
   -- BlockLocalConv qwt m_ci uid                -> undefined
   -- UnblockLocalConv qwt m_ci uid              -> undefined
   -- GetConv uid id'                            -> undefined
   -- GetTeamConv uid id' id2                    -> undefined
   -- RmUser uid ass                             -> undefined
   -- NewClient uid ci                           -> undefined
   -- RmClient uid ci                            -> undefined
   -- CheckUserCanJoinTeam uid                   -> undefined
   -- AddTeamMember uid id' x0                   -> undefined
   -- CreateTeam uid bnt id'                     -> undefined
   -- GetTeamMember uid id'                      -> undefined
   -- GetTeamMembers uid                         -> undefined
   -- GetTeamContacts uid                        -> undefined
   -- GetTeamId uid                              -> undefined
   -- GetTeam uid                                -> undefined
   -- GetTeamName uid                            -> undefined
   -- GetTeamLegalHoldStatus uid                 -> undefined
   -- GetTeamSearchVisibility uid                -> undefined
   -- ChangeTeamStatus uid ts m_al               -> undefined
