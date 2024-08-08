{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.FeatureTH where

import Data.Kind
import Generics.SOP.TH
import Imports
import Language.Haskell.TH hiding (Type)
import Wire.API.Team.Feature

featureCases :: ExpQ -> Q Exp
featureCases rhsQ = do
  rhs <- rhsQ
  TyConI (DataD _ _ _ _ constructors _) <- reify ''FeatureSingleton
  pure $
    LamCaseE
      [ Match (ConP c [] []) (NormalB rhs) []
        | GadtC [c] _ _ <- constructors
      ]

generateTupleP :: Q [Dec]
generateTupleP = do
  let maxSize = 64 :: Int
  tylist <- [t|[Type]|]
  let vars = [VarT (mkName ("a" <> show i)) | i <- [0 .. maxSize - 1]]
  pure
    [ ClosedTypeFamilyD
        (TypeFamilyHead (mkName "TupleP") [KindedTV (mkName "xs") () tylist] NoSig Nothing)
        [ TySynEqn
            Nothing
            ( ConT (mkName "TupleP")
                `AppT` mkPattern (take n vars)
            )
            (mkTuple (take n vars))
          | n <- [0 .. maxSize]
        ]
    ]
  where
    mkPattern = foldr (\x y -> PromotedConsT `AppT` x `AppT` y) PromotedNilT

    mkTuple [] = ConT ''()
    mkTuple [v] = ConT ''Identity `AppT` v
    mkTuple vs =
      let n = length vs
       in foldl' AppT (TupleT n) vs

-- | generates some of the remaining @SOP.Generic@ instances as orphans
--   it is cut off at 50 on purpose to reduce compilation times
--   you may increase up to 64 which is the number at which you
--   you should probably start fixing cql instead.
generateSOPInstances :: Q [Dec]
generateSOPInstances = concat <$> traverse (deriveGeneric . tupleTypeName) [31 .. 50]
