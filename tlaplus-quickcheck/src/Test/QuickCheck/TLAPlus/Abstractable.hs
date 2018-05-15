{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Test.QuickCheck.TLAPlus.Abstractable where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.DList as D
import Data.List  as L
import Data.Map   as M
import Data.Monoid
import Data.Text
import Data.Proxy
import           GHC.Generics hiding (from,to)
import qualified GHC.Generics
import Language.TLAPlus.Eval
import Language.TLAPlus.Syntax

class GAbstractable a where
    gToTLAValue :: a p -> DList (String,VA_Value)
    gFromTLAValue :: (String -> Maybe VA_Value) -> Maybe (a p)

withProxy :: (S1 a Proxy b -> Maybe (S1 a p b)) -> Maybe (S1 a p b)
withProxy g = g (M1 Proxy)

instance (Abstractable c,Selector a) => GAbstractable (S1 a (K1 b c)) where
    gFromTLAValue m = withProxy $ \p ->
      M1 . K1 <$> (fromTLAValue =<< m (selName p))
    gToTLAValue v@(M1 (K1 x)) = D.singleton (selName v, toTLAValue x)
instance GAbstractable c => GAbstractable (C1 b c) where
    gFromTLAValue = fmap M1 . gFromTLAValue
    gToTLAValue (M1 x) = gToTLAValue x
instance GAbstractable c => GAbstractable (D1 b c) where
    gFromTLAValue = fmap M1 . gFromTLAValue
    gToTLAValue (M1 x) = gToTLAValue x
instance (GAbstractable a,GAbstractable b) => GAbstractable (a :*: b) where
    gFromTLAValue m = (:*:) <$> gFromTLAValue m <*> gFromTLAValue m
    gToTLAValue (x :*: y) = gToTLAValue x <> gToTLAValue y

_List :: Ord k' => Iso (Map k a) (Map k' a') [(k,a)] [(k',a')]
_List = iso M.toList M.fromList

instance (Ord a,Abstractable a,Abstractable b) => Abstractable (Map a b) where
    toTLAValue = VA_Map . (_List.traverse %~ bimap toTLAValue toTLAValue)
    fromTLAValue = (_List.traverse) (bitraverse fromTLAValue fromTLAValue) <=< preview _VA_Map
    toTLARec = M.singleton "value" . toTLAValue
instance Abstractable VA_Value where
    toTLAValue = id
    fromTLAValue = Just
    toTLARec = M.singleton "value" . toTLAValue
instance Abstractable Text where
    toTLAValue = VA_String . unpack
    fromTLAValue = preview $ _VA_String . to pack
    toTLARec = M.singleton "value" . toTLAValue

instance Abstractable Int where
    toTLAValue = VA_Int
    fromTLAValue = preview _VA_Int
    toTLARec = M.singleton "value" . toTLAValue

class Abstractable a where
    toTLAValue :: a -> VA_Value
    default toTLAValue :: (Generic a,GAbstractable (Rep a)) => a -> VA_Value
    toTLAValue = VA_Map . M.fromList . L.map (first VA_String)
                        . D.toList . gToTLAValue . GHC.Generics.from
    fromTLAValue :: VA_Value -> Maybe a
    default fromTLAValue :: (Generic a,GAbstractable (Rep a)) => VA_Value -> Maybe a
    fromTLAValue = pure . GHC.Generics.to
        <=< gFromTLAValue . (\m k -> M.lookup (VA_String k) m)
        <=< preview _VA_Map
    toTLARec :: a -> Map String VA_Value
    default toTLARec :: (Generic a,GAbstractable (Rep a)) => a -> Map String VA_Value
    toTLARec = M.fromList . D.toList . gToTLAValue . GHC.Generics.from

toStateEnv :: Abstractable a => a -> Env
toStateEnv = L.map (first $ Unprimed []) . M.toList . toTLARec

toStateEnv' :: Abstractable a => a -> Env
toStateEnv' = L.map (first $ Primed []) . M.toList . toTLARec

toActionEnv :: Abstractable a => a -> Env
toActionEnv x = toStateEnv x ++ toStateEnv' x
