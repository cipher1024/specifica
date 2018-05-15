{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Map as M hiding (mapMaybe,map)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.TLAPlus.Parser
import Language.TLAPlus.Syntax
import Language.TLAPlus.Eval
import Test.Hspec hiding (example)
import Test.QuickCheck.TLAPlus.Abstractable
import Test.QuickCheck.TLAPlus.TH
import Text.Parsec.Error
import Text.Parsec.Pos

tlaImportSpec "example/bridge.tla"

example :: IO (Either ParseError AS_Spec)
example = parseFile tlaspec "example/bridge.tla"

instance Abstractable Bridge_State where
instance Abstractable Location where
    toTLAValue LeftLoc = VA_String "left"
    toTLAValue RightLoc = VA_String "right"
    fromTLAValue x = LeftLoc <$ x^?_VA_String.only "left" <|>
                     RightLoc <$ x^?_VA_String.only "right"
    toTLARec = M.singleton "value" . toTLAValue

data Location = LeftLoc | RightLoc
    deriving (Eq,Show,Generic)

data Bridge_State
    = Bridge_State {loc :: Map Text Location,
                    torch :: Location,
                    time :: Int }
    deriving (Eq,Show,Generic)

bridgeInit :: Bridge_State -> Either EvalError Bool
bridgeInit s = do
    (env,_) <- evalReturnEnv [bridgeSpec] config0
    v <- evalE (toStateEnv s ++ env) initBridgeDef
    return $ v == VA_Bool True

config0 :: CFG_Config
config0 = CFG_Config Nothing []

bridgeNext :: Bridge_State -> Bridge_State -> Either EvalError Bool
bridgeNext s s' = do
    (env,_) <- evalReturnEnv [bridgeSpec] config0
    v <- evalE (toStateEnv s ++ toStateEnv' s' ++ env) nextBridgeDef
    return $ v == (VA_Bool True)

-- data Bridge_State
--     = Bridge_State {loc :: Language.TLAPlus.Syntax.VA_Value,
--                     torch :: Language.TLAPlus.Syntax.VA_Value,
--                     time :: Language.TLAPlus.Syntax.VA_Value}
bridgeRecord :: Dec
bridgeRecord =
    DataD [] (mkName "Bridge_State") [] Nothing
    [ RecC (mkName "Bridge_State")
      [ (mkName "loc",lazy,valueT)
      , (mkName "torch",lazy,valueT)
      , (mkName "time",lazy,valueT) ] ]
    []
    where
      valueT = ConT ''VA_Value
      lazy = Bang NoSourceUnpackedness NoSourceStrictness

nextLoc :: Ord k => Map k Location -> Map k Location
nextLoc m = case minViewWithKey y of
              Just ((k,_),m') -> M.union (M.insert k RightLoc x) m'
              Nothing -> m
    where
      x = M.filter (RightLoc ==) m
      y = M.filter (LeftLoc ==) m

next :: Bridge_State -> Bridge_State
next Bridge_State { .. } = Bridge_State
    { loc = nextLoc loc
    , torch = RightLoc
    , time = time + 1 }

main :: IO ()
main = do

    hspec $ do
      it "bridge.tla name" $
          fmap name <$> example `shouldReturn` Right "bridge"
      it "bridge.tla extend" $
          fmap extendDecl <$> example `shouldReturn`
          Right (AS_ExtendDecl (newPos "example/bridge.tla" 3 1)
                 ["Naturals","TLC"])
      it "bridge.tla num defs" $
          fmap (length . mapMaybe (preview _AS_OperatorDef) . unitDef) <$> example
          `shouldReturn` Right 9
      it "bridge.tla num vars" $
          fmap (length . mapMaybe (preview _AS_VariableDecl) . unitDef) <$> example
          `shouldReturn` Right 1
      it "foo" $ do
          let s0 = Bridge_State { loc = fromList [("me",LeftLoc)
                                                 ,("assistant",LeftLoc)
                                                 ,("janitor",LeftLoc)
                                                 ,("professor",LeftLoc)]
                                , torch = LeftLoc
                                , time = 0 }
          print s0
          print $ bridgeInit s0
          print $ next s0
          print $ bridgeNext s0 $ next s0
      -- it "bridge.tla state initial record" $
      --     initBridge
      --     `shouldBe` Right Bridge_State
      --     { loc = toValue $ fromList $ map (,"left") ["me","assistant","janitor","professor"]
      --     , torch = toValue "left"
      --     , time = toValue (0 :: Int) }
