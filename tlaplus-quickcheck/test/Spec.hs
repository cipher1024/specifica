{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Maybe
import Language.Haskell.TH
import Language.TLAPlus.Parser
import Language.TLAPlus.Syntax
import Test.QuickCheck.TLAPlus.TH
import Test.Hspec hiding (example)
import Text.Parsec.Error
import Text.Parsec.Pos

tlaImportSpec "example/bridge.tla"

example :: IO (Either ParseError AS_Spec)
example = parseFile tlaspec "example/bridge.tla"

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

main :: IO ()
main = hspec $ do
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
      it "bridge.tla state record" $
          runQ (tlaImportSpec "example/bridge.tla")
          `shouldReturn` [bridgeRecord]
