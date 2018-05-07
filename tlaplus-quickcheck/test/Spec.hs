
import Control.Lens
import Data.Maybe
import Language.TLAPlus.Parser
import Language.TLAPlus.Syntax
import Test.Hspec hiding (example)
import Text.Parsec.Error
import Text.Parsec.Pos

example :: IO (Either ParseError AS_Spec)
example = parseFile tlaspec "example/bridge.tla"

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

