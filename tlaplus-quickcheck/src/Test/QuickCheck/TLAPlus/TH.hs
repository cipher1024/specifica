{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.QuickCheck.TLAPlus.TH where

import Control.Lens
import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.TLAPlus.Eval
import Language.TLAPlus.Parser
import Language.TLAPlus.Syntax

toName :: AS_Name -> Name
toName (AS_Name _ _ n) = mkName n

mkVar :: AS_Name -> Q VarBangType
mkVar n = do
    t <- [t|VA_Value|]
    return (toName n,Bang NoSourceUnpackedness NoSourceStrictness,t)

tlaImportSpec :: FilePath -> DecsQ
tlaImportSpec fp = do
    spec <- runIO (parseFile tlaspec fp)
        >>= either (fail . show) return
    forM_ (extendedMod (extendDecl spec) \\ ["TLC","Naturals"]) $
        fail "'Extend' not supported"
    let spec' = spec { name = name spec & _head %~ toUpper }
    concat <$> sequence
        [ pure <$> specStateRec spec'
        , initSpec spec' ]

specStateRec :: AS_Spec -> DecQ
specStateRec spec = do
    t <- sequence [ [t|Show|], [t|Eq|] ]
    let n = stateRec spec
        der = DerivClause Nothing <$> [ t ]
    con <- RecC n <$> mapM mkVar (variables spec)
    return $ DataD [] n [] Nothing [con] der

initSpec :: AS_Spec -> DecsQ
initSpec spec = do
    (_,initDef) <- lookupDef spec "Init"
    let vars = toName <$> variables spec
    env <- newName "env"
    fields <- initValues vars (VarE env) initDef
    let n = mkName $ "init" ++ name spec
        asgn = [ (n,) <$> varE n | (n,_) <- fields ]
        def  = doE $  bindS (tupP [varP env,wildP])
                         [| evalReturnEnv [ $(lift spec) ] (CFG_Config Nothing []) |]
                    : [ bindS (varP n) (pure e) | (n,e) <- fields ]
                   ++ [ noBindS [| return $(recConE (stateRec spec) asgn) |] ]
    sequence
        [ sigD n [t| ThrowsError $(conT $ stateRec spec) |]
        , valD (varP n) (normalB def) [] ]

initValues :: [Name] -> Exp -> AS_Expression -> Q [(Name,Exp)]
initValues vs e (AS_LAND _ xs) = concat <$> mapM (initValues vs e) xs
initValues vs e (AS_InfixOP _ AS_AND x y) = (++) <$> initValues vs e x <*> initValues vs e y
initValues vs e (AS_InfixOP _ AS_EQ (AS_Ident (AS_Name _ [] x)) y)
    | mkName x `elem` vs = pure . (mkName x, ) <$> [| evalE $(pure e) $(lift y) |]
initValues _ _ _ = pure []

stateRec :: AS_Spec -> Name
stateRec spec = mkName (name spec ++ "_State")

variables :: AS_Spec -> [AS_Name]
variables = concat . mapMaybe (preview (_AS_VariableDecl._2)) . unitDef

lookupDef :: AS_Spec -> String -> Q (AS_OperatorHead,AS_Expression)
lookupDef spec nm = do
    let defs = mapMaybe (preview $ _AS_OperatorDef . to ((,) <$> view _2 <*> view _3)) $ unitDef spec
        rs = filter (\(AS_OpHead (AS_Name _ ps n) _, _) -> null ps && n == nm) defs
    case rs of
      [def] -> return def
      [] -> fail $ "No definitions with name '" ++ nm ++ "'"
      _  -> fail $ "Multiple definitions with name '" ++ nm ++ "'"
