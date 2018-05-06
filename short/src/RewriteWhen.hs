module RewriteWhen(rewriteWhen) where

import Data.Generics
import Syntax
import Flatten
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax
import RewriteCont(beautifyLAND)

rewriteWhen :: SH_FL_Spec -> SH_FL_Spec
rewriteWhen = beautifyLAND
            . everywhere (mkT f)
  where f (SH_RoleDef info role vars l) =
            let l' = concatMap rewriteWhen0 l
             in SH_RoleDef info role vars l'
        f x = x

rewriteWhen0 :: SH_RoleElement -> [SH_RoleElement]
rewriteWhen0 (SH_WhenBlock _ guard l) =
    concatMap (rewriteWhen1 (Just guard)) l
rewriteWhen0 x = [x]

rewriteWhen1 :: Maybe SH_ExprWrapper -> SH_RoleElement -> [SH_RoleElement]
rewriteWhen1 g (SH_WhenBlock _ guard l) =
    concatMap (rewriteWhen1 (combineGuards g (Just guard))) l
rewriteWhen1 g (SH_MsgHandler info ann role when mtype lab any from gil) =
    [SH_MsgHandler info ann role (cg g when) mtype lab any from gil]
rewriteWhen1 g (SH_CallHandler info role when lab args hook gil) =
    [SH_CallHandler info role (cg g when) lab args hook gil]
rewriteWhen1 g (SH_TimeoutHandler info role when lab hook gil) =
    [SH_TimeoutHandler info role (cg g when) lab hook gil]
rewriteWhen1 g (SH_CrashHandler info ann role when remote id hook gil) =
    [SH_CrashHandler info ann role (cg g when) remote id hook gil]
rewriteWhen1 g (SH_Every info role when period hook gil) =
    [SH_Every info role (cg g when) period hook gil]
rewriteWhen1 g (SH_Once info role when lab hook gil) =
    [SH_Once info role (cg g when) lab hook gil]
rewriteWhen1 _ x = [x]

cg :: Maybe SH_ExprWrapper -> Maybe SH_ExprWrapper -> Maybe SH_ExprWrapper
cg = combineGuards

combineGuards :: Maybe SH_ExprWrapper -> Maybe SH_ExprWrapper
              -> Maybe SH_ExprWrapper
combineGuards Nothing Nothing = Nothing
combineGuards pred Nothing    = pred
combineGuards Nothing pred    = pred
combineGuards  (Just (SH_ExprWrapper _ a)) (Just (SH_ExprWrapper _ b)) =
    Just $ SH_ExprWrapper upos (AS_LAND epos [a, b])
combineGuards (Just _) (Just _) = undefined

---- HELPER -------------------------------------------------------------------
-- mk_AS_Ident :: String -> AS_Expression
-- mk_AS_Ident = AS_Ident epos []

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos = newPos

upos :: SourcePos
upos = mkPos "foo" 0 0
epos :: (SourcePos, Maybe a1, Maybe a2)
epos = (upos, Nothing, Nothing)
