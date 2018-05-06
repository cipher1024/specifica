module RewriteONCE(rewriteONCE) where

import Data.Generics
import Syntax
import Flatten
import RewriteLifecycle(appendIL)
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax

rewriteONCE :: SH_FL_Spec -> SH_FL_Spec
rewriteONCE = everywhere (mkT f)
    where f (SH_RoleDef _ rname vars elems) =
              let state = map (\(SH_Once _ _rname _ label _ _) ->
                                  SH_State upos False
                                    (SH_Ty_UserDef upos "BOOLEAN",
                                     name_once label)
                                    (Just $ SH_ExprWrapper upos $
                                       AS_Bool epos False))
                                (allONCE elems)
                  new_elems = map (replONCE rname) elems
              in SH_RoleDef upos rname vars $ new_elems ++ state
          f x = x
          allONCE :: [SH_RoleElement] -> [SH_RoleElement]
          allONCE = filter (\e -> case e of
                                       (SH_Once _ _ _ _ _ _) -> True
                                       _ -> False)
          -- No guard case
          replONCE _rname (SH_Once _ role when label hooks ginstr) =
              let w = AS_LAND epos $
                        [AS_PrefixOP epos AS_Not
                            (mk_AS_Ident (name_once label))] ++
                         (case when of
                            Just (SH_ExprWrapper _ w) -> [w]
                            Just _  -> undefined
                            Nothing -> [])
               in SH_CallHandler upos role (Just $ SH_ExprWrapper upos w)
                        (name_once label) [] hooks
                        (appendIL [SH_I_ChangeState upos
                                    [SH_ExprWrapper upos $ AS_InfixOP epos
                                    AS_EQ
                                    (mk_AS_Ident $ name_once label)
                                    (AS_Bool epos True)]] ginstr)
          replONCE _ x = x

name_once :: String -> String
name_once s = "g_once_" ++ s

---- HELPER -------------------------------------------------------------------
mk_AS_Ident :: String -> AS_Expression
mk_AS_Ident = AS_Ident epos []

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos = newPos

upos :: SourcePos
upos = mkPos "foo" 0 0
epos :: (SourcePos, Maybe a1, Maybe a2)
epos = (upos, Nothing, Nothing)
