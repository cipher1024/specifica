module RewriteStateInit(rewriteStateInit) where

import Data.Generics
import Syntax
import Flatten
import Text.ParserCombinators.Parsec.Pos as PPos
import RewriteCont(typeDefaultValue)

-- this makes things like Map<.,.> very convenient since I don't have to
-- write up the awkward [x \in X |-> default-value(X)]
rewriteStateInit :: SH_FL_Spec -> SH_FL_Spec
rewriteStateInit = everywhere (mkT f)
    where f (SH_State p per v@(ty, _var) Nothing) =
               {- Nothing = not initialized -}
               SH_State p per v (Just (SH_ExprWrapper upos
                                         (typeDefaultValue ty)))
          f x = x

---- HELPER -------------------------------------------------------------------
-- mk_AS_Ident :: String -> AS_Expression
-- mk_AS_Ident = AS_Ident epos []

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos = newPos

upos :: SourcePos
upos = mkPos "foo" 0 0
-- epos :: (SourcePos, Maybe a1, Maybe a2)
-- epos = (upos, Nothing, Nothing)
