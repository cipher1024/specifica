module Flatten where

import Data.Generics
import Syntax
import SyntaxPretty (ppInteractionElement, ppMsgDecl)

import Text.PrettyPrint.Leijen as PPrint

import Prelude hiding ((<$>))

type Role = SH_InteractionElement
type VerbTLA = SH_InteractionElement
type Const = String

data SH_FL_Spec = SH_FL_Spec { msgDecl :: [SH_MsgDecl],
                               msgExt :: [SH_InteractionElement],
                               roleDecl :: [Role],
                               verbTLA :: [VerbTLA],
                               displayAnn :: [SH_InteractionElement],
                               constant :: [Const] }
                  deriving (Eq, Ord, Show, Data, Typeable)

insideOut :: SH_Concern -> SH_FL_Spec
insideOut (SH_Concern _ _ l) =
    let a = concatMap cSH_MsgDecl l
        b = concatMap cSH_MsgExt l
        c = concatMap cSH_RoleDef l -- FIXME merge roles of same name!
        d = concatMap cSH_VerbTLA l
        e = concatMap cSH_DisplayAnn l
        f = concatMap cSH_Const l
     in SH_FL_Spec { msgDecl = a,
                     msgExt = b,
                     roleDecl = c,
                     verbTLA = d,
                     displayAnn = e,
                     constant = f }

cSH_MsgDecl :: SH_ConcernElement -> [SH_MsgDecl]
cSH_MsgDecl (SH_Interaction _ _ _ _ l) =
    concatMap cSH_MsgDecl2 l
    where cSH_MsgDecl2 (SH_IntraInteractionMsg _ m) = [m]
          cSH_MsgDecl2 _ = []
cSH_MsgDecl _ = []

cSH_MsgExt :: SH_ConcernElement -> [SH_InteractionElement]
cSH_MsgExt (SH_Interaction _ _ _ _ l) =
    concatMap cSH_MsgExt2 l
    where cSH_MsgExt2 e@(SH_Extend_Msg _ _f _t _mt _l) = [e]
          cSH_MsgExt2 _ = []
cSH_MsgExt _ = []

cSH_RoleDef :: SH_ConcernElement -> [Role]
cSH_RoleDef (SH_Interaction _ _ _ _ l) =
    concatMap cSH_RoleDef2 l
    where cSH_RoleDef2 r@SH_RoleDef{} = [r]
          cSH_RoleDef2 _ = []
cSH_RoleDef _ = []

cSH_VerbTLA :: SH_ConcernElement -> [VerbTLA]
cSH_VerbTLA (SH_Interaction _ _ _ _ l) =
    concatMap cSH_VerbTLA2 l
    where cSH_VerbTLA2 r@SH_VerbTLAOp{} = [r]
          cSH_VerbTLA2 _ = []
cSH_VerbTLA _ = []

cSH_DisplayAnn :: SH_ConcernElement -> [SH_InteractionElement]
cSH_DisplayAnn (SH_Interaction _ _ _ _ l) =
    concatMap cSH_DisplayAnn2 l
    where cSH_DisplayAnn2 r@(SH_DisplaySwimlane _ _) = [r]
          cSH_DisplayAnn2 _ = []
cSH_DisplayAnn _ = []

cSH_Const :: SH_ConcernElement -> [String]
cSH_Const (SH_Constant _ l) = l
cSH_Const _ = []

prettyPrintFlatSH :: SH_FL_Spec -> String
prettyPrintFlatSH spec =
    showWidth 79 $ ppFlatSH spec
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppFlatSH :: SH_FL_Spec -> Doc
ppFlatSH spec =
    (if constant spec /= []
     then text "CONST "
     else PPrint.empty) <+> hcat (punctuate comma (map text (constant spec)))
    <$> vcat (map ppInteractionElement (verbTLA spec))
    <$> vcat (map ppMsgDecl (msgDecl spec))
    <$> vcat (map ppInteractionElement (msgExt spec))
    <$> vcat (map ppInteractionElement (displayAnn spec))
    <$> vcat (map ppInteractionElement (roleDecl spec))
