{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.TLAPlus.Syntax where

import Control.Lens
import Control.Monad

import Data.Bitraversable
import Data.Map as Map hiding (map)
import Data.Set as Set hiding (map)
import Data.Generics

import Language.Haskell.TH.Syntax

import Text.ParserCombinators.Parsec.Pos as PPos

type AS_InfoE = (PPos.SourcePos, Maybe AS_UnitDef, Maybe AS_Expression)
type AS_InfoU = PPos.SourcePos

data AS_Spec = AS_Spec {name :: String,
                        extendDecl   :: AS_ExtendDecl,
                        unitDef      :: [AS_UnitDef]}
               deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_ExtendDecl = AS_ExtendDecl PPos.SourcePos [String]
                     deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_QBoundN = AS_QBoundN [AS_Name] AS_Expression
                  deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_QBound1 = AS_QBound1 AS_Name AS_Expression
                  deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_UnitDef =
    AS_FunctionDef AS_InfoU AS_Name [AS_QBoundN] AS_Expression
  | AS_OperatorDef AS_InfoU AS_OperatorHead AS_Expression
  | AS_Assume AS_InfoU AS_Expression
  | AS_Theorem AS_InfoU AS_Expression
  | AS_ConstantDecl AS_InfoU [AS_Name]
  | AS_VariableDecl AS_InfoU [AS_Name]
  | AS_Separator AS_InfoU
    deriving (Eq, Ord, Show, Data, Typeable, Lift)

instance Lift PPos.SourcePos where

data AS_OperatorHead = AS_OpHead AS_Name [AS_Expression]

                       deriving (Eq, Ord, Show, Data, Typeable, Lift)

class HasIdent a where
    mk_Ident :: AS_InfoE -> [String] -> String -> a

mk_Ident' :: HasIdent a => String -> a
mk_Ident' = mk_Ident (PPos.newPos "foo" 1 1,Nothing,Nothing) []

mk_IdentAt :: HasIdent a => SourcePos -> [String] -> String -> a
mk_IdentAt p = mk_Ident (p,Nothing,Nothing)

instance HasIdent AS_Name where
    mk_Ident = AS_Name

instance HasIdent AS_Expression where
    mk_Ident x ps n = AS_Ident $ AS_Name x ps n

data AS_Name = AS_Name AS_InfoE [String] String -- possibly prefixed X!Y!a
    deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_Expression =
        AS_Ident AS_Name
      | AS_FunArgList AS_InfoE [AS_Expression]
      | AS_OpApp AS_InfoE AS_Name [AS_Expression]
      | AS_FunctionType AS_InfoE AS_Expression AS_Expression
      | AS_PrefixOP AS_InfoE AS_PrefixOp AS_Expression
      | AS_PostfixOP AS_InfoE AS_PostfixOp AS_Expression
      | AS_InfixOP AS_InfoE AS_InfixOp AS_Expression AS_Expression
      | AS_Let AS_InfoE [AS_UnitDef] AS_Expression
      | AS_IF AS_InfoE AS_Expression AS_Expression AS_Expression
      | AS_DiscreteSet AS_InfoE [AS_Expression]
      | AS_RecordFunction AS_InfoE [AS_MapTo]
      | AS_QuantifierBoundFunction AS_InfoE [AS_QBoundN] AS_Expression
      | AS_Choose AS_InfoE AS_QBound1 AS_Expression
      | AS_Quantified AS_InfoE AS_QuantifierKind [AS_QBoundN] AS_Expression
      | AS_Tuple AS_InfoE [AS_Expression]
      | AS_LAND AS_InfoE [AS_Expression]
      | AS_LOR AS_InfoE [AS_Expression]
      | AS_Num AS_InfoE Int
      | AS_Bool AS_InfoE Bool
      | AS_StringLiteral AS_InfoE String
      | AS_RecordType AS_InfoE [AS_RecordElementType]
      | AS_SetComprehension AS_InfoE AS_QBound1 AS_Expression
      | AS_SetGeneration AS_InfoE AS_Expression AS_QBound1
      | AS_Except AS_Expression [AS_ExceptAssignment]
      | AS_OldVal
      | AS_Case AS_InfoE [AS_CaseArm] (Maybe AS_CaseArm)
      | AS_Stutter AS_Expression AS_Expression
        -- For codegen only (no parser support yet).
      | AS_Fair Bool AS_Expression AS_Expression
        -- A BIF is bound into the environment by making it the VA_OperatorDef
        -- expression. BIFs are not created by the parser, but instead they are
        -- bound in the env structure before evaluation starts.
        -- BIFs implictly know what argument names they look for.
      | AS_BIF String String
        -- in Parser.op_infixS we replace AS_CloseFunApp with the correct
        -- expression tree. AS_CloseFunApp thus never appears in a correct AST
      | AS_CloseFunApp -- the ] in a f[a,b] construct
        deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_Field = AS_Field String deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_ExceptNav = AS_ExceptNavField AS_Field                            -- .x
                  | AS_ExceptNavApp [AS_Expression]                    -- [x,y]
                    deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_ExceptAssignment = AS_ExceptAssignment [AS_ExceptNav] AS_Expression
                           deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_CaseArm = AS_CaseArm AS_InfoE AS_Expression AS_Expression
                | AS_OtherCaseArm AS_InfoE AS_Expression
                  deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_RecordElementType = AS_RecordElementType AS_InfoE
                              AS_Field AS_Expression
                            deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_QuantifierKind = AS_All | AS_Exist
                         deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_MapTo = AS_MapTo AS_Field AS_Expression
                deriving (Eq, Ord, Show, Data, Typeable, Lift)

data AS_PrefixOp = AS_SUBSET
                 | AS_INSTANCE
                 | AS_UNION
                 | AS_DOMAIN
                 | AS_UNCHANGED
                 | AS_Not
                 | AS_ALWAYS
                 | AS_Eventually
                   deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_PostfixOp = AS_Prime
                    deriving (Eq, Ord, Show, Data, Typeable, Lift)
data AS_InfixOp = AS_EQ
                | AS_NEQ
                | AS_COLONGT
                | AS_ATAT
                | AS_DOTDOT
                | AS_DOT
                | AS_GT
                | AS_LT
                | AS_LTEQ
                | AS_GTEQ
                | AS_SubsetEq
                | AS_Cup
                | AS_Cap
                | AS_SetMinus
                | AS_In
                | AS_Circ
                | AS_NotIn
                | AS_Plus
                | AS_DIV
                | AS_MOD
                | AS_Mult
                | AS_Minus
                | AS_Times
                | AS_AND
                | AS_OR
                | AS_Implication
                | AS_TildeGT
                | AS_FunApp -- f.g[a,b] => f.g `funapp` arrayref [a,b]
                  deriving (Eq, Ord, Show, Data, Typeable, Lift)
-------------------------------------------------------------------------------

parentE :: AS_Expression -> Maybe AS_Expression
parentE e = let (_pos, _u, mpe) = infoE e in mpe

-- don't call this unless the AST was rewritten to establish the
-- backwards links
parentU :: AS_Expression -> AS_UnitDef
parentU e = let (_pos, Just u, _mpe) = infoE e in u

ppLocE :: AS_Expression -> String
ppLocE e = let (pos, _u, _mpe) = infoE e in formatLoc pos

ppLocU :: AS_UnitDef -> String
ppLocU u = formatLoc $ infoU u

formatLoc :: SourcePos -> String
formatLoc info =
    let path = sourceName info
        line = sourceLine info
        col  = sourceColumn info
     in path ++ ":" ++ show line ++ ":" ++ show col

infoE :: AS_Expression -> AS_InfoE
infoE (AS_Ident (AS_Name info _ _)) = info
infoE (AS_FunArgList info _) = info
infoE (AS_OpApp info _ _) = info
infoE (AS_FunctionType info _ _) = info
infoE (AS_PrefixOP info _ _) = info
infoE (AS_PostfixOP info _ _) = info
infoE (AS_InfixOP info  _ _ _) = info
infoE (AS_Let info _ _) = info
infoE (AS_IF info _ _ _) = info
infoE (AS_DiscreteSet info _) = info
infoE (AS_RecordFunction info _) = info
infoE (AS_QuantifierBoundFunction info _ _) = info
infoE (AS_Choose info _ _) = info
infoE (AS_Quantified info _ _ _) = info
infoE (AS_Tuple info _) = info
infoE (AS_LAND info _) = info
infoE (AS_LOR info _) = info
infoE (AS_Num info _) = info
infoE (AS_Bool info _) = info
infoE (AS_StringLiteral info _) = info
infoE (AS_RecordType info _) = info
infoE (AS_SetComprehension info _ _) = info
infoE (AS_SetGeneration info _ _) = info
{-
      | AS_Except AS_Expression [AS_ExceptAssignment]
      | AS_OldVal
-}
infoE (AS_Case info _ _) = info
{-
      | AS_Stutter AS_Expression AS_Expression
      | AS_BIF AS_InfoE String String
      | AS_CloseFunApp
-}
{- for debugging - this ensures that we can print NoRule errors in Eval -}
infoE _ = mkDummyInfo "ERROR-Syntax.infoE-UPDATE-NEEDED"

mkDummyInfo :: SourceName -> (SourcePos, Maybe a1, Maybe a2)
mkDummyInfo s  = (PPos.newPos s 0 0, Nothing, Nothing)

infoU :: AS_UnitDef -> AS_InfoU
infoU (AS_FunctionDef info _ _ _) = info
infoU (AS_OperatorDef info _ _) = info
infoU (AS_Assume info _) = info
infoU (AS_Theorem info _) = info
infoU (AS_ConstantDecl info _) = info
infoU (AS_VariableDecl info _) = info
infoU (AS_Separator info) = info

-------------------------------------------------------------------------------

data CFG_Config = CFG_Config (Maybe String) [CFG_Statement]
                  deriving (Eq, Ord, Show, Data, Typeable)

data CFG_Statement = CFG_ConstantDef CFG_Info [CFG_ConstantEntry]
                   | CFG_Invariant CFG_Info [CFG_Ident]
                   | CFG_Property CFG_Info [CFG_Ident]
                   | CFG_Symmetry CFG_Info CFG_Ident
                   | CFG_View CFG_Info CFG_Ident
                     deriving (Eq, Ord, Show, Data, Typeable)

data CFG_Value = CFG_Atom CFG_Info String    -- translates to VA_Atom
               | CFG_Bool CFG_Info Bool                    -- VA_Bool
               | CFG_Int CFG_Info Int                      -- VA_Int
               | CFG_StringLiteral CFG_Info String         -- VA_String
               | CFG_Set CFG_Info (Set CFG_Value)          -- VA_Set
                 deriving (Eq, Ord, Show, Data, Typeable)

data CFG_Ident = CFG_Ident CFG_Info String
                 deriving (Eq, Ord, Show, Data, Typeable)

data CFG_ConstantEntry = CFG_Assignment CFG_Info CFG_Ident CFG_Value
                       | CFG_Subst CFG_Info CFG_Ident CFG_Ident
                         deriving (Eq, Ord, Show, Data, Typeable)

type CFG_Info = PPos.SourcePos

cfg_constants :: CFG_Config -> [(CFG_Ident, CFG_Value)]
cfg_constants (CFG_Config _name stmts) =
    concatMap (\s -> case s of
                 (CFG_ConstantDef _info l) ->
                     concatMap (\centry -> case centry of
                                   (CFG_Assignment _info ident value) -> [(ident, value)]
                                   _ -> []) l
                 _ -> []
          ) stmts

mkEmptyConfig :: CFG_Config
mkEmptyConfig = CFG_Config Nothing []
-------------------------------------------------------------------------------

data VA_Value = VA_Map (Map VA_Value VA_Value)       -- map
              | VA_Rec (Map VA_Value VA_Value)       -- rec (key = VA_String)
              | VA_Set (Set VA_Value)                -- set
              | VA_Seq [VA_Value]                    -- seq
              | VA_Int Int                           -- int
              | VA_Bool Bool                         -- boolean
              | VA_String String                     -- string
              | VA_Char Char                         -- char, from string[2]
              | VA_Atom String                       -- atom, from cfg file
              | VA_FunctionDef AS_InfoE              -- fun[x] ==
                  AS_Name [AS_QBoundN]
                  AS_Expression
              | VA_OperatorDef AS_InfoE              -- op(x) ==
                  AS_OperatorHead AS_Expression
              | VA_FunType VA_Value VA_Value
              | VA_RecType (Map VA_Value VA_Value)   -- key = VA_String
              | VA_SeqType VA_Value
              | VA_Var (Maybe VA_Value)
              | VA_FunArgList [VA_Value]             -- eval internal only
                deriving (Eq, Ord, Show, Data, Typeable)

makePrisms ''VA_Value

class Ord a => IsTLAValue a where
    toValue :: a -> VA_Value
    fromValue :: VA_Value -> Maybe a

instance IsTLAValue Int where
    toValue = VA_Int
    fromValue = preview _VA_Int

instance IsTLAValue String where
    toValue = VA_String
    fromValue = preview _VA_String

asList :: Ord k' => Lens (Map k a) (Map k' a') [(k,a)] [(k',a')]
asList = lens Map.toList (const Map.fromList)

instance (IsTLAValue k,IsTLAValue a) => IsTLAValue (Map k a) where
    toValue = VA_Map . (asList %~ map (bimap toValue toValue))
    fromValue = asList (traverse $ bitraverse fromValue fromValue)
                <=< preview _VA_Map

data TY_Type = TY_Map | TY_Rec | TY_Set | TY_Seq |
               TY_Int | TY_Bool | TY_String | TY_Char | TY_Atom |
               TY_Fun | TY_Op |
               TY_RecType | TY_FunType | TY_SeqType |
               TY_Var |
               TY_FunArgList
               deriving (Eq, Ord, Show, Data, Typeable)

typeOf :: VA_Value -> TY_Type
typeOf (VA_Map _) = TY_Map
typeOf (VA_Rec _) = TY_Rec
typeOf (VA_Set _) = TY_Set
typeOf (VA_Seq _) = TY_Seq
typeOf (VA_Int _) = TY_Int
typeOf (VA_Bool _) = TY_Bool
typeOf (VA_String _) = TY_String
typeOf (VA_Char _) = TY_Char
typeOf (VA_Atom _) = TY_Atom
typeOf (VA_FunctionDef _ _ _ _) = TY_Fun
typeOf (VA_OperatorDef _ _ _) = TY_Op
typeOf (VA_FunType _ _) = TY_FunType
typeOf (VA_RecType _) = TY_RecType
typeOf (VA_SeqType _) = TY_SeqType
typeOf (VA_Var _) = TY_Var
typeOf (VA_FunArgList _) = TY_FunArgList

ppTY :: TY_Type -> String
ppTY TY_Map = "Map"
ppTY TY_Rec = "Rec"
ppTY TY_Set = "Set"
ppTY TY_Seq = "Seq"
ppTY TY_Int = "Int"
ppTY TY_Bool = "Bool"
ppTY TY_String = "String"
ppTY TY_Char = "Char"
ppTY TY_Atom = "Atom"
ppTY TY_Fun = "Fun"
ppTY TY_Op = "Op"
ppTY TY_FunType = "FunType"
ppTY TY_RecType = "RecType"
ppTY TY_SeqType = "SeqType"
ppTY TY_Var = "Variable"
ppTY TY_FunArgList = "FunArgListType"

extendedMod :: AS_ExtendDecl -> [String]
extendedMod (AS_ExtendDecl _ ms) = ms

makePrisms ''AS_UnitDef
makePrisms ''AS_Expression
