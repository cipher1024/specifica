{-# language LambdaCase #-}

module Main where

import Data.Either

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec

import Language.TLAPlus.Pretty (prettyPrintAS)

import Flatten (insideOut, prettyPrintFlatSH)
import Merge (merge)
import Parser (mkState, shortspec)
import Rewrite (rewriteSpecialOperators, rewriteTag)
import RewriteCont (rewriteCont)
import RewriteDoMeanwhile (rewriteDoMeanwhile)
import RewriteExtendHook (rewriteExtendHook)
import RewriteLifecycle (rewriteLifecycle)
import RewriteMsgSetHandler (rewriteMsgSetHandler)
import RewriteONCE (rewriteONCE)
import RewriteOverrideTLA (rewriteOverrideTLA)
import RewriteStateInit (rewriteStateInit)
import RewriteTimer (rewriteTimer)
import RewriteWhen (rewriteWhen)
import Syntax
import SyntaxPretty (prettyPrintSH)
import TLACodeGen (gen, groupSendInstr)

main :: IO ()
main = do
  snames <- getArgs
  l <- mapM readAndParse snames
  let (e,l') = partitionEithers l
  if not (null e)
    then do
      let err = head e
      putStrLn ("ERROR: " ++ show err)
      exitFailure
    else do
      putStrLn "---- PARTS ----"
      mapM_
        (\(Right (SH_Spec name cl)) ->
           putStrLn $ prettyPrintSH $ SH_Spec name cl)
        l
      let l'' = concatMap (\(SH_Spec _name cl) -> cl) l'
      let merged = merge l''
      let (SH_Spec pname _) = head l'
      putStrLn "--------"
      putStrLn ""
      putStrLn ""
      putStrLn "-------- AFTER Merging interactions ---------------"
      putStrLn $ prettyPrintSH $ SH_Spec pname [merged]
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER insideOut (flatten) ----------------"
      let flspec = insideOut merged
      putStrLn $ prettyPrintFlatSH flspec
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteWhen ------------------------"
      let a = rewriteWhen flspec
      putStrLn $ prettyPrintFlatSH a
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteTAG -------------------------"
      let b = rewriteTag a
      putStrLn $ prettyPrintFlatSH b
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteStateInit -------------------"
      let c = rewriteStateInit b
      putStrLn $ prettyPrintFlatSH c
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteDoMeanwhile -----------------"
      let d = rewriteDoMeanwhile c
      putStrLn $ prettyPrintFlatSH d
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteCont ------------------------"
      let e = rewriteCont d
      putStrLn $ prettyPrintFlatSH e
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteONCE ------------------------"
      let f = rewriteONCE e
      putStrLn $ prettyPrintFlatSH f
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteTimer -----------------------"
      let g = rewriteTimer f
      putStrLn $ prettyPrintFlatSH g
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER RewriteMsgSetHandler ---------------"
      let h = rewriteMsgSetHandler g
      putStrLn $ prettyPrintFlatSH h
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER rewriteLifecycle -------------------"
               -- must come after RewriteMsgSetHandler since MsgSetHandler will
               -- potentially generate a negated guard for ANY handlers
               -- prim_ANY_MERGE2 (guard g in any msg m handler).
               -- if done in the wrong order, this leads to code that runs if
               -- ~running is true, which is non-sense
               -- became apparent in the replication_join example
      let i = rewriteLifecycle h
      putStrLn $ prettyPrintFlatSH i
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER groupSendInstr ---------------------"
               -- do the grouping after lifecycle, so multiple crash msgs
               -- (generated in lifecycle) are grouped also
      let j = groupSendInstr i
      putStrLn $ prettyPrintFlatSH j
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER RewriteExtendHook ------------------"
      let k = rewriteExtendHook j
      putStrLn $ prettyPrintFlatSH k
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER Rewrite special operators ----------"
      let l = rewriteSpecialOperators k
      putStrLn $ prettyPrintFlatSH l
      putStrLn "--------"
      putStrLn ""
      putStrLn "-------- AFTER RewriteOverrideTLA ----------"
      let m = rewriteOverrideTLA l
      putStrLn $ prettyPrintFlatSH m
      putStrLn "--------"
      putStrLn ""
      putStrLn ""
      let tla = gen m pname
      putStrLn $ prettyPrintAS tla
               -- use m in SLA gen to make sure that TAG'd msg fields can
               -- be used in the display annotations also.
               --; genSLAFile (map toLower pname) m tla
               --; genCFGFile (map toLower pname) m

readAndParse :: String -> IO (Either ParseError SH_Spec)
readAndParse fname = runParser shortspec mkState fname <$> readFile fname
