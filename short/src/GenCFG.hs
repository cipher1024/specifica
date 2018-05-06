module GenCFG(crashStartControlConst) where


-- boilerplate :: String -> SH_FL_Spec -> String
-- boilerplate pname spec =
--     unlines $
--       "CONSTANT" :
--       map (\s -> s ++ " <- " ++ xify s) (allSingleMsgHandlerNames spec)

-- genCFGFile :: String -> SH_FL_Spec -> IO ()
-- genCFGFile pname spec =
--     do { let configname = pname ++ ".config"
--        ; c <- readConfig configname
--        ; let b = boilerplate pname spec
--        ; let roles = allRoles spec \\ ["GLOBAL"]
--        ; let c' = complementConfig roles c
--        ; let header = "\\* Generated file, please edit .config file"
--        ; writeFile (pname ++ ".cfg")
--            (unlines [header, prettyPrintCFG c',b])
--        ; return ()
--        }

-- readConfig :: String -> IO CFG_Config
-- readConfig cfgname =
--     do { cfg <- readFile cfgname
--        ; case runParser cfgspec mkState cfgname cfg of
--            Left err -> do{ putStr $ "parse error in " ++ cfgname ++ " at "
--                          ; print err
--                          ; exitFailure
--                          }
--            Right cfg  -> return cfg
--        }

-- complementConfig :: [String] -> CFG_Config -> CFG_Config
-- complementConfig roles config@(CFG_Config name l) =
--     let cs = concatMap (f config) roles
--         cs' = if null cs then [] else [CFG_ConstantDef upos cs]
--      in CFG_Config name $ l ++ cs'
--   where f :: CFG_Config -> String -> [CFG_ConstantEntry]
--         f config role = concatMap (\name -> mk name config role)
--                                      crashStartControlConst
--         mk name config role =
--             if has name role config
--             then []
--             else [CFG_Assignment upos
--                     (CFG_Ident upos $ name ++ role)
--                     (CFG_Set upos empty)]
--         has :: String -> String -> CFG_Config -> Bool
--         has name role config =
--             [] /= everything (++) ([] `mkQ` g name role) config
--           where g name role a@(CFG_Assignment _ (CFG_Ident _ c) _)
--                   | c == name ++ role = [True] -- e.g. "CrashR"
--                   | otherwise = []
--                 g _ _ _ = []

crashStartControlConst :: [String]
crashStartControlConst = ["InitDown", "Crash", "Start"]

---- HELPER -------------------------------------------------------------------
-- mk_AS_Ident :: String -> AS_Expression
-- mk_AS_Ident = AS_Ident epos []

-- mkPos :: String -> Int -> Int -> PPos.SourcePos
-- mkPos = newPos

-- upos :: SourcePos
-- upos = mkPos "foo" 0 0
-- epos :: (SourcePos, Maybe a1, Maybe a2)
-- epos = (upos, Nothing, Nothing)
