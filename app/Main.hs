module Main where

import           Idris.Main
import           Idris.Core.TT
import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.Options

import           IRTS.Compiler
import           IRTS.CodegenMalfunction
import           IRTS.CodegenCommon

import           System.Environment
import           System.Exit



data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   interface :: Bool,
                   camlPkgs :: [String] }

showUsage :: IO a
showUsage = do
  putStrLn "A code generator which is intended to be called by the compiler"
  putStrLn
    "Usage: idris-malfunction \
  \ <ibc-files> [-o <output-file>] [-op/--ocamlpackage <ocaml-packages]"
  exitSuccess

getOpts :: IO Opts
getOpts = process (Opts [] "a.out" False []) <$> getArgs
 where
  process opts ("-o" : o      : xs) = process (opts { output = o }) xs
  process opts ("--interface" : xs) = process (opts { interface = True }) xs
  process opts (x             : xs) = case words x of
    [z] -> process (opts { inputs = z : inputs opts }) xs
    zs ->
      let f ys ("-op"            : p : ps) = f (p : ys) ps
          f ys ("--ocamlpackage" : p : ps) = f (p : ys) ps
          f ys (p                    : ps) = f ys ps
          f ys []                          = ys
      in  process (opts { camlPkgs = f [] zs }) xs
  process opts [] = opts

malfunctionMain :: Opts -> Idris ()
malfunctionMain opts = do
  elabPrims
  loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "malfunction") (output opts) (Just mainProg)
  runIO
    $ codegenMalfunction (camlPkgs opts) (ir { interfaces = interface opts })

main :: IO ()
main = do
  opts <- getOpts
  as   <- getArgs
  if null (inputs opts) then showUsage else runMain (malfunctionMain opts)
