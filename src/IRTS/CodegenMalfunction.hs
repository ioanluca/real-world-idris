module IRTS.CodegenMalfunction
  ( codegenMalfunction
  )
where

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.OCamlFFI
import           IRTS.Lang
import           Malfunction.AST
import           Malfunction.Codegen

import           Control.Exception

import           System.Process
import           System.Directory
import           System.FilePath

import qualified Data.Text.IO                  as T
import           Data.List                      ( intersperse
                                                , nub
                                                )
import           Data.Char                      ( toLower )

-- TODO:
-- unicode, cannot just show KStrs, ocaml 8bit, overflow safety?
-- implement all primitives
-- use ocaml gc optimizations through env vars
-- tail calls
-- squeeze multiple lets together? let x = let y = 3 in 
-- squeeze multiple lambdas together?
-- --interface flag?
-- dead code elimination (extra care with inlined definitions)
-- maybe get lang decls without liftings (if possible)
-- use state with a record for code generation monad?
-- use writerT and ReaderT instead of the Translate monad?
-- add Logging to the monad
codegenMalfunction :: [String] -> CodeGenerator
codegenMalfunction ps ci = do
  writeFile langFile $ stringify langDeclarations

  putStrLn "codegeninfo: "
  print (interfaces ci)
  print (compileObjs ci)
  print (outputType ci)
  print (compileLibs ci)
  print (compilerFlags ci)
  print (includes ci)
  print (importDirs ci)
  print (targetTriple ci)
  print (targetCPU ci)

  let prog = generateMlfProgram langDeclarations (names2Export exps)
  T.writeFile tmp $ mlfAST2Text prog

  callCommand fmtCommand
  let cmd = if null exps then compileCommand else cmxCommand
  putStrLn cmd
  catch (callCommand cmd) handler
  removeFile tmp
 where
  langDeclarations = liftDecls ci
  exps             = exportDecls ci
  outFile          = outputFile ci
  mlfFile          = if null exps
    then replaceExtensionIf outFile ".out" ".mlf"
    else let (Export _ f _) = head exps in replaceExtensionIf f ".mli" ".mlf"
  langFile  = replaceExtensionIf outFile ".out" ".lang"
  tmp       = "idris_malfunction_output.mlf"

  ocamllibs = nub $ "idrisobj" : (compileLibs ci) ++ ps
  camlPks   = if null ocamllibs
    then const ""
    else concat . (:) " -p " . intersperse " -p "

  fmtCommand = "malfunction fmt " ++ tmp ++ " > " ++ mlfFile
  compileCommand =
    "malfunction compile -o " ++ outFile ++ camlPks ocamllibs ++ " " ++ mlfFile
  cmxCommand = "malfunction cmx " ++ camlPks ocamllibs ++ " " ++ mlfFile

handler :: SomeException -> IO ()
handler ex = putStrLn $ "Caught exception: " ++ show ex

stringify :: [(Name, LDecl)] -> String
stringify = unwords . map ((\decl -> show decl ++ "\n\n\n") . snd)

replaceExtensionIf :: FilePath -> String -> String -> FilePath
replaceExtensionIf file curr new = case stripExtension curr file of
  Just fileNoExt -> fileNoExt <.> new
  _              -> file <.> new

names2Export :: [ExportIFace] -> [Name]
names2Export [] = []
names2Export (Export _ mliFile es : eifs) = -- fixme guard against the ocaml ffi name only
  fNames2Export es ++ names2Export eifs

fNames2Export :: [Export] -> [Name]
fNames2Export []                       = []
fNames2Export (ExportFun n _ _ _ : es) = n : fNames2Export es
fNames2Export (ExportData _      : es) = fNames2Export es
