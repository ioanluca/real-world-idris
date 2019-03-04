module IRTS.NewCodegenMalfunction
  ( codegenMalfunction
  )
where

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.Lang
import           Malfunction.AST
import           Malfunction.Codegen

import           Control.Exception

import           System.Process
import           System.Directory
import           System.FilePath


-- floats
-- unicode, cannot just show KStrs, ocaml 8bit, overflow safety?
-- ffi with ocaml
-- implement all primitives
-- use ocaml gc optimizations through env vars
-- replace all dummy params with KInt 0 for speed?
-- tail calls
-- functions with no args shouldn't be lambdas or applied
-- squeese multiple lets together? let x = let y = 3 in 
-- don't generate code for unused functions? ie remove dead code
codegenMalfunction :: CodeGenerator
codegenMalfunction ci = do
  writeFile langFile $ stringify langDeclarations

  let prog = generateMlfProgram langDeclarations
  -- get prog, textify, and write file
  -- writeFile tmp $ show compileExp
  callCommand fmtCommand
  catch (callCommand compileCommand) handler
  removeFile tmp
 where
  langDeclarations = liftDecls ci

  outFile          = outputFile ci
  mlfFile          = replaceExtensionIf outFile ".o" ".x.mlf"
  langFile         = replaceExtensionIf outFile ".o" ".lang"
  tmp              = "idris_malfunction_output.mlf"

  fmtCommand       = "malfunction fmt " ++ tmp ++ " > " ++ mlfFile
  evalCommand      = "cat " ++ mlfFile ++ " | malfunction eval"
  compileCommand   = "malfunction compile -o " ++ outFile ++ " " ++ mlfFile

-- runMain = mlfApp (cgName (sMN 0 "runMain")) [KStr "RUNMAIN_EATME"]


handler :: SomeException -> IO ()
handler ex = putStrLn $ "Caught exception: " ++ show ex

stringify :: [(Name, LDecl)] -> String
stringify = unwords . map (\decl -> show decl ++ "\n\n")

replaceExtensionIf :: FilePath -> String -> String -> FilePath
replaceExtensionIf file curr new = case stripExtension curr file of
  Just fileNoExt -> fileNoExt <.> new
  _              -> file <.> new
