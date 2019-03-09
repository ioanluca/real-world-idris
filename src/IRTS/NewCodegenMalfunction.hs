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

import qualified Data.Text.IO                  as T


-- unicode, cannot just show KStrs, ocaml 8bit, overflow safety?
-- implement all primitives
-- use ocaml gc optimizations through env vars
-- tail calls
-- squeese multiple lets together? let x = let y = 3 in 
-- --interface flag?
-- don't generate code for unused functions? ie remove dead code
-- maybe get lang decls without liftings
-- use writerT and ReaderT to make Translate
-- can remove inlined definitios I think
codegenMalfunction :: CodeGenerator
codegenMalfunction ci = do
  writeFile langFile $ stringify langDeclarations

  let prog = generateMlfProgram langDeclarations
  T.writeFile tmp $ mlfAST2Text prog

  callCommand fmtCommand
  catch (callCommand compileCommand) handler
  removeFile tmp
 where
  langDeclarations = liftDecls ci

  outFile          = outputFile ci
  mlfFile          = replaceExtensionIf outFile ".o" ".new.mlf"
  langFile         = replaceExtensionIf outFile ".o" ".lang"
  tmp              = "idris_malfunction_output.mlf"

  fmtCommand       = "malfunction fmt " ++ tmp ++ " > " ++ mlfFile
  evalCommand      = "cat " ++ mlfFile ++ " | malfunction eval"
  compileCommand   = "malfunction compile -o " ++ outFile ++ " " ++ mlfFile

handler :: SomeException -> IO ()
handler ex = putStrLn $ "Caught exception: " ++ show ex

stringify :: [(Name, LDecl)] -> String
stringify = unwords . map ((\decl -> show decl ++ "\n\n\n") . snd)

replaceExtensionIf :: FilePath -> String -> String -> FilePath
replaceExtensionIf file curr new = case stripExtension curr file of
  Just fileNoExt -> fileNoExt <.> new
  _              -> file <.> new
