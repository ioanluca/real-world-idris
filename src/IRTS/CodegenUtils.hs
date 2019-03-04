module IRTS.CodegenUtils
    ( asConnectedComponents,
      getNameTagArityMap
    )
where

import           IRTS.Lang
import           Idris.Core.TT                  ( Name )
import qualified Data.Graph                    as Graph
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( mapMaybe )


toNode :: LDecl -> Maybe (LDecl, Name, [Name])
toNode decl@(LFun _ name _ body) = Just (decl, name, Set.toList (free body))
toNode _                         = Nothing

freeCase :: LAlt -> Set.Set Name
freeCase (LConCase _ name names exp) =
    Set.unions $ Set.singleton name : free exp : map Set.singleton names
freeCase (LConstCase _ exp) = free exp
freeCase (LDefaultCase exp) = free exp

free :: LExp -> Set.Set Name
free (LV name           ) = Set.singleton name
free (LApp _ exp exps   ) = Set.unions $ free exp : map free exps
free (LLazyApp name exps) = Set.unions $ Set.singleton name : map free exps
free (LLazyExp exp      ) = free exp
free (LForce   exp      ) = free exp
free (LLet name exp1 exp2) =
    Set.unions [Set.singleton name, free exp1, free exp2]
free (LLam  names exp   ) = Set.unions $ free exp : map Set.singleton names
free (LProj exp   _     ) = free exp
free (LCon _ _ name exps) = Set.unions $ Set.singleton name : map free exps
free (LCase _ exp alts  ) = Set.unions $ free exp : map freeCase alts
free (LConst _          ) = Set.empty
free (LForeign _ _ args ) = Set.unions $ map (free . snd) args
free (LOp _ exps        ) = Set.unions $ map free exps
free LNothing             = Set.empty
free (LError _)           = Set.empty

asConnectedComponents :: [LDecl] -> [Graph.SCC LDecl]
asConnectedComponents = Graph.stronglyConnComp . mapMaybe toNode

getNameTagArityMap :: [LDecl] -> Map.Map Name (Int, Int)
getNameTagArityMap decls = Map.fromList
    [ (name, (tag, arity)) | (LConstructor name tag arity) <- decls ]



