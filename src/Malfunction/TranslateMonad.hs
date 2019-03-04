module Malfunction.TranslateMonad where

import           Data.Map.Strict               as M
import           Idris.Core.TT                  ( Name )
import           Control.Monad                  ( ap )

newtype Translate a =MkTrn ( M.Map Name (Int, Int) -> Either String a)

instance Functor Translate where
    fmap f (MkTrn t) = MkTrn $ \m -> case t m of
        Right a   -> Right (f a)
        Left  err -> Left err

instance Applicative Translate where
    pure a = MkTrn $ \m -> Right a
    (<*>) = ap

instance Monad Translate where
    MkTrn t >>= f = MkTrn $ \m -> case t m of
        Right a   -> let MkTrn h = f a in h m
        Left  err -> Left err

runTranslate :: Translate a -> M.Map Name (Int, Int) -> Either String a
runTranslate (MkTrn t) = t

ask :: Translate (M.Map Name (Int, Int))
ask = MkTrn $ \m -> Right m

crashWith :: String -> Translate a
crashWith err = MkTrn $ \m -> Left err
