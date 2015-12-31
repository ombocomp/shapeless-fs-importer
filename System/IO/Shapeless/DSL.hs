{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module System.IO.Shapeless.DSL where

import Control.Monad.Free
import Data.Foldable
import System.FilePath

data ShapelessDSL a =
   RenameFile FilePath String a
   deriving (Show, Eq, Functor)


type ShapelessAPI a = Free ShapelessDSL a

renameFile :: FilePath -> String -> ShapelessAPI ()
renameFile fp fn = liftF $ RenameFile fp fn ()

renameFiles :: Foldable f => f (FilePath, String) -> ShapelessAPI ()
renameFiles = foldl' func (pure ())
   where
      func acc (fp, fn) = acc *> liftF (RenameFile fp fn ())

