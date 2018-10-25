module Common.FileType
  ( FileType(..)
  ) where

data FileType
  = JavaScript
  | OldTypeScript
  | TypeScript
  deriving (Show, Eq)
