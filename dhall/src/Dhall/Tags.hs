{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Dhall.Tags where

import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Dhall.Core (Binding(..), Expr(..))
import           Dhall.Parser (Src(..))
import qualified Text.Megaparsec.Pos

newtype Tags = Tags (Map Text (Map FilePath IntSet))
  deriving Show

instance Semigroup Tags where
  Tags m0 <> Tags m1 = Tags (Map.unionWith (Map.unionWith IntSet.union) m0 m1)

instance Monoid Tags where
  mempty = empty

empty :: Tags
empty = Tags Map.empty

singleton :: Text -> FilePath -> Int -> Tags
singleton name fp linePos =
  Tags (Map.singleton name (Map.singleton fp (IntSet.singleton linePos)))

srcSingleton :: Src -> Text -> Tags
srcSingleton (Src startPos _ _) name =
    singleton
      name
      (Text.Megaparsec.Pos.sourceName startPos)
      (Text.Megaparsec.Pos.unPos (Text.Megaparsec.Pos.sourceLine startPos))

tags :: Expr Src a -> Tags
tags e0 = loop empty e0
  where
    loop !t (Note src e1) = case e1 of
      Const _ -> t
      Var _ -> t
      Lam _ _ _ -> t -- TODO?
      Pi name e2 e3 -> loop (loop (t <> srcSingleton src name) e2) e3
      Let binds e2 -> loop (t <> foldMap (bindingTags src) binds) e2
      _ -> t
    loop _ _ = error "Expected Note"

bindingTags :: Src -> Binding Src a -> Tags
bindingTags src (Binding name mAnnot rhs) =
       srcSingleton src name
    <> foldMap tags mAnnot
    <> tags rhs

tagsToList :: Tags -> [(Text, FilePath, Int)]
tagsToList (Tags m) =
    Map.foldMapWithKey
        (\name locs ->
            Map.foldMapWithKey
                (\fp ls -> map (\l -> (name, fp, l)) (IntSet.toList ls))
                locs)
        m

ctags :: Tags -> Doc a
ctags = Pretty.vcat
      . map (\(name, fp, line) ->
                Pretty.pretty name <> tab <> Pretty.pretty fp <> tab <> Pretty.pretty line)
      . tagsToList
  where
    tab = Pretty.pretty '\t'
