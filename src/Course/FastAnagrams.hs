{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams cs fn = (fastAnagramsPure cs) <$> lines <$> (readFile fn)

fastAnagramsPure ::
  Chars
  -> List Chars
  -> List Chars
fastAnagramsPure cs dic = (listh . S.toList . S.intersection perms) dicset
  where 
    dicset = (S.fromList . hlist) dic
    perms  = (S.fromList . hlist) (permutations cs)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
