{-# LANGUAGE FlexibleInstances #-}

module HuskyML where

class Attribute a where
  diff :: a -> a -> Double
  times :: a -> a -> Double

data Feature = FString String | FInt Int | FDouble Double | FChar Char deriving (Eq, Ord)

instance Attribute Feature where
  diff (FString s1) (FString s2) = fromIntegral $ (length s1) - (length s2)
  diff (FInt s1) (FInt s2) = fromIntegral $ s1 -  s2
  diff _ _ = error "unable to diff a0 a1"
  times (FString s1) (FString s2) = 0.0
  times (FInt s1) (FInt s2) = fromIntegral $ s1 * s2
  times _ _ = error "unable to times a0 a1"


-- | Represents an item with list of features and a label
data Classified a = Classified {features::[Feature],
                                label::a
                               }
