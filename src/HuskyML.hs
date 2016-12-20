{-# LANGUAGE FlexibleInstances #-}

module HuskyML where

class Attribute a where
  diff :: a -> a -> Double
  times :: a -> a -> Double

instance Attribute String where
  diff s1 s2 = fromIntegral $ (length s1) - (length s2)
  times s1 s2 = fromIntegral $ (length s1) * (length s2)
  
instance Attribute Char where
  diff c1 c2 = fromIntegral $ fromEnum c1 - fromEnum c2
  times c1 c2 = fromIntegral $ fromEnum c1 * fromEnum c2
  
instance Attribute Double  where
  diff d1 d2 = d1 - d2
  times d1 d2 = d1 * d2

instance Attribute Integer  where
  diff d1 d2 = fromIntegral $ d1 - d2
  times d1 d2 = fromIntegral $ d1 * d2

instance Attribute Int  where
  diff d1 d2 = fromIntegral $ d1 - d2
  times d1 d2 = fromIntegral $ d1 * d2


-- | Represents an item with list of features and a label
data Classified a b = Classified {features::[b],
                                label::a
                               }

