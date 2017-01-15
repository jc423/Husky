module HuskyML where

class Attribute a where
  diff :: a -> a -> Double
  times :: a -> a -> Double

class TrainingPt a where
  features :: a -> [Feature]

instance TrainingPt (Classified a) where
  features (Classified xs _) = xs

instance TrainingPt Unclassified where
  features (Unclassified xs) = xs

  
data Feature = FString String | FInt Int | FDouble Double | FChar Char deriving (Eq, Ord, Show)

instance Attribute Feature where
  diff (FString s1) (FString s2) = fromIntegral $ (length s1) - (length s2)
  diff (FInt s1) (FInt s2) = fromIntegral $ s1 -  s2
  diff (FDouble d1) (FDouble d2) = d1 - d2
  diff (FChar c1) (FChar c2) = fromIntegral $ fromEnum c1 - fromEnum c2
  diff _ _ = error "unable to diff a0 a1"
  times (FString s1) (FString s2) = fromIntegral $ (length s1) * (length s2)
  times (FInt s1) (FInt s2) = fromIntegral $ s1 * s2
  times (FDouble d1) (FDouble d2) = d1 * d2
  times (FChar c1) (FChar c2) = fromIntegral $ fromEnum c1 *  fromEnum c2
  times _ _ = error "unable to times a0 a1"


-- | Represents a classified data point
data Classified a = Classified [Feature] a deriving (Show, Eq)

-- | Represents unclassified data point
data Unclassified = Unclassified [Feature] deriving (Show, Eq)

label :: Classified a -> a
label (Classified _ x) = x


