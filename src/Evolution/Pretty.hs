{-# LANGUAGE FlexibleInstances #-}

module Evolution.Pretty where

import Data.List (intercalate)
import Data.Semigroup (Max (Max, getMax))
import Evolution.Run (Individual (..))
import Pretty (Pretty (..))

-- * Pretty Print Individual

newtype Zip a = Zip {getZip :: [a]} deriving (Show)

instance Semigroup a => Semigroup (Zip a) where
  Zip (a : as) <> Zip (b : bs) = Zip (a <> b : getZip (Zip as <> Zip bs))
  _ <> _ = Zip []

instance Show a => Pretty [Individual a] where
  pretty is = unlines $ [separator '┬', paddedHeader, separator '┼'] <> padded <> [separator '┴']
    where
      showInd i = [show $ _fitness i, pretty $ _indTree i]
      shown = map showInd is
      header = [["Fitness", "Tree"]]
      widths = map getMax $ getZip $ foldl1 (<>) $ map (Zip . map (Max . length)) (shown <> header)
      pad = intercalate " │ " . zipWith (\p s -> s ++ replicate (p - length s) ' ') widths
      paddedHeader = pad . head $ header
      separator m = intercalate ['─', m, '─'] $ map (`replicate` '─') widths
      padded = map pad shown