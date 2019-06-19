module Data.Registry.Internal.Feat (
  bconcat
, breadthConcat
, breadthSelection
, limitPartsSize
, limitPartsSizeWith
) where

import           Prelude             ((!!))
import           Protolude
import           Test.Feat.Enumerate

-- | Limit the size of an enumerate so that each part has a maximum number of elements
limitPartsSize :: Integer -> Enumerate a -> Enumerate a
limitPartsSize maxSize = limitPartsSizeWith (repeat maxSize)

-- | Limit the size of an enumerate with a distribution of maximum values
limitPartsSizeWith :: [Integer] -> Enumerate a -> Enumerate a
limitPartsSizeWith maxSizes e = fromParts (uncurry limitPartSize <$> mzip 0 maxSizes (parts e))

-- | Zip with default values from monoids for padding
--   from https://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
mzip :: a -> [a] -> [b] -> [(a, b)]
mzip defaultA (a:as) (b:bs) = (a, b) : mzip defaultA as bs
mzip defaultA []     (b:bs) = (defaultA, b) : mzip defaultA [] bs
mzip _ _ _                  = []

-- | Limit the size of a part
limitPartSize :: Integer -> Finite a -> Finite a
limitPartSize maxSize (Finite card sel) = Finite (min card maxSize) sel

-- | Concatenate enumerates in a breadth-first manner
--  IMPORTANT NOTE: this only works if those enumerates have finite parts!
bconcat :: [Enumerate a] -> Enumerate a
bconcat []    = mempty
bconcat [a]   = a
bconcat xs    = Enumerate
  (toRev . map breadthConcat . transpose $ map parts xs)

-- | Concat finite parts breadth-first by picking an element of each part,
--   instead of going for all the elements of the first part, then all the elements of the second
--   etc...
--  IMPORTANT NOTE: this only works for a finite list of parts!
breadthConcat :: [Finite a] -> Finite a
breadthConcat fs = Finite
  (sum $ map fCard fs)
  (breadthSelection $ filter ((>0) . fCard) fs)

-- | Return a selection function enumerating elements breadth first from a list of parts
--         p1  p2  p3 p4
--         0   1   2  3
--         4   5   6  7
--         8   9      10
--        11          12
breadthSelection :: [Finite a] -> (Index -> a)
breadthSelection fs =
  if null fs then (\index -> panic $ "index not found " <> show index)
  else
    let totalNumberOfRows = maximum (map fCard fs)
        rows              = [0..(totalNumberOfRows - 1)]
        rowsSums          = drop 1 $ scanl ((+)) 0 (fmap (numberOfValuesOnRow fs) rows)
    in \index ->
         let rowsSumsBefore    = takeWhile (<= index) rowsSums
             valuesBefore      = fromMaybe 0 (lastMay rowsSumsBefore)
             row               = fromIntegral $ length rowsSumsBefore
             column            = index - valuesBefore
             partsWithValues   = partsOnRow fs row
             -- traceShow ("total number of rows", totalNumberOfRows, "values on rows", fmap (numberOfValuesOnRow fs) rows, "rowsSums", rowsSums, "rowsSumsBefore", rowsSumsBefore, "valuesBefore", valuesBefore, "row", row, "column", column) $
          in  fIndex (partsWithValues !! (fromIntegral column)) row

-- | Return all the parts having an element on a given row
numberOfValuesOnRow :: [Finite a] -> Integer -> Integer
numberOfValuesOnRow fs row = fromIntegral . length $ partsOnRow fs row

-- | Return all the parts having a value on a given row
partsOnRow :: [Finite a] -> Integer -> [Finite a]
partsOnRow fs rowNumber = reverse $ go [] fs rowNumber
  where
    go res [] _ = res
    go res (f:rest) r =
      if r < fCard f then
        go (f:res) rest r
      else
        go res rest r
