module Main where

import           Codec.Binary.UTF8.String ( encode )
import           Data.Algorithm.Diff
import           Data.ByteString          ( ByteString, pack )
import           Rainbow

main :: IO ()
main = printDiffOf _sOld _sNew

printDiffOf :: String -> String -> IO ()
printDiffOf sOld sNew = do
  let sDiff = getDiff sOld sNew
  putStrLn "Original sentences:"
  mapM_ printCharWithColor [x | x <- sDiff, not $ isSecond x]
  putStrLn "\n\nAfter revision:"
  mapM_ printCharWithColor [x | x <- sDiff, not $ isFirst x]
  putStrLn "\n\nThe diff all:"
  mapM_ printCharWithColor sDiff
  putStrLn ""

isBoth :: Diff a -> Bool
isBoth (Both _ _) = True
isBoth _          = False

isFirst :: Diff a -> Bool
isFirst (First _) = True
isFirst _         = False

isSecond :: Diff a -> Bool
isSecond (Second _) = True
isSecond _          = False

fromDiff :: Diff a -> a
fromDiff (First a)  = a
fromDiff (Second a) = a
fromDiff (Both a _) = a

printCharWithColor :: Diff Char -> IO ()
printCharWithColor d
  | isFirst d = putChunk $ (chunk . toBS . fromDiff) d & fore red
  | isSecond d = putChunk $ (chunk . toBS . fromDiff) d & fore green
  | otherwise = putChunk $ (chunk . toBS . fromDiff) d & fore black
  where
    toBS :: Char -> ByteString
    toBS c = pack . encode $ [c]

_sOld =
  "Hello, my name is Tomato. I'm from South America.\nMy face had turned to red from yellow with breeding.\nNice to meet you."

_sNew =
  "Hello, world. My name is Tomato. I'm from western South America.\nMy genome sequensing had been completed in 2012.\nMy face had turned to red from yellow with breeding. "

_sDiff = getDiff _sOld _sNew
