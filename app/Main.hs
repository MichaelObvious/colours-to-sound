{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Lib

import           Codec.Picture
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import           Data.Fixed
import           Data.Foldable
import qualified Data.Vector.Storable as V
import           GHC.Word
import           System.Environment
import           System.Process
import           Text.Printf

type Hz = Float
type Seconds = Float

sampleRate :: Float
sampleRate = 48_000

volume :: Float
volume = 0.5

imageData2rgb :: [Word8] -> [(Float, Float, Float)]
imageData2rgb [] = []
imageData2rgb (r:g:b:xs) = [((fromIntegral r) / 255.0, (fromIntegral g) / 255.0, (fromIntegral b) / 255.0)] ++ imageData2rgb xs

imageToPixels :: DynamicImage -> [(Float, Float, Float)]
imageToPixels image = imageData2rgb $ V.toList $ imageData $ convertRGB8 image

rgb2hue :: (Float, Float, Float) -> Float
rgb2hue (r, g, b)
  | m_big == m_little = 0
  | m_big == r = ((g - b) / c `mod'` 6.0) / 6.0
  | m_big == g = ((b - r) / c + 2.0) / 6.0
  | m_big == b = ((r - g) / c + 4.0) / 6.0
   -- should never happen
  | otherwise = undefined
  
  where
    infinity = read "Infinity"
    m_big = foldl max 0 [r, g, b]
    m_little = foldl min infinity [r, g, b]
    c = m_big - m_little

hue2hz :: Float -> Hz
hue2hz hue = (hue * 389 + 400) * 10 ^ 12

wave :: Hz -> Seconds -> [Float]
wave freq duration = map ((* volume) . sin . (* step)) [0.0 .. sampleRate * duration]
  where
    step = (freq * 2 * pi) / sampleRate

save :: [Float] -> FilePath -> IO ()
save samples fileName = B.writeFile fileName $ B.toLazyByteString $ fold $ map B.floatLE samples

-- Only for testing in the REPL
play :: [Float] -> IO ()
play samples = do
  save samples filePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate filePath
  return ()
  where filePath = "test.bin"

main :: IO ()
main = do
  path:_ <- getArgs
  eimg <- readImage path
  case eimg of
    Left _ -> error "Could not load image"
    Right image -> putStrLn $ show $ imageToPixels image
    _ -> error "Unsupported pixel format"
