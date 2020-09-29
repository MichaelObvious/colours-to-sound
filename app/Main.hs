{-# LANGUAGE NumericUnderscores #-}

{-
TODO:
  bpm
  attack
-}

module Main where

import           Lib

import           Codec.Picture
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import           Data.Fixed
import           Data.Foldable
import           Data.List
import qualified Data.Vector.Storable as V
import           GHC.Word
import           System.Environment
import           System.Process
import           Text.Printf

type RGB = (Float, Float, Float)
type HSV = (Float, Float, Float)
type Hz = Float
type Seconds = Float
type Pulse = Float

sampleRate :: Float
sampleRate = 48_000

volume :: Float
volume = 0.75

pitchStandard :: Hz
pitchStandard = 440.0

imageData2rgb :: [Word8] -> [RGB]
imageData2rgb [] = []
imageData2rgb (r:g:b:xs) = [((fromIntegral r) / 255.0, (fromIntegral g) / 255.0, (fromIntegral b) / 255.0)] ++ imageData2rgb xs

imageToPixels :: DynamicImage -> [RGB]
imageToPixels image = imageData2rgb $ V.toList $ imageData $ convertRGB8 image

rgb2hsv :: RGB -> HSV
rgb2hsv (r, g, b)
  | m_big == m_little = (0.0, s, v)
  | m_big == r = (((g - b) / c `mod'` 6.0) / 6.0, s, v)
  | m_big == g = (((b - r) / c + 2.0) / 6.0, s, v)
  | m_big == b = (((r - g) / c + 4.0) / 6.0, s, v)
   -- should never happen
  | otherwise = undefined

  where
    infinity = read "Infinity"
    m_big = foldl max 0 [r, g, b]
    m_little = foldl min infinity [r, g, b]
    c = m_big - m_little
    s = if m_big == 0 then 0 else c
    v = m_big

hsv2hz :: HSV -> Hz
hsv2hz (h, _, v) = hv^2 * (max_hz - min_hz) + min_hz
  where
    hv = h * v
    max_hz = 15_000
    min_hz = 30

hsv2note :: HSV -> Hz
hsv2note (h, _, v) = note semitone_d
  where
    hv = h * v
    semitone_d = fromIntegral $ round (hv * 88.0 - 39.0)

    note :: Float -> Hz
    note n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

hsv2volume :: HSV -> Float
hsv2volume (_, s, _) = s * volume

image2wave :: (HSV -> Hz) -> DynamicImage -> [Pulse]
image2wave hsvtofreq image = concat $ map (applyADSR . hsv2wave . head) $ group hsvs
  where 
    hsvs = map rgb2hsv $ imageToPixels image
    hsv2wave hsv = wave (hsvtofreq hsv) (hsv2volume hsv) 1.0
    applyADSR wave = zipWith (*) [adsr (n / (fromIntegral $ length wave)) | n <- [0.0 .. (fromIntegral $ length wave) :: Float]] wave

adsr :: Pulse -> Pulse -- attack decay sustain release
adsr x
  | x <= 0.1 = a x
  | x <= 0.4 = s1 x
  | x <= 0.5 = d x
  | x <= 0.8 = s2 x
  | otherwise = r x
  where
    a n = 10 * n
    s1 n = 1.0
    d n = 1.4 - n
    s2 n = 0.9
    r n = -5 * n + 5

wave :: Hz -> Float -> Seconds -> [Pulse]
wave freq vol duration = map ((* vol) . sin . (* step)) [0.0 .. sampleRate * duration]
  where
    step = (freq * 2 * pi) / sampleRate

save :: FilePath -> [Pulse] -> IO ()
save fileName samples = B.writeFile fileName $ B.toLazyByteString $ fold $ map B.floatLE samples

-- Only for testing in the REPL
play :: [Float] -> IO ()
play samples = do
  save filePath samples
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate filePath
  return ()
  where filePath = "test.bin"

main :: IO ()
main = do
  path:_ <- getArgs
  eimg <- readImage path
  case eimg of
    Left _ -> error "Could not load image"
    Right image -> play $ image2wave hsv2note image
    _ -> error "Unsupported pixel format"
