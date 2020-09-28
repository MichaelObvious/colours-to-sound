{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Lib

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable
import           System.Process
import           Text.Printf

type Hz = Float
type Seconds = Float

sampleRate :: Float
sampleRate = 48_000

volume :: Float
volume = 0.5

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
main = undefined
