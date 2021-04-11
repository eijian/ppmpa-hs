--
-- Ray tracer w/Photon map
--
-- compile: ghc -o rt RT.hs
-- usage  : ./rt [screen info file] < [photonmapfile] > [imagefile.ppm]

module Main where

--import Data.List
import           Control.Monad
import qualified Data.Time as TM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           System.Environment
import           System.IO

import           Antialias
import           Camera
import           Scene
import           Tracer

usage :: String
usage = "Usage: rt <camera file> <scene file> [<radius>] < [photon map file]"

useClassic :: Bool
useClassic = True
radiusDef :: Double
radiusDef = 0.1

-- FUNCTIONS --

main :: IO ()
main = do
  -- read scene information
  as <- getArgs
  (fn1, fn2, radius) <- if length as >= 2
    then
      if length as == 3
        then do
          let r = read (as !! 2) :: Double
          return (as !! 0, as !! 1, r * r)
        else return (as !! 0, as !! 1, radiusDef * radiusDef)
    else error usage
  cam <- readCamera fn1
  (lgts, objs) <- readScene fn2

  -- read photon map
  t0 <- TM.getCurrentTime
  (msize, photonmap) <- readMap (nSamplePhoton cam) radius
  hPutStr stderr ("finished reading map:" ++ (show msize) ++ " photons, ")
  t1 <- TM.getCurrentTime
  hPutStrLn stderr (show (TM.diffUTCTime t1 t0))

  -- tracing image
  let
    uc = useClassic
    tracer = traceRay cam m_air 0 objs lgts photonmap radius uc
  rays <- V.mapM (generateRay cam) $ screenMap cam
  image <- V.mapM tracer rays

  -- output image data with/without anti-aliasing
  mapM_ putStrLn $ pnmHeader cam
  if (progressive cam) == True
    then
      forM_ [0..(V.length image - 1)] $ \i -> do
        --TIO.putStrLn $ radianceToText (image V.! i)
        putStrLn $ radianceToString (image V.! i)
    else do
      let pixels = V.map (radianceToRgb cam) image
      forM_ [0..(V.length pixels - 1)] $ \i -> do
        rgb <- smooth tracer cam pixels i
        --TIO.putStrLn $ rgbToText rgb
        putStrLn $ rgbToString rgb
