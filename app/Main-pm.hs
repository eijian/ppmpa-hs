--
-- Photon map generator (revision 2)
--
-- compile: ghc -o pm PM.hs
-- usage  : ./pm [screen info file] > photonmapfile

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import           System.Environment

import           Scene
import           Tracer
import           Ray.Algebra
import           Ray.Light
import           Ray.Object
import           Ray.Optics

usage :: String
usage = "Usage: pm <scene file> [<#photon>]  (output photon map to stdout)"

nPhoton :: Int
nPhoton = 100000
useClassic :: Bool
useClassic = True

main :: IO ()
main = do
  as <- getArgs
  (fn, nphoton) <- if length as >= 1
    then
      if length as == 2
        then return (as !! 0, read (as !! 1) :: Int)
        else return (as !! 0, nPhoton)
    else error usage
  (lgts, objs) <- readScene fn
  let
    pw = (V.sum $ V.map flux lgts) / (fromIntegral nphoton)
    ns = V.map (calcN pw) lgts
  putStrLn $ show nphoton
  putStrLn $ show pw
  V.zipWithM_ (outputPhotonCaches useClassic objs) lgts ns
  
calcN :: Double -> Light -> Int
calcN pw light = round (flux light / pw)

outputPhotonCaches :: Bool -> V.Vector Object -> Light -> Int -> IO ()
outputPhotonCaches uc objs lgt n = V.mapM_ (outputPhotonCache uc lgt objs) $ V.replicate n 1

outputPhotonCache :: Bool -> Light -> V.Vector Object -> Int -> IO ()
outputPhotonCache uc lgt objs _ =
  generatePhoton lgt >>= tracePhoton uc m_air objs 0 >>= V.mapM_ (putStrLn.showPhoton)

showPhoton :: PhotonCache -> String
showPhoton (wl, (Vector3 px py pz, Vector3 dx dy dz)) = show wl ++ " " 
  ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " "
  ++ show dx ++ " " ++ show dy ++ " " ++ show dz
