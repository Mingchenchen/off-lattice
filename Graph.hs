{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO
import System.Directory
import Control.Monad
import Text.Regex.Posix
import System.Exit
import System.FilePath
import LA


inTheRightDir :: FilePath -> Bool
inTheRightDir = (=~ ".*/off-lattice/?$")



main :: IO ()
main = do
  d <- getCurrentDirectory
  case inTheRightDir d of
    False -> do
               putStrLn "Please place this program in the \"*/off-lattice\" folder before running it"
               exitFailure
    True  -> do
               findAndBuildGraphs d
               exitSuccess



findAndBuildGraphs :: FilePath -> IO ()
findAndBuildGraphs d = do
    cs  <- getDirectoryContents d
    ds' <- filterM doesDirectoryExist $ (map (d </>)) cs
    let ds = filter (\x -> not (x =~ ".*[.][.]?$" || x =~ "[.].*")) ds'
    case any (=~ ".*/hpchains/?$") ds of
      True  -> do
        buildGraphs d
      False -> do
        mapM_ findAndBuildGraphs ds


buildGraphs :: FilePath -> IO ()
buildGraphs d = do
  let c = d </> "hpchains"
  let g = d </> "graphs"
  createDirectoryIfMissing False g
  fs <- getDirectoryContents c >>= filterM doesFileExist . map (c </>)
  mapM_ (f g) fs
  where
    f c cp = do
      let end = cp =~ "[0-9]*.csv" :: String
      let g = c </> "graph" ++ end
      readFile cp >>= return . toGraph >>= writeFile g

toGraph !str = unlines $ map (\x -> unwords $ map (ints x) vs) vs
  where
    vs = parse str
    ints a b = if dist a b <= 2.5 then "1" else "0"

parse :: String -> [(Double, Double, Double)]
parse !str = map fst $ filter (isP . snd) $ zip vs hps
  where
    vs = vects $ map read $ filter (not . isHPN) $ words $ str
    hps = filter isHPN $ words $ str

    vects (x:y:z:xs) = (x,y,z) : vects xs
    vects _          = []

    isP "P" = True
    isP _   = False

    isHPN "H" = True
    isHPN "P" = True
    isHPN "N" = True
    isHPN _   = False

















