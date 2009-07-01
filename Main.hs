import System.Environment
import System.Exit
import System.IO

import Data.ByteString.Char8 (pack)
import Graphics.Vty

split :: Eq a => [a] -> [[a]] -> [[[a]]]
split _ [] = [[]]
split tok (x:xs)
      | tok == x  = [] : split tok xs
      | otherwise = let (x':xs') = split tok xs in (x:x') : xs'

bsLine :: String -> Char -> Int -> Image
bsLine txt fill width = renderBS attr $ pack txt'
    where
      txt' = ' ' : txt ++ (replicate (width - length txt - 2) fill)

loop :: Vty -> [[String]] -> IO ()
loop vty frames = loop' 0
    where
      maxframe = length frames - 1
      formatText w [] = empty
      formatText w (x:xs) = (bsLine x ' ' w) <-> (formatText w xs)
      loop' n = do
        let (title:text) = frames !! n
        (w, h) <- getSize vty
        let im = bsLine title ' ' w <->
                 bsLine "" '-' w <->
                 formatText w text
        update vty (pic { pCursor = NoCursor,
                          pImage = im })
        ev <- getEvent vty
        case ev of
          EvKey KLeft _  -> loop' (max 0 (n-1))
          EvKey KRight _ -> loop' (min maxframe (n+1))
          EvKey (KASCII 'q') _       -> return ()
          EvKey (KASCII 'c') [MCtrl] -> return ()
          _              -> loop' n

main = do
  args <- getArgs
  presentation <- parse args
  h <- openFile presentation ReadMode
  frames <- fmap makeFrames $ hGetContents h
  vty <- mkVty
  refresh vty
  loop vty frames
  shutdown vty
  where
    makeFrames s = split "--" $ lines s

    parse ["-h"] = usage
    parse [x]    = return x
    parse _      = usage

    usage = putStrLn "Usage: hspresent [-h] <presentation>" >> exit
    exit = exitWith ExitSuccess
