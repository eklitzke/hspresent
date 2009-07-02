import System.Environment
import System.Exit
import System.IO

import Data.ByteString.Char8 (pack)
import Graphics.Vty

-- pad left fills the left side with pad chars, pad right fills the right side
-- with pad chars
data Pad = Pad { padLeft     :: String -> String
               , padRight    :: String -> String }

type Frame = [String]

newPad :: Char -> Int -> Pad
newPad c w = Pad { padLeft     = padl
                 , padRight    = padr }
    where
      pad' t = replicate (w - length t) c
      padl t = pad' t ++ t
      padr t = t ++ pad' t

-- |Split an input list
split :: Eq a => [a] -> [[a]] -> [[[a]]]
split _ [] = [[]]
split tok (x:xs)
      | tok == x  = [] : split tok xs
      | otherwise = let (x':xs') = split tok xs in (x:x') : xs'

-- |Convert a string to an image
toImage :: String -> Image
toImage = renderBS attr . pack

-- |Render a frame. The first pair of Ints is the tuple (screen width, screen
-- height), the second pair of Ints is the two numbers to display in the lower
-- right corner, likely the tuple (slide num, total slides).
renderFrame :: Frame -> (Int, Int) -> (Int, Int) -> Image
renderFrame (title:body) (w, h) (num, tot) = vertcat $ map toImage text
    where
      p = newPad ' ' w
      q = newPad '-' w
      t = padRight p (' ' : title)
      sep = replicate w '-'
      body' = [padRight p (' ':b) | b <- body]
      vsep = [padRight p "" | _ <- [1 .. h - length body - 3]]
      foot = padLeft p (show (num + 1) ++ " / " ++ show tot)
      text = foldl (++) [] [[t, sep], body', vsep, [foot]]
      

-- |Take a Vty and a list of frames, and do the presentation; this function
-- terminates when q or Ctrl-C are entered.
loop :: Vty -> [Frame] -> IO ()
loop vty frames = loop' 0
    where
      frameLen = length frames
      maxframe = frameLen - 1
      loop' n = do
        let frame = frames !! n
        dims <- getSize vty
        update vty $ pic { pCursor = NoCursor,
                           pImage  = renderFrame frame dims (n, frameLen) }
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
  loop vty frames
  shutdown vty
  where
    makeFrames = split "--" . lines

    parse ["-h"] = usage
    parse [x]    = return x
    parse _      = usage

    usage = putStrLn "Usage: hspresent [-h] <presentation>" >> exit
    exit = exitWith ExitSuccess
