import System.Environment
import System.Exit
import System.IO
import Data.Array.IArray

import Data.ByteString.Char8 (pack)
import Graphics.Vty

-- pad left fills the left side with pad chars, pad right fills the right side
-- with pad chars
data Pad = Pad { padLeft     :: String -> String
               , padRight    :: String -> String }

type Frame = [String]

-- an immutable array of frames, one-indexed
type Presentation = Array Int Frame

newPad :: Char -> Int -> Pad
newPad c w = Pad { padLeft     = padl
                 , padRight    = padr }
    where
      pad' t = replicate (w - length t) c
      shorten = take w
      padl t = pad' t ++ shorten t
      padr t = shorten t ++ pad' t

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
      t = padRight p (' ' : title)
      sep = replicate w '-'
      body' = take (min (h - 3) (length body)) body -- maybe shorten body
      bodyLines = [padRight p (' ':b) | b <- body']
      vsep = [padRight p "" | _ <- [1 .. h - length body - 3]]
      foot = padLeft p (show num ++ " / " ++ show tot)
      text = foldl (++) [] [[t, sep], bodyLines, vsep, [foot]]


-- |Take a Vty and a list of frames, and do the presentation; this function
-- terminates when q or Ctrl-C are entered.
loop :: Vty -> Presentation -> IO ()
loop vty pres = loop' minFrame
    where
      (minFrame, maxFrame) = bounds pres
      loop' n = do
        let frame = pres ! n
        dims <- getSize vty
        blankVty vty
        update vty $ pic { pCursor = NoCursor,
                           pImage  = renderFrame frame dims (n, maxFrame) }
        eventLoop
        where
          eventLoop = do ev <- getEvent vty
                         case ev of
                           EvKey KLeft _  -> loop' (max minFrame (n-1))
                           EvKey KRight _ -> loop' (min maxFrame (n+1))
                           EvKey KHome _  -> loop' minFrame
                           EvKey KEnd _   -> loop' maxFrame
                           EvKey (KASCII 'q') _       -> return ()
                           EvKey (KASCII 'c') [MCtrl] -> return ()
                           EvKey (KASCII 'r') _ -> blankVty vty >> loop' n
                           EvResize _ _   -> blankVty vty >> loop' n
                           _              -> eventLoop

-- |Sometimes the vty module gets confused and leaves artifacts from previous
-- slides on the screen. Calling blankvty a few times seems to fix this.
blankVty :: Vty -> IO ()
blankVty vty = do
  (w, h) <- getSize vty
  let lines = [replicate w ' ' | _ <- [1..h]]
  update vty $ pic { pCursor = NoCursor
                   , pImage = vertcat $ map toImage lines}
  refresh vty

-- |Parse an input file, representing a presentation, into a presentation
makeFrames :: String -> Presentation
makeFrames xs = listArray (1, length frames) frames
    where
      frames = filter (\p -> head (head p) /= '#') frames'
      frames' = concatMap copyFrames $ basicSplit xs
      basicSplit = split "--" . lines
      copyFrames = cf [] . split "."
      cf _ [] = []
      cf h (x:xs) = let p = h ++ x in p : cf (p ++ [[]]) xs

main = do
  args <- getArgs
  presentation <- parse args
  h <- openFile presentation ReadMode
  frames <- fmap makeFrames $ hGetContents h
  vty <- mkVty
  loop vty frames
  shutdown vty
  where
    parse ["-h"] = usage
    parse [x]    = return x
    parse _      = usage

    usage = putStrLn "Usage: hspresent [-h] <presentation>" >> exit
    exit = exitWith ExitSuccess
