-- This is the original template, by Dr. Ben Stephenson
-- Taken from: http://nifty.stanford.edu/2018/stephenson-mondrian-art/
--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Other constants used during the generation of the image
--
split_low = 120 :: Int
split_penalty = 1.5 :: Float
fill_prob = 0.25 :: Float

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Generate the tag for a rectangle with random color.  Replace the 
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:rest: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian _ _ 0 _ rvals = (rvals, "")
mondrian _ _ _ 0 rvals = (rvals, "")
mondrian x y w h (r:s:rest)
  | w > width `div` 2 && 
    h > height `div` 2 = b_split x y w h (r:s:rest)
  | w > width `div` 2  = h_split x y w h (r:s:rest)
  | h > height `div` 2 = v_split x y w h (r:s:rest)
  | hs && vs           = b_split x y w h rest
  | hs                 = h_split x y w h rest
  | vs                 = v_split x y w h rest
  | otherwise = (s:rest, "<rect x=" ++ (show x) ++ 
                         " y=" ++ (show y) ++ 
                         " width=" ++ (show w) ++ 
                         " height=" ++ (show h) ++ 
                         " stroke=\"black\" stroke-width=\"3\" fill=\"" ++ 
                         (randomColor r) ++ 
                         "\" />\n")
  where 
    rand1 = randomInt split_low (round (fromIntegral w * split_penalty)) r
    hs = if rand1 < w then True else False
    rand2 = randomInt split_low (round (fromIntegral h * split_penalty)) s
    vs = if rand2 < h then True else False

--
--  Split the region both horizontally and vertically
--
b_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
b_split x y w h (r:s:rest) = (rest4, s1 ++ s2 ++ s3 ++ s4)
  where 
    h_rand = randomInt 33 67 r
    v_rand = randomInt 33 67 s
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y lw th rest
    (rest2, s2) = mondrian (x + lw) y rw th rest1
    (rest3, s3) = mondrian x (y + th) lw bh rest2
    (rest4, s4) = mondrian (x + lw) (y + th) rw bh rest3

--
--  Split the region horizontally so that we get two that are side by side
--
h_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
h_split x y w h (r:rest) = (rest2, s1 ++ s2)
  where 
    h_rand = randomInt 33 67 r
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    (rest1, s1) = mondrian x y lw h rest
    (rest2, s2) = mondrian (x + lw) y rw h rest1

--
--  Split the region vertically so that we get one on top the other
--
v_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
v_split x y w h (r:rest) = (rest2, s1 ++ s2)
  where 
    v_rand = randomInt 33 67 r
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y w th rest
    (rest2, s2) = mondrian x (y + th) w bh rest1

--
--  Select the random fill color for the region
--
randomColor :: Float -> String
randomColor rval
  | rval < 1.0 * fill_prob / 3.0 = "red"
  | rval < 2.0 * fill_prob / 3.0 = "skyblue"
  | rval < 3.0 * fill_prob / 3.0 = "yellow"
  | otherwise                    = "white"

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
