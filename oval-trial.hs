-- WHAT DID I DO??
-- Cleaned up code (added comments, types etc.)
-- opacity / fade
-- lil write up for wiki
-- Reviewed questions
-- update guard to only allow up to 1 digit [1...4] to be entered

-- WHAT DO I WANT/NEED TO DO??
-- tests, this is all that's said on the proposal about testing:
--    Does the code work? They will first test it on test cases you suggest, and
--    then test it on their own test cases.
-- so do we need to write any????
-- meet up and make sure we both understand all code for presentation

--
-- Original Template by Dr. Ben Stephenson, from:
-- http://nifty.stanford.edu/2018/stephenson-mondrian-art/
-- Specifically, templates for global variables, randomList, rl_helper, randomInt,
--   mondrian, the split functions, fromList, and generating SVG tag in main were
--   used in this project.
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM, when, unless)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)
import Data.Char
import Data.String

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- split_low: Controls minimum split size
-- split_penalty: Controls size of our regions (smaller penalty means smaller regions)
--   = 1.5:  Big rounded circles or squares
--   = 0.75: Many tiny circles or squares
--
split_low = 100 :: Int
split_penalty = 0.75 :: Double

-- 
-- Generates and returns a list of 20000 random doubles between 0 and 1, 
-- for use in generating random colours if 'Random' was chosen.
--
randomList :: Int -> [Double]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

--
-- Helper for randomList by generating one random Double between 0 and 1.
--
rl_helper :: StdGen -> [Double]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Double) g

--
-- Generates and returns a list of 20000 random doubles between 0 and 17, 
-- for use in generating random colours if 'Gradient' was chosen.
-- Choose from 18 options as there are 3 different colour lists of 6 each.
--
gradientList :: [Double]
gradientList = take 20000 ([0..17] ++ gradientList)

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Double -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Generates portion of HTML for a rectangle or oval with a random colour.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   p: Value for rounded corners of oval, if desired
--   w, h: The width and height of the region
--   r:s:rest: A list of random floating point values between 0 and 1
--   c1, c2, c3: Colour hues chosen by user
--   h1, h2, h3: Colour hues for gradient list
--   g: True if 'gradient' was selected by user, random otherwise
--   z: 1 if 'Oval' was selected by user, rectangle otherwise	
--
-- Returns:
--   [Double]: The remaining, unused random values
--   [Char]: HTML that draws the image
--
-- rand1: Random number to determine whether region should split horizontally
-- rand2: Random number to determine whether region should split vertically
-- hs: Horizontal split, if rand1 < width of region, then split
-- vs: Vertical split, if rand2 < height of the region, then split
--
-- mondrian :: Int -> Int -> Int -> Int -> Int -> [Double] -> [Double] -> Bool -> [Char] -> [Char] -> [Char] -> ([Double], [Char])
mondrian _ _ _ 0 _ rvals gvals _ _ _ _ z = (rvals, "")
mondrian _ _ _ _ 0 rvals gvals _ _ _ _ z = (rvals, "")
mondrian x y p w h (r:s:rest) (h1:h2:h3) g c1 c2 c3 z
  | hs && vs           = b_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3 z
  | hs                 = h_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3 z
  | vs                 = v_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3 z
  | p <= 0             = c_curl x y p (w) h rest (h1:h2:h3) g c1 c2 c3 z
  | otherwise = (s:rest, "<rect x=" ++ (show x) ++ 
                         " y=" ++ (show y) ++
                         (if (z == '1') then (" rx=" ++ (show p) ++ " ry=" ++ (show p)) else (" rx=\"0\"" ++ " ry=\"0\"")) ++
                         " width=" ++ (show w) ++ 
                         " height=" ++ (show h) ++
                         " stroke=\"white\" stroke-width=\"0.5\" fill=\"" ++ 
                         (randomColor x y p w h r h2 g c1 c2 c3 z) ++
                         "\"" ++ 
                         " opacity=\"" ++ 
                         (if g then (fade y) else show 1.0) ++
                         "\" />\n") 
  where 
    rand1 = randomInt split_low (round (fromIntegral w * split_penalty)) r
    hs = if rand1 < w then True else False
    rand2 = randomInt split_low (round (fromIntegral h * split_penalty)) s
    vs = if rand2 < h then True else False

--
--  Split the region both horizontally and vertically
--
--b_split :: Int -> Int -> Int -> Int -> Int -> [Double] -> [Double] -> Bool -> [Char] -> [Char] -> [Char] -> ([Double], [Char])
b_split x y p w h (r:s:rest) (h1:h2:h3) g c1 c2 c3 z = (rest4, s1 ++ s2 ++ s3 ++ s4)
  where 
    h_rand = randomInt 37 67 r
    v_rand = randomInt 37 67 s
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y p lw th rest h3 g c1 c2 c3 z
    (rest2, s2) = mondrian (x + lw) y p rw th rest1 h3 g c1 c2 c3 z
    (rest3, s3) = mondrian x (y + th) p lw bh rest2 h3 g c1 c2 c3 z
    (rest4, s4) = mondrian (x + lw) (y + th) p rw bh rest3 h3 g c1 c2 c3 z

--
--  Split the region horizontally so that we get two that are side by side
--
-- h_split :: Int -> Int -> Int -> Int -> Int -> [Double] -> [Double] -> Bool -> [Char] -> [Char] -> [Char] -> ([Double], [Char])
h_split x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 z = (rest2, s1 ++ s2)
  where 
    h_rand = randomInt 37 67 r
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    (rest1, s1) = mondrian x y p lw h rest h3 g c1 c2 c3 z
    (rest2, s2) = mondrian (x + lw) y p rw h rest1 h3 g c1 c2 c3 z

--
--  Split the region vertically so that we get one on top the other
--
--v_split :: Int -> Int -> Int -> Int -> Int -> [Double] -> [Double] -> Bool -> [Char] -> [Char] -> [Char] -> ([Double], [Char])
v_split x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 z = (rest2, s1 ++ s2)
  where 
    v_rand = randomInt 37 67 r
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y p w th rest h3 g c1 c2 c3 z
    (rest2, s2) = mondrian x (y + th) p w bh rest1 h3 g c1 c2 c3 z

--
-- Round the region's edges to generate an oval 
--
-- c_curl :: Int -> Int -> Int -> Int -> Int -> [Double] -> [Double] -> Bool -> [Char] -> [Char] -> [Char] -> ([Double], [Char])
c_curl x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 z = (rest1, s1)
  where 
    p_rand = randomInt 37 67 r
    (rest1, s1) = mondrian x y (p + p_rand) w h rest h3 g c1 c2 c3 z

--
-- If 'Gradient' was selected, gradually increase opacity of the image vertically
--
fade :: Int -> String
fade y
 | y <= (height `div` 10) = show 0.01
 | y <= (height `div` 9) = show 0.05
 | y <= (height `div` 8) = show 0.1
 | y <= (height `div` 7) = show 0.2
 | y <= (height `div` 6) = show 0.25
 | y <= (height `div` 5) = show 0.3
 | y <= (height `div` 4) = show 0.5
 | y <= (height `div` 3) = show 0.7
 | y <= (height `div` 2) = show 0.8
 | otherwise = show 1.0

--
-- Select the random fill colour for the region, if 'Random' was selected. 
-- Colour generated depends on properties chosen by user, but is otherwise totally random.
--
-- randomColor :: Int -> Int -> Int -> Int -> Int -> Double -> Double -> Bool -> [Char] -> [Char] -> [Char] -> String
randomColor x y p w h r h1 g c1 c2 c3 z
 | g == True = gradientColor x y p w h r h1 g c1 c2 c3 z
 | head c1 == '1' && head c2 == '1' && head c3 == '1' = fromList r (oranges ++ greys ++ whites)
 | head c1 == '1' && head c2 == '1' && head c3 == '2' = fromList r (oranges ++ greys ++ yellows)
 | head c1 == '1' && head c2 == '1' && head c3 == '3' = fromList r (oranges ++ greys ++ reds)
 | head c1 == '1' && head c2 == '2' && head c3 == '1' = fromList r (oranges ++ pinks ++ whites)
 | head c1 == '1' && head c2 == '2' && head c3 == '2' = fromList r (oranges ++ pinks ++ yellows)
 | head c1 == '1' && head c2 == '2' && head c3 == '3' = fromList r (oranges ++ pinks ++ reds)
 | head c1 == '1' && head c2 == '3' && head c3 == '1' = fromList r (oranges ++ purples ++ whites)
 | head c1 == '1' && head c2 == '3' && head c3 == '2' = fromList r (oranges ++ purples ++ yellows)
 | head c1 == '1' && head c2 == '3' && head c3 == '3' = fromList r (oranges ++ purples ++ reds)

 | head c1 == '2' && head c2 == '1' && head c3 == '1' = fromList r (browns ++ greys ++ whites)
 | head c1 == '2' && head c2 == '1' && head c3 == '2' = fromList r (browns ++ greys ++ yellows)
 | head c1 == '2' && head c2 == '1' && head c3 == '3' = fromList r (browns ++ greys ++ reds)
 | head c1 == '2' && head c2 == '2' && head c3 == '1' = fromList r (browns ++ pinks ++ whites)
 | head c1 == '2' && head c2 == '2' && head c3 == '2' = fromList r (browns ++ pinks ++ yellows)
 | head c1 == '2' && head c2 == '2' && head c3 == '3' = fromList r (browns ++ pinks ++ reds)
 | head c1 == '2' && head c2 == '3' && head c3 == '1' = fromList r (browns ++ purples ++ whites)
 | head c1 == '2' && head c2 == '3' && head c3 == '2' = fromList r (browns ++ purples ++ yellows)
 | head c1 == '2' && head c2 == '3' && head c3 == '3' = fromList r (browns ++ purples ++ reds)

 | head c1 == '3' && head c2 == '1' && head c3 == '1' = fromList r (blues ++ greys ++ whites)
 | head c1 == '3' && head c2 == '1' && head c3 == '2' = fromList r (blues ++ greys ++ yellows)
 | head c1 == '3' && head c2 == '1' && head c3 == '3' = fromList r (blues ++ greys ++ reds)
 | head c1 == '3' && head c2 == '2' && head c3 == '1' = fromList r (blues ++ pinks ++ whites)
 | head c1 == '3' && head c2 == '2' && head c3 == '2' = fromList r (blues ++ pinks ++ yellows)
 | head c1 == '3' && head c2 == '2' && head c3 == '3' = fromList r (blues ++ pinks ++ reds)
 | head c1 == '3' && head c2 == '3' && head c3 == '1' = fromList r (blues ++ purples ++ whites)
 | head c1 == '3' && head c2 == '3' && head c3 == '2' = fromList r (blues ++ purples ++ yellows)
 | head c1 == '3' && head c2 == '3' && head c3 == '3' = fromList r (blues ++ purples ++ reds)

 | head c1 == '4' && head c2 == '1' && head c3 == '1' = fromList r (greens ++ greys ++ whites)
 | head c1 == '4' && head c2 == '1' && head c3 == '2' = fromList r (greens ++ greys ++ yellows)
 | head c1 == '4' && head c2 == '1' && head c3 == '3' = fromList r (greens ++ greys ++ reds)
 | head c1 == '4' && head c2 == '2' && head c3 == '1' = fromList r (greens ++ pinks ++ whites)
 | head c1 == '4' && head c2 == '2' && head c3 == '2' = fromList r (greens ++ pinks ++ yellows)
 | head c1 == '4' && head c2 == '2' && head c3 == '3' = fromList r (greens ++ pinks ++ reds)
 | head c1 == '4' && head c2 == '3' && head c3 == '1' = fromList r (greens ++ purples ++ whites)
 | head c1 == '4' && head c2 == '3' && head c3 == '2' = fromList r (greens ++ purples ++ yellows)
 | head c1 == '4' && head c2 == '3' && head c3 == '3' = fromList r (greens ++ purples ++ reds) 
 | otherwise = fromList r (merge reds (merge oranges (merge blues greens)))
 where
   reds = ["indianred","lightcoral","salmon","crimson","red","darkred"]
   pinks = ["pink","lightpink","hotpink","deeppink","mediumvioletred","palevioletred"]
   oranges = ["lightsalmon","coral","tomato","orangered","darkorange","orange"]
   yellows = ["gold","yellow","moccasin","peachpuff","palegoldenrod","khaki"]
   purples = ["plum","violet","fuchsia","mediumorchid","blueviolet","purple"]
   greens = ["chatreuse","limegreen","mediumseagreen","forestgreen","darkgreen","darkcyan"]
   blues = ["aqua","steelblue","deepskyblue","blue","navy","turquoise"]
   browns = ["burlywood","sandybrown","darkgoldenrod","saddlebrown","brown","goldenrod"]
   whites = ["white","honeydew","aliceblue","seashell","beige","mistyrose"]
   greys = ["gainsboro","silver","gray","lightslategray","darkslategrey", "gray"]
   
--
-- Select the random fill colour for the region, if 'Gradient' was selected. 
-- Colour generated depends on properties chosen by user, but is done as a gradient.
--    
-- gradientColor :: Int -> Int -> Int -> Int -> Int -> Double -> Double -> Bool -> [Char] -> [Char] -> [Char] -> String
gradientColor x y p w h h1 r g c1 c2 c3 z
 -- | h1 > 17 = gradientColor x y p w h h1 (r-17) g c1 c2 c3
 | head c1 == '1' && head c2 == '1' && head c3 == '1' = (merge (merge oranges greys) whites) !! round r
 | head c1 == '1' && head c2 == '1' && head c3 == '2' = (merge (merge oranges greys) yellows) !! round r --fromList r (oranges ++ greys ++ yellows)
 | head c1 == '1' && head c2 == '1' && head c3 == '3' = (merge (merge oranges greys) reds) !! round r --fromList r (oranges ++ greys ++ reds)
 | head c1 == '1' && head c2 == '2' && head c3 == '1' = (merge (merge oranges pinks) whites) !! round r --fromList r (oranges ++ pinks ++ whites)
 | head c1 == '1' && head c2 == '2' && head c3 == '2' = (merge (merge oranges pinks) yellows) !! round r --fromList r (oranges ++ pinks ++ yellows)
 | head c1 == '1' && head c2 == '2' && head c3 == '3' = (merge (merge oranges pinks) reds) !! round r --fromList r (oranges ++ pinks ++ reds)
 | head c1 == '1' && head c2 == '3' && head c3 == '1' = (merge (merge oranges purples) whites) !! round r --fromList r (oranges ++ purples ++ whites)
 | head c1 == '1' && head c2 == '3' && head c3 == '2' = (merge (merge oranges purples) yellows) !! round r --fromList r (oranges ++ purples ++ yellows)
 | head c1 == '1' && head c2 == '3' && head c3 == '3' = (merge (merge oranges purples) reds) !! round r --fromList r (oranges ++ purples ++ reds)

 | head c1 == '2' && head c2 == '1' && head c3 == '1' = (merge (merge browns greys) whites) !! round r -- fromList r (browns ++ greys ++ whites)
 | head c1 == '2' && head c2 == '1' && head c3 == '2' = (merge (merge browns greys) yellows) !! round r --fromList r (browns ++ greys ++ yellows)
 | head c1 == '2' && head c2 == '1' && head c3 == '3' = (merge (merge browns greys) reds) !! round r --fromList r (browns ++ greys ++ reds)
 | head c1 == '2' && head c2 == '2' && head c3 == '1' = (merge (merge browns pinks) whites) !! round r --fromList r (browns ++ pinks ++ whites)
 | head c1 == '2' && head c2 == '2' && head c3 == '2' = (merge (merge browns pinks) yellows) !! round r --fromList r (browns ++ pinks ++ yellows)
 | head c1 == '2' && head c2 == '2' && head c3 == '3' = (merge (merge browns pinks) reds) !! round r --fromList r (browns ++ pinks ++ reds)
 | head c1 == '2' && head c2 == '3' && head c3 == '1' = (merge (merge browns purples) whites) !! round r --fromList r (browns ++ purples ++ whites)
 | head c1 == '2' && head c2 == '3' && head c3 == '2' = (merge (merge browns purples) yellows) !! round r --fromList r (browns ++ purples ++ yellows)
 | head c1 == '2' && head c2 == '3' && head c3 == '3' = (merge (merge browns purples) reds) !! round r --fromList r (browns ++ purples ++ reds)

 | head c1 == '3' && head c2 == '1' && head c3 == '1' = (merge (merge blues greys) whites) !! round r -- fromList r (blues ++ greys ++ whites)
 | head c1 == '3' && head c2 == '1' && head c3 == '2' = (merge (merge blues greys) yellows) !! round r --fromList r (blues ++ greys ++ yellows)
 | head c1 == '3' && head c2 == '1' && head c3 == '3' = (merge (merge blues greys) reds) !! round r --fromList r (blues ++ greys ++ reds)
 | head c1 == '3' && head c2 == '2' && head c3 == '1' = (merge (merge blues pinks) whites) !! round r --fromList r (blues ++ pinks ++ whites)
 | head c1 == '3' && head c2 == '2' && head c3 == '2' = (merge (merge blues pinks) yellows) !! round r --fromList r (blues ++ pinks ++ yellows)
 | head c1 == '3' && head c2 == '2' && head c3 == '3' = (merge (merge blues pinks) reds) !! round r --fromList r (blues ++ pinks ++ reds)
 | head c1 == '3' && head c2 == '3' && head c3 == '1' = (merge (merge blues purples) whites) !! round r --fromList r (blues ++ purples ++ whites)
 | head c1 == '3' && head c2 == '3' && head c3 == '2' = (merge (merge blues purples) yellows) !! round r --fromList r (blues ++ purples ++ yellows)
 | head c1 == '3' && head c2 == '3' && head c3 == '3' = (merge (merge blues purples) reds) !! round r -- fromList r (blues ++ purples ++ reds)

 | head c1 == '4' && head c2 == '1' && head c3 == '1' = (merge (merge greens greys) whites) !! round r -- fromList r (greens ++ greys ++ whites)
 | head c1 == '4' && head c2 == '1' && head c3 == '2' = (merge (merge greens greys) yellows) !! round r -- fromList r (greens ++ greys ++ yellows)
 | head c1 == '4' && head c2 == '1' && head c3 == '3' = (merge (merge greens greys) reds) !! round r -- fromList r (greens ++ greys ++ reds)
 | head c1 == '4' && head c2 == '2' && head c3 == '1' = (merge (merge greens pinks) whites) !! round r -- fromList r (greens ++ pinks ++ whites)
 | head c1 == '4' && head c2 == '2' && head c3 == '2' = (merge (merge greens pinks) yellows) !! round r -- fromList r (greens ++ pinks ++ yellows)
 | head c1 == '4' && head c2 == '2' && head c3 == '3' = (merge (merge greens pinks) reds) !! round r -- fromList r (greens ++ pinks ++ reds)
 | head c1 == '4' && head c2 == '3' && head c3 == '1' = (merge (merge greens purples) whites) !! round r -- fromList r (greens ++ purples ++ whites)
 | head c1 == '4' && head c2 == '3' && head c3 == '2' = (merge (merge greens purples) yellows) !! round r -- fromList r (greens ++ purples ++ yellows)
 | head c1 == '4' && head c2 == '3' && head c3 == '3' = (merge (merge greens purples) reds) !! round r -- fromList r (greens ++ purples ++ reds) 
 | otherwise = fromList r (merge reds (merge oranges (merge blues greens)))
 where
   reds = ["indianred","lightcoral","salmon","crimson","red","darkred"]
   pinks = ["pink","lightpink","hotpink","deeppink","mediumvioletred","palevioletred"]
   oranges = ["lightsalmon","coral","tomato","orangered","darkorange","orange"]
   yellows = ["gold","yellow","moccasin","peachpuff","palegoldenrod","khaki"]
   purples = ["plum","violet","fuchsia","mediumorchid","blueviolet","purple"]
   greens = ["chatreuse","limegreen","mediumseagreen","forestgreen","darkgreen","darkcyan"]
   blues = ["aqua","steelblue","deepskyblue","blue","navy","turquoise"]
   browns = ["burlywood","sandybrown","darkgoldenrod","saddlebrown","brown","goldenrod"]
   whites = ["white","honeydew","aliceblue","seashell","beige","mistyrose"]
   greys = ["gainsboro","silver","gray","lightslategray","darkslategrey", "gray"]

--
-- Merges colour lists. 
-- ex. Instead of reds ++ blues ++ greens, we'll get a more mixed list
--
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs   
   
--
--  Select an item from a list based on a value between 0.0 and 1.0.  At 0.0
--  the first item is returned.  At 1.0 the last item is returned.  Values
--  in between do a linear interpolation to identify the position returned.
--
fromList :: Double -> [String] -> String
fromList 1.0 vals = vals !! (length vals - 1)
fromList r vals = vals !! (floor (r * fromIntegral (length vals)))

--
-- Higher order sanitation function
--   input: The IO string we need to sanitize
--   (h:t): String of boolean functions which input must maintain
-- Prevents happy little accidents
-- 
sanitation :: [Char] -> [(Char -> Bool)] -> IO () 
sanitation input [] = putStrLn ("\n")
sanitation input (h:t)  = do
 if (input == []) then do {
  ; putStrLn("\nInvalid entry. Please try again!\n")
  -- what are these next two lines doing?
  ; newInput <- getLine
  ; sanitationHelper newInput (h:t) 
  }  
  else if (not (h $ head input)) then do {
   ; putStrLn("\nInvalid entry. Please try again!")
   ; newInput <- getLine
   ; sanitationHelper newInput (h:t)
   } 
   else do {
    ; sanitationHelper input t
  }

-- 
-- Checks that no more than 1 number is entered at a time
--   input: The IO string we're checking
--   (h:t): String of boolean functions which input must maintain
--
sanitationHelper :: String -> [Char -> Bool] -> IO ()
sanitationHelper input [] = putStrLn ("\n")
sanitationHelper input (h:t) = do
 if ((length input) > 1) then do {
  ; putStrLn("\nInvalid entry. Please try again!\n")
  ; newInput <- getLine
  ; sanitationHelper newInput (h:t)
 }
 else sanitation input (h:t)

-- 
-- Checks that characters are not numbers
--   input: The IO string we're checking
--   (h:t): String of boolean functions which input must maintain
--

alphaCheck :: String -> [Char -> Bool] -> IO ()
alphaCheck input [] = putStrLn ("\n")
alphaCheck input (h:t) = do
 if (input == []) then do {
  ; putStrLn("\nInvalid entry. Please try again!\n")
  ; newInput <- getLine
  ; alphaCheck newInput (h:t) 
  }  
  else if (not (h $ head input)) then do {
   ; putStrLn("\nInvalid entry. Please try again!")
   ; newInput <- getLine
   ; alphaCheck newInput (h:t)
   } 
   else do {
    ; alphaCheck input t
   }

   
--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do

  -- seed = 0 --> Output image will be the same every time.
  -- seed = random --> Program generates different sequence of random numbers each time it is run.
  --   ie. The output image will be completely random every time
  -- NOTE: keeping seed = 0 for testing purposes

  -- let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed
  let gradientValues = take 20000 gradientList

  -- Gets the artists name, to be displayed at top of HTML file
  -- we would need to get keywords from the user here
  putStrLn("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nWhat is your name?\n")
  theArtist <- getLine
  alphaCheck theArtist [isAlpha]
  -- sanitation theArtist [isAlpha]

  -- First colour palette
  -- 1 = oranges
  -- 2 = browns
  -- 3 = blues
  -- 4 = greens
  putStrLn("Please type a number from 1 to 4:\nWhat is your favourite season? \n\n1. Summer\n2. Autumn\n3. Winter\n4. Spring")
  clr1 <- getLine
  sanitationHelper clr1 [isDigit,  (`elem` ['1','2','3','4'])]
  -- sanitation clr1 [isDigit,  (`elem` ['1','2','3','4'])]

  -- Second colour palette
  -- 1 = greys
  -- 2 = pinks
  -- 3 = purples
  putStrLn("\nPlease type a number from 1 to 3:\nWhat makes your heart smile?\n1. Hobbies and Passions\n2. Friends and Family\n3. Culture and Experiences")
  clr2 <- getLine
  sanitationHelper clr2 [isDigit, (`elem` ['1','2','3'])]

  -- Final colour palette
  -- 1 = whites
  -- 2 = yellows
  -- 3 = reds
  putStrLn("\nPlease type a number from 1 to 3:\nHow are you feeling today?\n1. Pretty mediocre\n2. Happy!\n3. Under the weather")
  clr3 <- getLine
  sanitationHelper clr3 [isDigit, (`elem` ['1','2','3'])]
  
  -- Decides on image type
  -- 1 = gradient (fewer colours, more consistency with colour spread)
  -- 2 = random (more colours, completely random colour spread)
  putStrLn("\nPlease type a number from 1 to 2:\nWould you prefer a gradient or random colours?\n1. Gradient\n2. Random")
  answer <- getLine
  sanitationHelper answer [isDigit, (`elem` ['1','2'])]

  -- Decides on shape type, ovals or rectangles
  putStrLn("\nPlease type a number from 1 to 2:\nWould you prefer ovals or rectangles?\n1. Ovals\n2. Rectangles")
  shape <- getLine
  sanitationHelper answer [isDigit, (`elem` ['1','2'])]

  -- Names the image & the file
  putStrLn("\nWhat would you like to name your masterpiece?")
  masterpiece <- getLine
  alphaCheck masterpiece [isAlpha]

  putStrLn("Bob Ross is doing his work...")

  -- Build the actual image based on answers given above
  -- First case is for gradients, second is for random mondrians

  if (head answer == '1') 
  then do
    let prefix = "<html><head><title>" ++
               theArtist ++
               "'s Masterpiece"++
               "</title></head><body><p>" ++ 
               theArtist ++
               "'s Masterpiece"++
               "</p>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
    let image = snd (mondrian 0 0 0 width height randomValues gradientValues True clr1 clr2 clr3 (head shape))
    let suffix = "</svg>\n</html>"

    putStrLn("Thank you! Your painting in progress...")
    writeFile(masterpiece ++ "_gradient.html") (prefix ++ image ++ suffix)
    putStrLn(masterpiece ++ " is complete!")
  else do
    let prefix = "<html><head><title>" ++
               theArtist ++
               "'s Masterpiece"++
               "</title></head><body><p>" ++ 
               theArtist ++
               "'s Masterpiece"++
               "</p>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
    let image = snd (mondrian 0 0 0 width height randomValues randomValues False clr1 clr2 clr3 (head shape))
    let suffix = "</svg>\n</html>"
    putStrLn("Thank you! Your Painting in progress...")
    writeFile(masterpiece ++ "_random.html") (prefix ++ image ++ suffix)
    putStrLn(masterpiece ++ " is complete!")
