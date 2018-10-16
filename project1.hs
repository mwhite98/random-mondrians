--
-- Original Template by Dr. Ben Stephenson, from:
-- http://nifty.stanford.edu/2018/stephenson-mondrian-art/
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM, when, unless)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)
import Data.Char

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
-- changing the split penalty allows us to change the size of our regions (smaller penalty = smaller regions
-- split_penalty -> 1.5 = big rounded squares
-- split_penalty -> 0.75 = many tiny circles
-- changing split low allows us to control the minimum split size

split_low = 100 :: Int
split_penalty = 0.75 :: Double

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 

--randomList :: Int -> [Double]
--randomList seed = take 999 [0.001, 0.002..1]


randomList :: Int -> [Double]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

gradientList :: [Double]
gradientList = take 20000 ([0..17] ++ gradientList)

rl_helper :: StdGen -> [Double]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Double) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Double -> Int
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
--   c1, c2, c3: colour hues chosen by user
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--

-- mondrian :: Int -> Int -> Int -> Int -> Int -> [Float] -> [Float] -> Bool -> [Char] -> [Char] -> [Char] -> ([Float], String)
mondrian _ _ _ 0 _ rvals gvals _ _ _ _ = (rvals, "")
mondrian _ _ _ _ 0 rvals gvals _ _ _ _ = (rvals, "")
mondrian x y p w h (r:s:rest) (h1:h2:h3) g c1 c2 c3
  -- hs = horizontal split
  -- vs = vertical split
  | hs && vs           = b_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3
  | hs                 = h_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3
  | vs                 = v_split x y p (w) h rest (h1:h2:h3) g c1 c2 c3
  | p <= 0             = c_curl x y p (w) h rest (h1:h2:h3) g c1 c2 c3
  | otherwise = (s:rest, "<rect x=" ++ (show x) ++ 
                         " y=" ++ (show y) ++
                         " rx=" ++ (show p) ++ " ry=" ++ (show p) ++
                         " width=" ++ (show w) ++ 
                         " height=" ++ (show h) ++
                         " stroke=\"white\" stroke-width=\"0.5\" fill=\"" ++ 
                         (randomColor x y p w h r h2 g c1 c2 c3) ++
                         "\"" ++ " opacity=\"1.0\"" ++ "/>\n") 
  where 
    rand1 = randomInt split_low (round (fromIntegral w * split_penalty)) r
    hs = if rand1 < w then True else False
    rand2 = randomInt split_low (round (fromIntegral h * split_penalty)) s
    vs = if rand2 < h then True else False

  --rand2 is same as rand1 but for height (vertical)
  -- rand1 low high x = ((high - low) * x) + low
  -- rand1 = ((width of region - splitlow) * r) + splitlow
  -- hs: if rand1 < width of region, then split
	
	
	
	
--
--  Split the region both horizontally and vertically
--
--b_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
-- 37 and 67
b_split x y p w h (r:s:rest) (h1:h2:h3) g c1 c2 c3 = (rest4, s1 ++ s2 ++ s3 ++ s4)
  where 
    h_rand = randomInt 37 67 r
    v_rand = randomInt 37 67 s
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y p lw th rest h3 g c1 c2 c3
    (rest2, s2) = mondrian (x + lw) y p rw th rest1 h3 g c1 c2 c3
    (rest3, s3) = mondrian x (y + th) p lw bh rest2 h3 g c1 c2 c3
    (rest4, s4) = mondrian (x + lw) (y + th) p rw bh rest3 h3 g c1 c2 c3

--
--  Split the region horizontally so that we get two that are side by side
--
--h_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
h_split x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 = (rest2, s1 ++ s2)
  where 
    h_rand = randomInt 37 67 r
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    (rest1, s1) = mondrian x y p lw h rest h3 g c1 c2 c3
    (rest2, s2) = mondrian (x + lw) y p rw h rest1 h3 g c1 c2 c3

--
--  Split the region vertically so that we get one on top the other
--
--v_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
v_split x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 = (rest2, s1 ++ s2)
  where 
    v_rand = randomInt 37 67 r
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y p w th rest h3 g c1 c2 c3
    (rest2, s2) = mondrian x (y + th) p w bh rest1 h3 g c1 c2 c3

c_curl x y p w h (r:rest) (h1:h2:h3) g c1 c2 c3 = (rest1, s1)
  where 
    p_rand = randomInt 37 67 r
    (rest1, s1) = mondrian x y (p + p_rand) w h rest h3 g c1 c2 c3


--
--  Select the random fill color for the region.  The first random number
--  determines the general colors class (Yellow, Blue, Green, Red or White)
--  and the second selects the specific color within the class
--
randomColor :: Int -> Int -> Int -> Int -> Int -> Double -> Double -> Bool -> [Char] -> [Char] -> [Char] -> String

randomColor x y p w h r h1 g c1 c2 c3 
 | g == True = gradientColor x y p w h r h1 g c1 c2 c3
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
 | otherwise = fromList r whites
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
   greys = ["gainsboro","silver","gray","lightslategray","darkslategrey","black"]
   
-- WHAT IF WE USED !! INSTEAD OF FROMLIST TO GET    
   
gradientColor :: Int -> Int -> Int -> Int -> Int -> Double -> Double -> Bool -> [Char] -> [Char] -> [Char] -> String
gradientColor x y p w h h1 r g c1 c2 c3 
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
 | otherwise = fromList r whites
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
   greys = ["gainsboro","silver","gray","lightslategray","darkslategrey","black"]


-- function to merge our colourlists. instead of red ++ blue ++ green we'll get a more more mixed list
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

{-

what is your favourite season?
1. Summer -> Orange
2. Autumn -> brown
3. Winter -> blues
4. Spring -> greens

what makes you think of home?
1. lonely -> grey
2. love -> pink
3. family -> purple

what helps you wind down?
1. petting a cat -> white
2. sunny day -> yellow
3. physical activity -> red
-}


sanitation :: [Char] -> [(Char -> Bool)] -> IO () 
sanitation input [] = putStrLn ("\nThank You!\n")
sanitation input (h:t)  = do
 if (input == []) then do {
  ; putStrLn("\nInvalid entry. Please try again!\n")
  ; newInput <- getLine
  ; sanitation newInput (h:t) 
  }  
  else if (not (h $ head input)) then do {
   ; putStrLn("\nInvalid entry. Please try again!")
   ; newInput <- getLine
   ; sanitation newInput (h:t)
   } 
   else do {
    ; sanitation input t
	}

   
   
--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  let seed = 0
  --seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed
  let gradientValues = take 20000 gradientList

  -- we would need to get keywords from the user here
  putStrLn("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nWhat is your name?\n")
  theArtist <- getLine
  sanitation theArtist [isAlpha]
   
  -- putStrLn(show (take 25 gradientValues))
  putStrLn("Please type a number from 1 to 4:\nwhat is your favourite season? \n\n1. Summer\n2. Autumn\n3. Winter\n4. Spring")
  clr1 <- getLine
  sanitation clr1 [isDigit,  (`elem` ['1','2','3','4'])]
  
  putStrLn("\nPlease type a number from 1 to 3:\nWhat makes you think of home?\n1. Lonely\n2. Love\n3. Family")
  clr2 <- getLine
  sanitation clr2 [isDigit, (`elem` ['1','2','3'])]
  
  putStrLn("\nPlease type a number from 1 to 3:\nWhat helps you wind down?\n1. Petting a cat\n2. A sunny day\n3. Physical activity")
  clr3 <- getLine
  sanitation clr3 [isDigit, (`elem` ['1','2','3'])]
  
  putStrLn("\nPlease type a number from 1 to 2:\nWould you prefer a gradient or random colours?\n1. Gradient\n2. Random")
  answer <- getLine
  sanitation answer [isDigit, (`elem` ['1','2'])]


  putStrLn("\nWhat would you like to name your masterpiece?")
  masterpiece <- getLine
  sanitation masterpiece [isAlpha]

  putStrLn("Bob Ross is doing his work...")

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
    let image = snd (mondrian 0 0 0 width height randomValues gradientValues True clr1 clr2 clr3)
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
    let image = snd (mondrian 0 0 0 width height randomValues randomValues False clr1 clr2 clr3)
    let suffix = "</svg>\n</html>"
    putStrLn("Thank you! Your Painting in progress...")
    writeFile(masterpiece ++ "_random.html") (prefix ++ image ++ suffix)
    putStrLn(masterpiece ++ " is complete!")
