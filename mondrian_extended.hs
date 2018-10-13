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
split_low = 50 :: Int
split_penalty = 0.2 :: Float

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
-- we need to add our keyword into mondrian
  | w > width `div` 2 && 
    h > height `div` 2 = b_split x (y) w h (r:s:rest)
  | w > width `div` 2  = h_split x y w h (r:s:rest)
  | h > height `div` 2 = v_split x y (w) h (r:s:rest)
  | hs && vs           = b_split x y (w) h rest
  | hs                 = h_split x y (w) h rest
  | vs                 = v_split x y (w) h rest
  | otherwise = (s:rest, "<rect x=" ++ (show x) ++ 
                         " y=" ++ (show y) ++ 
                         " width=" ++ (show w) ++ 
                         " height=" ++ (show h) ++ 
                         " stroke=\"black\" stroke-width=\"3\" fill=\"" ++ 
                         (randomColor x y w h r s) ++ 
						 -- we would need random colour to take our key word here
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
--  Select the random fill color for the region.  The first random number
--  determines the general colors class (Yellow, Blue, Green, Red or White)
--  and the second selects the specific color within the class
--
randomColor :: Int -> Int -> Int -> Int -> Float -> Float -> String
randomColor x y w h r1 r2
 -- we need random colour to take, as parameters, whatever the user types in
 -- then, based on what the user inputs, we will choose random colours from that list (maybe 2 lists? 3?)

-- WHICH MEANS we also need mondrian to take in our key word

  | r1 * ltotal <= lul^8 = fromList r2 yellows
  | r1 * ltotal <= lul^8 + lur^8 = fromList r2 blues
  | r1 * ltotal <= lul^8 + lur^8 + lll^8 = fromList r2 greens
  | r1 * ltotal <= lul^8 + lur^8 + lll^8 + llr^8 = fromList r2 reds
  | otherwise = fromList r2 whites
  where
    yellows = ["gold", "goldenrod", "orange", "sandybrown", "yellow"]
    blues = ["royalblue", "skyblue", "mediumslateblue", "powderblue", "cornflowerblue"] 
    reds = ["salmon", "tomato", "coral", "indianred", "pink"]
    greens = ["palegreen", "limegreen", "greenyellow", "yellowgreen", "chartreuse"]
    whites = ["beige", "ivory", "cornsilk", "floralwhite", "whitesmoke"]
    lul = sqrt (xc ^ 2 + yc ^ 2)
    lur = sqrt ((fromIntegral width - xc) ^ 2 + yc ^ 2)
    lll = sqrt (xc ^ 2 + (fromIntegral height - yc) ^ 2)
    llr = sqrt ((fromIntegral width - xc) ^ 2 + (fromIntegral height - yc) ^ 2)
    xc = fromIntegral x + (fromIntegral w) / 2
    yc = fromIntegral y + (fromIntegral h) / 2
    -- 
    --  At 1.0, there are no white regions.  Increasing the 1.0 to a larger
    --  value will cause white regions to be included in the output.
    --
    ltotal = 1.0 * (lul^8 + lur^8 + lll^8 + llr^8)

--
--  Select an item from a list based on a value between 0.0 and 1.0.  At 0.0
--  the first item is returned.  At 1.0 the last item is returned.  Values
--  in between do a linear interpolation to identify the position returned.
--
fromList :: Float -> [String] -> String
fromList 1.0 vals = vals !! (length vals - 1)
fromList r vals = vals !! (floor (r * fromIntegral (length vals)))







{-
Reds = ["indianred","lightcoral","salmon","darksalmon","lightsalmon","crimson","red","firebrick","darkred"]
Pinks = ["pink","lightpink","hotpink","deeppink","mediumvioletred","palevioletred"]
Oranges = ["lightsalmon","coral","tomato","orangered","darkorange","orange"]
Yellows = ["gold","yellow","lightyellow","lemonchiffon","lightgoldenrodyellow","papayawhip","moccasin","peachpuff","palegoldenrod","khaki","darkkhaki"]
Purples = ["lavender","thistle","plum","violet","orchid","fuchsia","magenta","mediumorchid","mediumpurple","rebeccapurple","blueviolet","darkviolet",
"darkorchid","darkmagenta","purple","indigo","slateblue","darkslateblue","mediumslateblue"]
greens = ["greenyellow","chatreuse","lawngreen","lime","limegreen","palegreen","lightgreen","mediumspringgreen","springgreen","mediumseagreen",
"seagreen","forestgreen","green","darkgreen","olivedrab","olive","darkolivegreen","mediumaquamarine","darkseagreen","lightseagreen","darkcyan","teal"]
blues = ["aqua","cyan","lightcyan","paleturquoise","aquamarine","turquoise","mediumturquoise","cadetblue","steelblue","lightsteelblue","powderblue",
"lightblue","skyblue","lightskyblue","deepskyblue","dodgerblue","cornflowerblue","mediumslateblue","royalblue","blue","mediumblue","darkblue","navy",
"midnightblue"]
browns = ["cornsilk","blachedalmond","bisque","navajowhite","wheat","burlywood","tan","rosybrown","sandybrown","goldenrod","darkgoldenrod","peru",
"chocolate","saddlebrown","sienna","brown","maroon"]
whites = ["white","snow","honeydew","mintcream","azure","aliceblue","ghostwhite","whitesmoke","seashell","beige","oldlace","floralwhite","ivory",
"antiquewhite","linen","lavenderblush","mistyrose"]
greys = ["gainsboro","lightgray","silver","darkgray","gray","dimgray","lightslategray","slategray","darkslategrey","black"]

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

  -- we would need to get keywords from the user here
  putStrLn("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nPlease type a number from 1 to 4:\nwhat is your favourite season? \n\n1. Summer\n2. Autumn\n3. Winter\n4. Spring")
  clr1 <- getLine
  putStrLn("\nPlease type a number from 1 to 3:\nWhat makes you think of home?\n1. Lonely\n2. Love\n3. Family")
  clr2 <- getLine
  putStrLn("\nPlease type a number from 1 to 3:\nWhat helps you wind down?\n1. Petting a cat\n2. A sunny day\n3. Physical activity")
  clr3 <- getLine
  putStrLn("\nWhat would you like to name your masterpiece?")
  masterpiece <- getLine
  putStrLn("Bob Ross is doing his work...")
  
  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
  putStrLn(masterpiece ++ " is complete!")
