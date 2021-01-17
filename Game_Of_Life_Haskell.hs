-- Name: Trym Ettestøl Osland
-- 10 (s 0 0, b 2 2) 1 1 2 2 2 3 1 4
-- The comment above is the answer to task 2.4, test it with testcustomglider
-- If you are not familiar with the game of life, check this out:
-- https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules

import Data.Char

customglider :: String
customglider = "10 (s 0 0, b 2 2) 1 1 2 2 2 3 1 4"

testcustomglider :: IO ()
testcustomglider = life (initialize1 (tokenize (customglider)))

conwayglider :: String
conwayglider = "10 (s 2 3, b 3 3) 4 2 2 3 4 3 3 4 4 4"

testconwayglider :: IO ()
testconwayglider = life (initialize1 (tokenize (conwayglider)))

{-
When main is run it takes an input for a file, that game of life is then run using the life function. 

I have taken basis in the code for game of life in the course's book
(Graham Hutton - Programming in Haskell second edition, page 133).
I had to change most of the code to fit the assignment. 

I was not able to finish the code for the visualizer, you can see what I was working on at the bottom.
In place is a very simple visualizer (showcells), used in the book. 
Everything else should be answered to some extent

The type Game is used allot;
Game is a tuple with Pos and [Int], Pos is a list of positions of the alive cells, while [Int] is a list for the settings of the game. 
For a overview of [Int]'s setting propoerties, see the comment above initializeSettings

For reading files i use a tokenizer and a self-defined parser (starting with initialize1). 

The function life is where the game itself is run.
This is where the user can interact with the game via inputs. 
-}

type Pos = (Int, Int)

type Board = [Pos]

type Game = (Board, [Int])

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: (Pos, [Int]) -> [Pos]
neighbs ((x,y), xs) = [(x1, y1) | x1 <- [x + 1, x, x - 1], x1 <= (xs !! 1), x1 > 0, 
                            y1 <- [y + 1, y, y - 1], y1 <= (xs !! 0), y > 0,
                            not (x1 == x && y1 == y)]

liveneighbs :: Board -> (Pos, [Int]) -> Int
liveneighbs b = length.filter(isAlive b).neighbs

survivors :: Game -> [Pos]
survivors g = [p | p <- (fst g), elem (liveneighbs (fst g) (p, snd g)) [((snd g) !! 2)..((snd g) !! 3)]]

births :: Game -> [Pos]
births g = [(x,y) | x <- [1..((snd g) !! 0)], 
                    y <- [1..((snd g) !! 1)],
                    isEmpty (fst g) (x,y),
                    liveneighbs (fst g) ((x,y), snd g) >= (snd g) !! 4,
                    liveneighbs (fst g) ((x,y), snd g) <= (snd g) !! 5]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Game -> Game
nextgen g 
  | (fst g) == (survivors g ++ births g) = (survivors g ++ births g, insertAtn 7 1 (snd g))
  | otherwise = (survivors g ++ births g, snd g)

addCells :: Game -> [String] -> Game
addCells g [] = g
addCells g (x:s:xs) =
  let x1 = read s :: Int
      y1 = read x :: Int 
      width = (snd g) !! 0
      height = (snd g) !! 1
  in
  if (x1 < 1 || y1 < 1 || x1 > height || y1 > width) then error "index utenfor kartet"
  else addCells ((fst g) ++ [(x1, y1)], (snd g)) xs

removeCells :: Game -> [String] -> Game
removeCells g [] = g
removeCells g (x:s:xs)= 
  let x1 = read s :: Int
      y1 = read x :: Int
      a = filter (/= (x1, y1)) (fst g)
  in
  removeCells (a, snd g) xs

life :: Game -> IO ()
life g = do 
    putStrLn ""
    if ((snd g) !! 7 > 0 && (snd g) !! 6 >= 1) then --Stability check on live mode
        do cls
           showcells (fst g)
           putStrLn $ "\n stability has occured with " ++ show ((snd g) !! 6) ++ " generations left"
    else if ((snd g) !! 7 > 0) then --Stability check 
        do cls
           showcells (fst g)
           putStrLn "\n stability has occured"
    else if ((snd g) !! 6 >= 1) then -- Live mode
        do cls
           let g1 = (nextgen g)
           showcells (fst g1)
           wait 500000
           life (fst g1, insertAtn 6 (((snd g1) !! 6) - 1) (snd g1))
    else putStr ""
    putStrLn "\n give input: "
    (xs) <- getLine
    if (xs) == "" then 
        do cls
           showcells (fst (nextgen g))
           life (nextgen g)
    else if (head xs) == 'c' then
        do cls
           let a = read ((tokenize xs) !! 1)
               g1 = ([], insertAtn 1 a (insertAtn 0 a (snd g))) :: Game 
               --Empty board, same previous settings besides size
           showcells (fst g1)
           life g1
    else if (head xs) == 'n' then
      do cls
         let g1 = (addCells g (tokenize (tail xs)))
         showcells (fst g1)
         life g1
    else if (head xs) == 'e' then
      do cls 
         let g1 = (removeCells g (tokenize (tail xs)))
         showcells (fst g1)
         life g1
    else if (head xs) == 'b' then
      do cls
         let a = tokenize (tail xs)
             m = read (a !! 0) :: Int
             n = read (a !! 1) :: Int
             g1 = (fst g, insertAtn 4 m (insertAtn 5 n (snd g)))
         showcells (fst g1)
         life g1
    else if (head xs) == 's' then
      do cls
         let a = tokenize (tail xs)
             m = read (a !! 0) :: Int
             n = read (a !! 1) :: Int
             g1 = (fst g, insertAtn 2 m (insertAtn 3 n (snd g)))
         showcells (fst g1)
         life g1
    else if (head xs) == '?' then
      do cls 
         showcells (fst g)
         let m1 = show ((snd g) !! 2)
             n1 = show ((snd g) !! 3)
             m2 = show ((snd g) !! 4)
             n2 = show ((snd g) !! 5)
         putStrLn $ "s: (" ++ m1 ++ ", " ++ n1 ++ "). b: (" ++ m2 ++ ", " ++ n2 ++ ")."
         life g
    else if (head xs) == 'w' then
      do cls
         showcells (fst g)
         putStrLn $ "Current (x,y)-positions for the alive cells are: " ++ show (fst g)
         life g
    else if (head xs) == 'r' then --Changes the game to the specified file and prints the new rules 
      do cls 
         a <- readFile ((tokenize xs) !! 1)
         let g1 = initialize1 (tokenize a) :: Game
             m1 = show ((snd g1) !! 2)
             n1 = show ((snd g1) !! 3)
             m2 = show ((snd g1) !! 4)
             n2 = show ((snd g1) !! 5)
             posg1 = show (fst g1)
         showcells (fst g1)
         putStrLn $ "The rules for the file '" ++ ((tokenize xs) !! 1) ++ "' are; s: (" ++ m1 ++ ", " ++ n1 ++ "). b: (" ++ m2 ++ ", " ++ n2 ++ ")."
         putStrLn $ "And the (x,y)-position of the starting cells are: " ++ posg1
         life g1
    else if (head xs) == 'l' then
      do cls
         showcells (fst g)
         let x = read (head (tokenize (tail xs))) :: Int
             g1 = (fst g, insertAtn 6 x (snd g))
         life g1
    else if (xs) == "quit" then 
        do cls
           error "Game of life has been quit"
           {-
           There used to be an empty return () here instead of the error.
           That solution produced a bug when live mode had been previously used.
           The bug would make it such that the return () method would not exit life;
           hence the error usage.
           -}
    else do cls 
            showcells (fst g)
            putStrLn "Invalid input"
            life (g)

{-
The type Game is a tuple with positions and settings; 
The settings are represented by a list of Int.
Place n in the list represents:
0 - width
1 - height
2 - m1 (a cell surives if at has at least m1 neighbours...)
3 - n1 (...and at most n1 neighbours)
4 - m2 (a cell is born if it has at least m2 neighbours...)
5 - n2 (...and at most n2 neighbours)
6 - x (if x is >= 1 then the game is in live mode)
7 - stable (if stable is > 0 then the game is in stable mode)
-}
initializeSettings :: [Int]
initializeSettings = [-1,-1,-1,-1,-1,-1,-1,-1]

--This parser is used to read a tokenized file to construct a Game
initialize1 :: [String] -> Game
initialize1 (x:xs) = 
    let g = ([], initializeSettings) 
        n = read x :: Int 
        in
    if (n < 1 || n > 99) then error "error on input file, initialize1"
    else 
    initialize2 (xs, (fst g, insertAtn 0 n (insertAtn 1 n (snd g))))

initialize2 :: ([String], Game) -> Game
initialize2 ((x:xs), g) 
  | x == "(" = initialize3 (xs, g)
  | otherwise = error "error on input file, initialize2"

initialize3 :: ([String], Game) -> Game
initialize3 ((x:s:t:xs), g)
  | x == "s" = initialize4 (xs, (fst g, insertAtn 2 (read s :: Int) (insertAtn 3 (read t :: Int) (snd g))))
  | x == "b" = initialize4 (xs, (fst g, insertAtn 4 (read s :: Int) (insertAtn 5 (read t :: Int) (snd g))))
  | otherwise = error "error on input file, initialize3"

initialize4 :: ([String], Game) -> Game
initialize4 ((y:x:s:t:xs), g)
  |y == "," && x == "s" = initialize5 (xs, (fst g, insertAtn 2 (read s :: Int) (insertAtn 3 (read t :: Int) (snd g))))
  |y == "," && x == "b" = initialize5 (xs, (fst g, insertAtn 4 (read s :: Int) (insertAtn 5 (read t :: Int) (snd g))))
  | otherwise = error "error on input file, initialize4"

initialize5 :: ([String], Game) -> Game
initialize5 ((x:xs), g)
  |x == ")" = initialize6 (xs, g)
  | otherwise = error "error on input file, initialize5"

--This is the only recursive parser, as it reads the rest of the line until it has all initial alive cells
initialize6 :: ([String], Game) -> Game
initialize6 ([], g) = g
initialize6 ((x:s:xs), g) = initialize6 (xs, (((fst g) ++ [(read s :: Int, read x :: Int)]), snd g))

main = do
    putStrLn "filen?: "
    filNavn <- getLine
    x <- readFile filNavn
    let y = initialize1 (tokenize x) in life y 
    return ()

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

tokenize :: [Char] -> [String]
tokenize [] = []
tokenize (' ' : s) = tokenize s
tokenize (',' : s) = "," : tokenize s
tokenize ('(' : s) = "(" : tokenize s
tokenize (')' : s) = ")" : tokenize s
tokenize (c : s)
  | isDigit c =
    let (cs, s') = collectWhile isDigit s
     in (c : cs) : tokenize s'
  | isAlpha c =
    let (cs, s') = collectWhile isAlpha s
     in (c : cs) : tokenize s'
  | otherwise = error ("unexpected character " ++ show c)

collectWhile :: (Char -> Bool) -> String -> (String, String)
collectWhile p s = (takeWhile p s, dropWhile p s)

insertAtn :: Int -> a -> [a] -> [a]
insertAtn n a xs
  | n < length xs = let (ys, zs) = splitAt n xs in ys ++ [a] ++ (tail zs)
  |otherwise = error "Cannot insert an element at index larger than the length of the list"