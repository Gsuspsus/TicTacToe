import Data.List
import Control.Monad
import Text.Read (readMaybe)

data Square = Empty | Cross | Circle deriving (Eq, Read)
data Player = None | Player {name :: String, value :: Square} deriving (Eq)
type Board = [[Square]]
data GameState = InProgress | Won Player | StaleMate
data Game = Game {board :: Board, players :: [Player], state :: GameState} 

instance Show Square where
  show Empty = "#"
  show Cross = "X"
  show Circle = "O"

instance Show Player where
  show None = ""
  show (Player n v) = n ++ " (" ++ show v ++ ")"

instance Show Game where
  show (Game b (p:_) _) = showBoard b ++ "\n" ++ show p
   
generateBoard :: Int -> Board
generateBoard n = nOf $ nOf Empty
  where nOf = take n . repeat

showBoard :: Board -> String
showBoard board = intercalate "\n" $ map (intercalate " | " . map show) board

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace f y) x

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]

step :: (a -> b, a -> b) -> (a,a) -> (b, b)
step (f,g) (x,y) = (f x, g y) 

stepN :: Int -> (a -> a, a -> a) -> (a,a) -> [(a,a)]
stepN n fs pos = take n $ iterate (step fs) pos

at :: [[a]] -> (Int,Int) -> a
at xs (x,y) = ((xs !! y) !! x)

stepRight :: Int -> [(Int,Int)] 
stepRight l = stepN l ((+1),(+1)) (0,0)

stepLeft :: Int -> [(Int,Int)] 
stepLeft l = stepN l ((subtract 1),(+1)) (l-1,0)

updateGame :: Game -> (Int,Int) -> Game
updateGame (Game b ps@(p:_) s) pos = Game b' (rotate ps) (updateGameState (Game b' ps s))
  where 
    b' = replace2D (const (value p)) pos b

hasWon :: Board -> Player -> Bool
hasWon b p = horizontalWin || verticalWin || diagonalWin
 where 
    horizontalWin = any allPlayerValue b
    verticalWin = allPlayerValue (map head b)
    diagonalWin = allPlayerValue mainDiagonal || allPlayerValue antiDiagonal 
    mainDiagonal = map (b `at`) $ stepRight len
    antiDiagonal = map (b `at`) $ stepLeft len
    len = length b
    allPlayerValue = all (==(value p))

updateGameState :: Game -> GameState
updateGameState (Game b (p:ps) _)
  | all (not . any (==Empty)) b = StaleMate
  | hasWon b p = Won p
  | otherwise = InProgress

readPosition :: IO (Maybe (Int,Int))
readPosition = (readMaybe <$> getLine)

runGame :: Game -> IO ()
runGame game@(Game b (p:_) s) = do
  print game
  pos <- readPosition
  case pos of 
    Nothing -> reset "Invalid pos" 
    Just pos' -> do 
      when (b `at` pos' /= Empty) $ reset "Position occupied"
      let game' = updateGame game pos' 
      case (state game') of
        InProgress -> runGame game'
        Won player -> putStrLn (show p ++ " WON THE GAME!") >> print game'
        StaleMate -> putStrLn "Stalemate"
  where 
    reset prompt = putStrLn prompt >> runGame game
  
main = do 
  let board =  generateBoard 3
  runGame (Game board [Player "gsus" Circle, Player "psus" Cross] InProgress)