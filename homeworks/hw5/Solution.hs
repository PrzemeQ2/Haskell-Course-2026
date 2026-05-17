module Solution where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- 1. Stack machine
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH x) = modify (x :)

execInstr POP = do
  s <- get
  case s of
    []     -> return ()
    (_:xs) -> put xs

execInstr DUP = do
  s <- get
  case s of
    []     -> return ()
    (x:xs) -> put (x:x:xs)

execInstr SWAP = do
  s <- get
  case s of
    []         -> return ()
    [_]        -> return ()
    (x:y:xs)   -> put (y:x:xs)

execInstr ADD = do
  s <- get
  case s of
    []         -> return ()
    [_]        -> return ()
    (x:y:xs)   -> put ((x+y):xs)

execInstr MUL = do
  s <- get
  case s of
    []         -> return ()
    [_]        -> return ()
    (x:y:xs)   -> put ((x*y):xs)

execInstr NEG = do
  s <- get
  case s of
    []     -> return ()
    (x:xs) -> put ((-x):xs)

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

-- 2. Expression evaluator with variable bindings 
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right

eval :: Expr -> State (Map String Int) Int
eval (Num x) = return x

eval (Var str) = do
  s <- get
  return (fromJust (Map.lookup str s))

eval (Add e1 e2) = do 
  ex1 <- eval e1 
  ex2 <- eval e2
  return (ex1 + ex2)

eval (Mul e1 e2) = do 
  ex1 <- eval e1 
  ex2 <- eval e2
  return (ex1 * ex2)

eval (Neg e) = do 
  x <- eval e
  return (-x)

eval (Assign str e) = do
  x <- eval e 
  modify (Map.insert str x)
  return x 

eval (Seq e1 e2) = do 
  ex1 <- eval e1
  ex2 <- eval e2
  return ex2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- 3. Memoised edit (Levenshtein) distance
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get 
  case Map.lookup (i,j) cache of 
    Just value -> return value
    Nothing -> do
      x <- calculateDist
      modify (Map.insert (i,j) x)
      return x
    where
      calculateDist  
        | i == 0 = return j
        | j == 0 = return i
        | (xs !! (i-1)) == (ys !! (j-1)) = editDistM xs ys (i-1) (j-1)
        | otherwise = do 
          del <- editDistM xs ys (i-1) j
          ins <- editDistM xs ys i (j-1)
          sub <- editDistM xs ys (i-1) (j-1)
          return (1+ minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- StateT and "Treasure Hunters" Game Simulation
data Location 
  = Empty
  | Treasure Int 
  | Trap Int
  | Obstacle Int 
  | DecisionPoint [(String, Int)]
  | Goal
  deriving Show

data GameState = GameState {
  playerPosition :: Int, 
  playerEnergy :: Int,
  playerScore :: Int,
  board :: Map Int Location
} deriving Show

type AdventureGame a = StateT GameState IO a

getDiceRoll :: IO Int
getDiceRoll = do 
  putStrLn "Enter a result of a dice roll (1-6)"
  input <- getLine
  case reads input of 
    [(n, "")] | n >= 1 && n <= 6 -> return n 
    _ -> do
      putStrLn "Incorrect number (1-6), try again"
      getDiceRoll

showOption :: (Int, String) -> IO ()
showOption (i, o) = do
  putStrLn (show i ++ ". " ++ o)

printOptions :: [String] -> IO ()
printOptions options = do 
    let pairs = zip [1..] options
    mapM_ showOption pairs  

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do 
  putStrLn "Choose an option"
  printOptions options
  input <- getLine
  case reads input of
    [(n, "")] | n >= 1 && n <= length options  -> return (options !! (n-1))
    _ -> do
      putStrLn "Incorrect option"
      getPlayerChoice options

displayGameState :: GameState -> IO ()
displayGameState gameState = do 
  putStrLn "============"
  putStrLn ("Position:    " ++ show (playerPosition gameState))
  putStrLn ("Energy: " ++ show (playerEnergy gameState))
  putStrLn ("Score:  " ++ show (playerScore gameState))
  putStrLn "============"

movePlayer   :: Int -> AdventureGame Int
movePlayer roll = do 
  modify (\gameState -> gameState {
      playerPosition = playerPosition gameState + roll,
      playerEnergy = playerEnergy gameState - 1
  })
  return roll

makeDecision :: [String] -> AdventureGame String
makeDecision options = liftIO (getPlayerChoice options)

handleLocation :: AdventureGame Bool
handleLocation = do 
  gameState <- get
  let pos = playerPosition gameState
  let loc = Map.findWithDefault Empty pos (board gameState)
  case loc of 
    Empty -> do 
      liftIO (putStrLn "(Empty)") 
      return False
    Treasure n -> do 
      modify(\g -> g {playerScore = playerScore g + n})
      liftIO(putStrLn ("Treasure: +" ++ show n ++ " points"))
      return False
    Trap n -> do
      modify(\g -> g {playerScore = playerScore g - n})
      liftIO(putStrLn ("Trap: -" ++ show n ++ " points"))
      return False
    Obstacle n -> do 
      modify (\g -> g { playerPosition = max 0 (playerPosition g - n) })
      liftIO(putStrLn ("Obstacle,  pushed back " ++ show n ++ " steps"))
      return False
    DecisionPoint options -> do 
      liftIO (putStrLn "Decision point")
      let opts = map fst options
      choice <- makeDecision opts
      case lookup choice options of 
        Just newPosition -> do
          modify (\g -> g {playerPosition = newPosition})
        Nothing -> return ()
      return False
    Goal -> do
      liftIO (putStrLn "You've reached the treassure")
      return True

playTurn :: AdventureGame Bool
playTurn = do 
  gameState <- get
  liftIO (displayGameState gameState)
  if playerEnergy gameState <= 0
    then do 
      liftIO (putStrLn "Not enough energy! Game Over!")
      return True
    else do 
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      handleLocation
    
playGame :: AdventureGame ()
playGame = do 
  ended <- playTurn
  if ended then return () else playGame 