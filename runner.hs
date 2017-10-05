 -- Runner simulation
 
data Point = Point {x :: Int, y :: Int} deriving (Eq,Show)
data Runner = Runner {name :: String, direction :: Point, velocity :: Int} deriving (Eq,Show)
data RunnerState = RunnerState {runner :: Runner, position :: Point } deriving (Eq,Show)
data GameState = GameState {time :: Int, runners :: [RunnerState]} deriving (Eq,Show)
data GameHistory = GameHistory {history :: [GameState]} deriving (Eq,Show)
 
r1 = Runner "one" (Point 1 0) 2
r2 = Runner "one" (Point 0 1) 3

rs = [r1, r2] 
 
rss = map (\r -> (RunnerState r (Point 0 0))) rs 
 
 -- Length of run, list of players
doRun :: Int -> [Runner] -> GameHistory
doRun endTime rs = doRun' endTime (GameState 0 (map (\r -> (RunnerState r (Point 0 0))) rs)) (GameHistory [])
 
 -- length of run,
doRun' :: Int -> GameState -> GameHistory -> GameHistory
doRun' endTime state@(GameState time rs) (GameHistory history)
    | time == endTime = (GameHistory (history ++ [state]))
    | otherwise = doRun' endTime (GameState (time + 1) (map (\r -> updateRunnerState time r) rs)) (GameHistory (history ++ [state]))
    
updateRunnerState :: Int -> RunnerState -> RunnerState
updateRunnerState time rs@(RunnerState r@(Runner _ (Point incX incY ) velocity) (Point atX atY)) 
    | time `mod` velocity == 0 = RunnerState r (Point (atX + incX) (atY + incY))
    | otherwise = rs


