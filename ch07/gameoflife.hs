module GameOfLife  where
type Row = [State]
type Game = [Row]
type Position = (Int, Int)
type GameSize = (Int, Int)
type GameState = (Game, Position)
data State = Dead | Alive deriving (Show, Eq)

buildRow      ::     Int -> Row
buildRow x    =      [Dead | _ <- [1..x]]
buildBoard    ::     GameSize -> Game
buildBoard (x, y)  = [buildRow x | _ <- [1..y]]

isAlive :: State -> Bool
isAlive Alive = True
isAlive _     = False

isValidPosition :: Position -> GameSize -> Bool
isValidPosition (row, column) (rowMax, columnMax)
    | row < 0 || row >= rowMax = False
    | column < 0 || column >= columnMax = False
    | otherwise = True

getState :: GameState -> State
getState (game, (row, column)) = (game !! row) !! column

neighborStates :: GameState -> [State]
neighborStates game (row, column) = [getState (game, position) | position <- positions,
                                                                 isValidPosition position gameSize]
    where positions = [(row - 1, column),
                       (row + 1, column),
                       (row,     column + 1),
                       (row,     column - 1),
                       (row - 1, column - 1),
                       (row + 1, column + 1),
                       (row - 1, column + 1),
                       (row + 1, column - 1)]
          gameSize = (length (game !! 0), length game)

countNeighbors  :: GameState -> Integer
countNeighbors (game, position) = fromIntegral $ length $ filter isAlive  $ neighborStates game position

updateCell      :: Position -> Game -> Game
updateCell     position (oldGame, newGame) 
    | countNeighbors gamestate <= 1 = (oldGame, newGame)-- die, if not already
    | countNeighbors gamestate < 3  = (oldGame, newGame)
    | countNeighbors gamestate == 3 = (oldGame, newGame)-- become alive if not already
    | otherwise                     = (oldGame, newGame)-- die, if not already

tick :: Game -> Game
tick game = snd $ foldr updateCell (game, game) indecies
    where  indecies = [((rowIndex, columnIndex)) |  (row, rowIndex) <- zip game [0..],
                                                    (cel, columnIndex) <- zip row [0..]]
