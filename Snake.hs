import GifStream
import System.Random

-- Stopping focus of the browser tab stops the animation. Reload the page to fix it.

type Position = (Int,Int)

data Action = MoveLeft | MoveRight | MoveUp | MoveDown deriving Eq

data State = State
    { oldAction :: Action
    , snake :: [Position]
    , food :: Position
    }

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 250000 -- in µs
port = 5002

width = 32
height = 32
zoom = 4

main :: IO ()
main = server port delay logic

logic :: IO () -> IO Char -> (Frame -> IO ()) -> IO ()
logic wait getInput sendFrame = initialState >>= go
  where
    go (State oldAction snake food) = do
      input <- getInput

      -- Generate new state
      let action = validateAction oldAction (charToAction input oldAction)
      let newSnake = moveSnake snake food action
      newFood <- moveFood newSnake food
      let frame = map (map (colorize newSnake newFood)) fieldPositions

      sendFrame (scale zoom frame)

      wait
      if checkGameOver newSnake then initialState >>= go else go (State action newSnake newFood)

initialState :: IO State
initialState = do
  let startSnake = [(15,15),(14,15)]
  let food = (28,28)
  return (State MoveRight startSnake food)

charToAction :: Char -> Action -> Action
charToAction c oldAction = case c of
  'w' -> MoveUp
  'a' -> MoveLeft
  's' -> MoveDown
  'd' -> MoveRight
  _   -> oldAction

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) (map (concatMap (replicate z)) frame)

fieldPositions :: [[Position]]
fieldPositions = reverse (vertical width height) where
    vertical w h = map (horizontal w) [0..(h-1)]
    horizontal w h = map (\x -> (x,h)) [0..(w-1)]
    
colorize :: [Position] -> Position -> Position -> RGB
colorize snake food pos
    | pos == food       = (3,0,0)
    | elem pos snake    = (3,3,3)
    | otherwise         = (1,1,1)
    
validateAction :: Action -> Action -> Action
validateAction MoveUp MoveDown      = MoveUp
validateAction MoveRight MoveLeft   = MoveRight
validateAction MoveDown MoveUp      = MoveDown
validateAction MoveLeft MoveRight   = MoveLeft
validateAction _ newAction          = newAction

moveSnake :: [Position] -> Position -> Action -> [Position]
moveSnake snake food action = if food == newHead then newHead:snake else newHead:(trimTail snake) where
    newHead = case action of
        MoveUp      -> (fst (head snake), snd (head snake) + 1)
        MoveRight   -> (fst (head snake) + 1, snd (head snake))
        MoveDown    -> (fst (head snake), snd (head snake) - 1)
        MoveLeft    -> (fst (head snake) - 1, snd (head snake))
    trimTail []     = []
    trimTail (h:[]) = []
    trimTail (h:b)  = h:(trimTail b)
        
moveFood :: [Position] -> Position -> IO Position
moveFood snake food = do
    let l = length snake
    let flatPositions = foldr (++) [] fieldPositions
    let freePositions = filter (\p -> not (elem p snake)) flatPositions
    x <- randomRIO (0, width * height - l - 1)
    if elem food snake then return (freePositions !! x) else return food
    
checkGameOver :: [Position] -> Bool
checkGameOver []        = False
checkGameOver (p:ps)    = if inBounds p && not (elem p ps) then checkGameOver ps else True where
    inBounds (x,y) = x >= 0 && x < width && y >= 0 && y < height