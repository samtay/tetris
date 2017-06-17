{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tetris where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (ViewL(..), ViewR(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Lens.Micro
import Lens.Micro.TH
import System.Random (getStdRandom, randomR)

import Prelude hiding (Left, Right)
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))

-- TODO
--   1. prune rowClears on zero
--   2. make sure argument orders make sense
--   3. possibly add 'user' to game state to draw name entry from UI.Game

-- Types and instances

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = (Int, Int)

-- | Tetris shape in location context
data Block = Block
  { _shape  :: Tetrimino -- ^ block type
  , _origin :: Coord -- ^ origin
  , _extra  :: [Coord] -- ^ extraneous cells
  } deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

-- | Board
--
-- If coordinate not present in map, yet in bounds, then it is empty,
-- otherwise its value is the type of tetrimino occupying it.
type Board = Map Coord Tetrimino

-- | Game state
data Game = Game
  { _level :: Int
  , _block :: Block
  , _nextShape :: Tetrimino
  , _nextShapeBag :: Seq.Seq Tetrimino
  , _rowClears :: Seq.Seq Int
  , _score :: Int
  , _board :: Board
  } deriving (Eq, Show)

makeLenses ''Game

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s

instance Translatable Coord where
  translate Left (x, y) = (x-1, y)
  translate Right (x, y) = (x+1, y)
  translate Down (x,y) = (x, y-1)

instance Translatable Block where
  translate d b =
    b & origin %~ translate d
      & extra %~ fmap (translate d)

-- Low level functions on blocks and coordinates

initBlock :: Tetrimino -> Block
initBlock I = Block I startOrigin [(-2,0), (-1,0), (1,0)]
initBlock O = Block O startOrigin [(-1,0), (-1,-1), (0,-1)]
initBlock S = Block S startOrigin [(-1,-1), (0,-1), (1,0)]
initBlock Z = Block Z startOrigin [(-1,0), (0,-1), (1,-1)]
initBlock L = Block L startOrigin [(-1,-1), (-1,0), (1,0)]
initBlock J = Block J startOrigin [(-1,0), (1,0), (1,-1)]
initBlock T = Block T startOrigin [(-1,0), (0,-1), (1,0)]

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | Starting block origin
startOrigin :: Coord
startOrigin = (6, 22)

-- | Rotate block counter clockwise about origin
-- *Note*: Strict unsafe rotation not respecting boundaries
-- Safety can only be assured within Game context
rotate' :: Block -> Block
rotate' b@(Block s o@(xo,yo) cs)
  | s == O = b -- O doesn't need rotation
  | s == I && (xo,yo+1) `elem` cs = rotateWith clockwise b -- I only has two orientations
  | otherwise = rotateWith counterclockwise b
  where
    rotateWith :: (Coord -> Coord -> Coord) -> Block -> Block
    rotateWith dir b = b & extra %~ fmap (dir (b ^. origin))

    clockwise :: Coord -- ^ origin
              -> Coord -- ^ point to rotate around origin
              -> Coord
    clockwise (xo, yo) (x, y) = (xo + y - yo, xo + y - x)

    counterclockwise :: Coord -- ^ origin
                     -> Coord -- ^ point to rotate around origin
                     -> Coord
    counterclockwise (xo, yo) (x, y) = (xo + yo - y, x + yo - xo)

-- | Get coordinates of entire block
blockCoords :: Block -> [Coord]
blockCoords b = b ^. origin : b ^. extra

-- Higher level functions on game and board

-- | Facilitates cycling through at least 4 occurences of each shape
-- before next bag (random permutation of 4*each tetrimino) is created. If input is empty,
-- generates new bag, otherwise just unshifts the first value and returns pair.
bagFourTetriminoEach :: Seq.Seq Tetrimino -> IO (Tetrimino, Seq.Seq Tetrimino)
bagFourTetriminoEach = go . Seq.viewl
  where
    go (t :< ts) = return (t, ts)
    go EmptyL = freshList >>= bagFourTetriminoEach
    freshList = shuffle $ Seq.cycleTaking 28 $ Seq.fromList [(I)..]

-- | Initialize a game with a given level
initGame :: Int ->  IO Game
initGame lvl = do
  (s1, bag1) <- bagFourTetriminoEach mempty
  (s2, bag2) <- bagFourTetriminoEach bag1
  return $
    Game { _level = lvl
         , _block = initBlock s1
         , _nextShape = s2
         , _nextShapeBag = bag2
         , _score = 0
         , _rowClears = mempty
         , _board = mempty }

isGameOver :: Game -> Bool
isGameOver g = blockStopped g && g ^. block ^. origin == startOrigin

timeStep :: Game -> IO Game
timeStep g = if (blockStopped g)
                then return . coreUpdater $ g
                else stopUpdater . coreUpdater $ g
  where
    coreUpdater = gravitate
    stopUpdater = nextBlock . updateScore . clearFullRows . freezeBlock

-- TODO check if mapKeysMonotonic works
clearFullRows :: Game -> Game
clearFullRows g = g & board %~ clearBoard
                    & rowClears %~ (|> rowCount)
  where
    clearBoard           = M.mapKeys shiftCoordAbove . M.filterWithKey isInFullRow
    isInFullRow (_,y) _  = y `elem` fullRowIndices
    rowCount             = length fullRowIndices
    fullRowIndices       = filter isFullRow [1..boardHeight]
    isFullRow r          = boardWidth == (length . M.filterWithKey (inRow r) $ g ^. board)
    inRow r (_, y) _     = r == y
    shiftCoordAbove (x,y) =
      let offset = length . filter (< y) $ fullRowIndices
       in (x, y - offset)

-- | This updates game points with respect to the current
-- _rowClears value (thus should only be used ONCE per step)
--
-- Note I'm keeping rowClears as a sequence in case I want to award
-- more points for back to back clears, right now the scoring is more simple
updateScore :: Game -> Game
updateScore g = g & score %~ (+ newPoints)
  where
    newPoints = (g ^. level) * (g ^. rowClears ^. to latestOrZero ^. to points)

-- | Points awarded from number of rows cleared
points :: Int -- ^ rows cleared
       -> Int -- ^ resulting points
points 0 = 0
points 1 = 40
points 2 = 100
points 3 = 300
points n = 800


-- | Handle counterclockwise block rotation (if possible)
-- Allows wallkicks: http://tetris.wikia.com/wiki/TGM_rotation
rotate :: Game -> Game
rotate g = g & block .~ nextB
  where nextB     = fromMaybe blk $ getFirst . mconcat $ bs
        bs        = map ($ blk) safeFuncs
        safeFuncs = map (mkSafe .) funcs
        mkSafe b  = if isValidBlockPosition b brd then First (Just b) else First Nothing
        funcs     = [rotate', rotate' . translate Left, rotate' . translate Right]
        blk       = g ^. block
        brd       = g ^. board

blockStopped :: Game -> Bool
blockStopped g = isStopped (g ^. board) (g ^. block)

-- | Check if a block on a board is stopped from further gravitation
isStopped :: Board -> Block -> Bool
isStopped b = any (`M.member` b) . map (translate Down) . blockCoords

-- | Freeze current block
freezeBlock :: Game -> Game
freezeBlock g = g & board %~ (M.union blkMap)
  where blk    = g ^. block
        blkMap = M.fromList $ zip (blk ^. to blockCoords) (repeat $ blk ^. shape)

-- | Replace block with next block
nextBlock :: Game -> IO Game
nextBlock g = do
  (t, ts) <- bagFourTetriminoEach (g ^. nextShapeBag)
  return $
    g & block        .~ initBlock (g ^. nextShape)
      & nextShape    .~ t
      & nextShapeBag .~ ts

-- | Try to shift current block; if shifting not possible, leave block where it is
shift :: Direction -> Game -> Game
shift d g = g & block %~ shiftBlock
  where shiftBlock b = if isValidBlockPosition (translate d b) (g ^. board)
                          then translate d b
                          else b

-- | Check if coordinate is already occupied or free in board
isFree, isOccupied :: Board -> Coord -> Bool
isFree     = flip M.notMember
isOccupied = flip M.member

-- | Check if coordinate is in or out of bounds
isInBounds, isOutOfBounds :: Coord -> Bool
isInBounds (x,y) = x `elem` [1..boardWidth] && y `elem` [1..boardHeight]
isOutOfBounds = not . isInBounds

-- | Gravitate current block, i.e. shift down
gravitate :: Game -> Game
gravitate = shift Down

-- | Checks if block's potential new location is valid
isValidBlockPosition :: Block -> Board -> Bool
isValidBlockPosition blk brd = all validCoord $ blk ^. to blockCoords
  where validCoord = (&&) <$> isFree brd <*> isInBounds

-- | Shuffle a sequence (random permutation)
shuffle :: Seq.Seq a -> IO (Seq.Seq a)
shuffle xs
  | null xs   = mempty
  | otherwise = do
      randomPosition <- getStdRandom (randomR (0, length xs - 1))
      let (left, right) = Seq.splitAt randomPosition xs
          (y :< ys)     = Seq.viewl right
      fmap (y <|) (shuffle $ left >< ys)

latestOrZero :: Seq.Seq Int -> Int
latestOrZero = go . Seq.viewr
  where go EmptyR = 0
        go (_ :> n) = n
