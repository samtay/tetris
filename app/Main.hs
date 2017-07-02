module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Exit (exitSuccess)

import Tetris (Game(..))
import UI.PickLevel (pickLevel)
import UI.Game (playGame)

import Options.Applicative

data Opts = Opts
  { hardDrop :: Maybe HardDropOpt
  , level    :: Maybe Int
  , score    :: Bool
  } deriving (Show) -- TODO remove

data HardDropOpt = AsciiOnly | CustomChars String deriving (Show) -- TODO remove

opts :: Parser Opts
opts = Opts
  <$> optional hardDropOpt
  <*> optional (option auto
    (  long "level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Specify level (unspecified results in prompt)" ))
  <*> switch
    (  long "high-score"
    <> help "Print high score and exit" )


hardDropOpt :: Parser HardDropOpt
hardDropOpt = asciiOpt <|> custOpt
  where
    asciiOpt = flag' AsciiOnly
      (  long "ascii-only"
      <> short 'a'
      <> help "Use '[]' as hard drop preview cell instead of '◤◢'" )
    custOpt = CustomChars <$> option twoChar
      (  long "preview-chars"
      <> short 'p'
      <> metavar "CHARS"
      <> help "Custom two character preview cell" )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "tetris - the iconic game right in your terminal" )

twoChar :: ReadM String
twoChar = do
  cs <- str
  if length cs /= 2
     then readerError "Preview must be two characters long"
     else return cs

hdOptStr :: HardDropOpt -> String
hdOptStr AsciiOnly = "[]"
hdOptStr (CustomChars s) = s

main :: IO ()
main = do
  (Opts hd ml hs) <- execParser fullopts          -- get CLI opts/args
  let mp = hdOptStr <$> hd                        -- determine hard drop preview cell
  when hs (getHighScore >>= print >> exitSuccess) -- show high score and exit
  l <- fromMaybe pickLevel (return <$> ml)        -- pick level prompt if necessary
  g <- playGame l mp                              -- play game
  handleEndGame (_score g)                        -- save & print score

handleEndGame :: Int -> IO ()
handleEndGame = const $ return ()

getHighScore :: IO Int
getHighScore = error "Not yet implemented"
