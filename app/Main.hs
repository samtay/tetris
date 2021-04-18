module Main where

import Control.Monad (when)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))

import Tetris (Game(..))
import UI.PickLevel (pickLevel)
import UI.Game (playGame)

data Opts = Opts
  { hardDrop :: HardDropOpt
  , level    :: Maybe Int
  , score    :: Bool
  }

data HardDropOpt = None | AsciiOnly | CustomChars String

opts :: Parser Opts
opts = Opts
  <$> hardDropOpt
  <*> optional (option auto
    (  long "level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Specify level (unspecified results in prompt)" ))
  <*> switch
    (  long "high-score"
    <> help "Print high score and exit" )

hardDropOpt :: Parser HardDropOpt
hardDropOpt = noneOpt <|> asciiOpt <|> custOpt
  where
    noneOpt = flag' None
      (  long "no-preview"
      <> short 'n'
      <> help "Don't show preview cell" )
    asciiOpt = flag' AsciiOnly
      (  long "ascii-only"
      <> short 'a'
      <> help "Use '[]' as hard drop preview cell" )
    custOpt = CustomChars <$> option twoChar
      (  long "preview-chars"
      <> short 'p'
      <> metavar "CHARS"
      <> value "◤◢"
      <> showDefaultWith (const "◤◢")
      <> help "Customize two character preview cell" )

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

hdOptStr :: HardDropOpt -> Maybe String
hdOptStr None            = Nothing
hdOptStr AsciiOnly       = Just "[]"
hdOptStr (CustomChars s) = Just s

main :: IO ()
main = do
  (Opts hd ml hs) <- execParser fullopts           -- get CLI opts/args
  when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  l <- maybe pickLevel return ml                   -- pick level prompt if necessary
  g <- playGame l (hdOptStr hd)                    -- play game
  handleEndGame (_score g)                         -- save & print score

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn $ "Your final score: " ++ show s
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      setHighScore s

printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s

getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing

setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "tetris"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")
