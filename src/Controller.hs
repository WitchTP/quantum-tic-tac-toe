{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Controller where

import Control.Monad (void)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Text (encodeToLazyText)
import Data.IORef (IORef, readIORef, atomicWriteIORef)
import Data.List (intersperse)
import Data.Text.Lazy (toStrict)
import GHC.Generics
import Game (GameData(..), Coord(..), CoordHalf(..), placePiece, initialState)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.UI.Webviewhs as WHS

data Command = Load
             | Place Coord
             | Debug T.Text
             | Clear
             deriving (Eq, Generic)

instance FromJSON Command
instance ToJSON Command

handleCommand :: IORef GameData -> Command -> WHS.Window a -> IO ()
handleCommand gameState Load window = sendGameState gameState window
handleCommand _ (Debug text) _ = T.putStrLn text
handleCommand gameState Clear window = atomicWriteIORef gameState initialState >> sendGameState gameState window
handleCommand gameState (Place coord) window = readIORef gameState >>= atomicWriteIORef gameState . flip placePiece coord >> sendGameState gameState window

sendGameState :: IORef GameData -> WHS.Window a -> IO ()
sendGameState gameState window = readIORef gameState >>= void . WHS.runJavaScript' window . getSendGameStateScript

getSendGameStateScript :: GameData -> T.Text
getSendGameStateScript state = "window.gameState(" <> jsonToText state <> ");"

jsonToText :: ToJSON a => a -> T.Text
jsonToText = toStrict . encodeToLazyText

sampleCommands :: T.Text
sampleCommands = (<>) "Valid commands: " $ foldMap id $ intersperse ", " $ fmap jsonToText [Load, Place $ Coord A B, Clear, Debug "sample text!"]
