{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Controller
import Data.Aeson (decode)
import Data.FileEmbed (embedDir)
import Data.IORef (IORef, newIORef)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Game (GameData(..), initialState)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  gameState <- newIORef initialState
  WHS.withWindowLoop windowParams (handleInput gameState) (WHS.WithWindowLoopSetUp windowSetup) (WHS.WithWindowLoopTearDown windowTeardown) windowLoop

windowParams :: WHS.WindowParams
windowParams = WHS.WindowParams { WHS.windowParamsTitle = "Quantum Tic Tac Toe"
                                , WHS.windowParamsUri = page
                                , WHS.windowParamsWidth = 800
                                , WHS.windowParamsHeight = 600
                                , WHS.windowParamsResizable = True
                                , WHS.windowParamsDebuggable = True
                                }

handleInput :: IORef GameData -> WHS.Window a -> T.Text -> IO ()
handleInput gameState window text = do
  case decode (encodeUtf8 $ fromStrict text) of
    Just command -> handleCommand gameState command window
    Nothing -> T.putStrLn $ "Invalid command: " <> text <> ". " <> sampleCommands

windowSetup :: WHS.Window a -> IO ()
windowSetup window = do
  void $ WHS.injectCss' window styling
  mapM_ (WHS.runJavaScript' window) scripts

windowTeardown :: WHS.Window a -> IO ()
windowTeardown = void . return . const

windowLoop :: WHS.Window a -> IO Bool
windowLoop _ = return True

page :: T.Text
page = asHtml $ findResourceByPath "main.html"

scripts :: [T.Text]
scripts = fmap findResourceByPath ["flyd.min.js", "phaser.min.js", "main.js"]

styling :: T.Text
styling = findResourceByPath "main.css"

asHtml :: T.Text -> T.Text
asHtml = (<>) "data:text/html,"

-- This method throws a runtime exception if the resource cannot be found.
findResourceByPath :: FilePath -> T.Text
findResourceByPath path = decodeUtf8 $ head $ fmap snd $ filter ((==) path . fst) resources

resources :: [(FilePath, B.ByteString)]
resources = $(embedDir "resources")
