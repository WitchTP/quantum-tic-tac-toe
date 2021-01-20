{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric #-}

module Game where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Maybe (catMaybes)
import GHC.Generics

data GameData = GameData { gameBoard :: !(Board Piece)
                         , currentTurn :: !Turn
                         , currentScoringLines :: ![Line]
                         } deriving (Generic)

data Piece = X | O | N deriving (Eq, Generic)
data CoordHalf = A | B | C deriving (Eq, Generic)
data Coord = Coord CoordHalf CoordHalf deriving (Eq, Generic)
data Turn = TurnX | TurnO deriving (Eq, Generic)

data Triple a = Triple { tripleFirst :: !a
                       , tripleSecond :: !a
                       , tripleThird :: !a
                       } deriving (Eq, Functor, Foldable, Generic)

data Line = Line { lineStart :: !Coord
                 , lineEnd :: !Coord
                 , linePiece :: !Piece
                 } deriving (Eq, Generic)

newtype Board a = Board (Triple (Triple a)) deriving (Eq, Functor, Foldable, Generic)

instance FromJSON GameData
instance FromJSON Piece
instance FromJSON CoordHalf
instance FromJSON Coord
instance FromJSON Turn
instance (FromJSON a) => FromJSON (Triple a)
instance FromJSON Line
instance (FromJSON a) => FromJSON (Board a)

instance ToJSON GameData
instance ToJSON Piece
instance ToJSON CoordHalf
instance ToJSON Coord
instance ToJSON Turn
instance (ToJSON a) => ToJSON (Triple a)
instance ToJSON Line
instance (ToJSON a) => ToJSON (Board a)

initialState :: GameData
initialState = GameData { gameBoard = emptyBoard
                        , currentTurn = TurnX
                        , currentScoringLines = [] }

placePiece :: GameData -> Coord -> GameData
placePiece gameState coord = do
  let board = gameBoard gameState
  let turn = currentTurn gameState
  let piece = turnToPiece turn
  let newBoard = updateGameBoard board piece coord
  let scoringLines = findScoringLines newBoard
  let newTurn = nextTurn board newBoard turn
  GameData { gameBoard = newBoard
           , currentTurn = newTurn
           , currentScoringLines = scoringLines }

emptyBoard :: Board Piece
emptyBoard = Board $ Triple (Triple N N N) (Triple N N N) (Triple N N N)

updateGameBoard :: Board Piece -> Piece -> Coord -> Board Piece
updateGameBoard (Board triple@(Triple (Triple N _ _) _ _)) piece (Coord A A) = Board $ triple { tripleFirst = ((tripleFirst triple) { tripleFirst = piece }) }
updateGameBoard (Board triple@(Triple (Triple _ N _) _ _)) piece (Coord A B) = Board $ triple { tripleFirst = ((tripleFirst triple) { tripleSecond = piece }) }
updateGameBoard (Board triple@(Triple (Triple _ _ N) _ _)) piece (Coord A C) = Board $ triple { tripleFirst = ((tripleFirst triple) { tripleThird = piece }) }
updateGameBoard (Board triple@(Triple _ (Triple N _ _) _)) piece (Coord B A) = Board $ triple { tripleSecond = ((tripleSecond triple) { tripleFirst = piece }) }
updateGameBoard (Board triple@(Triple _ (Triple _ N _) _)) piece (Coord B B) = Board $ triple { tripleSecond = ((tripleSecond triple) { tripleSecond = piece }) }
updateGameBoard (Board triple@(Triple _ (Triple _ _ N) _)) piece (Coord B C) = Board $ triple { tripleSecond = ((tripleSecond triple) { tripleThird = piece }) }
updateGameBoard (Board triple@(Triple _ _ (Triple N _ _))) piece (Coord C A) = Board $ triple { tripleThird = ((tripleThird triple) { tripleFirst = piece }) }
updateGameBoard (Board triple@(Triple _ _ (Triple _ N _))) piece (Coord C B) = Board $ triple { tripleThird = ((tripleThird triple) { tripleSecond = piece }) }
updateGameBoard (Board triple@(Triple _ _ (Triple _ _ N))) piece (Coord C C) = Board $ triple { tripleThird = ((tripleThird triple) { tripleThird = piece }) }
updateGameBoard board _ _ = board

nextTurn :: (Eq a) => Board a -> Board a -> Turn -> Turn
nextTurn oldBoard newBoard turn = if oldBoard == newBoard then turn else getNextTurn turn
  where
    getNextTurn :: Turn -> Turn
    getNextTurn TurnX = TurnO
    getNextTurn TurnO = TurnX

turnToPiece :: Turn -> Piece
turnToPiece TurnX = X
turnToPiece TurnO = O

getCoordsWithPiece :: Board Piece -> Piece -> [Coord]
getCoordsWithPiece board piece = fmap snd $ filter ((==) piece . fst) $ flip zip coordList $ foldMap pure board

coordList :: [Coord]
coordList = [Coord A A, Coord A B, Coord A C, Coord B A, Coord B B, Coord B C, Coord C A, Coord C B, Coord C C]

findScoringLines :: Board Piece -> [Line]
findScoringLines board = catMaybes $ fmap (\(start, end) -> getScoringLine start end board) getLineCoords

getLineCoords :: [(Coord, Coord)]
getLineCoords = [(Coord A A, Coord A C), (Coord B A, Coord B C), (Coord C A, Coord C C), (Coord A A, Coord C A), (Coord A B, Coord C B), (Coord A C, Coord C C), (Coord A A, Coord C C), (Coord A C, Coord C A)]

getScoringLine :: Coord -> Coord -> Board Piece -> Maybe Line
getScoringLine start@(Coord A A) end@(Coord A C) (Board (Triple (Triple X X X) _ _)) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A A) end@(Coord A C) (Board (Triple (Triple O O O) _ _)) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord B A) end@(Coord B C) (Board (Triple _ (Triple X X X) _)) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord B A) end@(Coord B C) (Board (Triple _ (Triple O O O) _)) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord C A) end@(Coord C C) (Board (Triple _ _ (Triple X X X))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord C A) end@(Coord C C) (Board (Triple _ _ (Triple O O O))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord A A) end@(Coord C A) (Board (Triple (Triple X _ _) (Triple X _ _) (Triple X _ _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A A) end@(Coord C A) (Board (Triple (Triple O _ _) (Triple O _ _) (Triple O _ _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord A B) end@(Coord C B) (Board (Triple (Triple _ X _) (Triple _ X _) (Triple _ X _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A B) end@(Coord C B) (Board (Triple (Triple _ O _) (Triple _ O _) (Triple _ O _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord A C) end@(Coord C C) (Board (Triple (Triple _ _ X) (Triple _ _ X) (Triple _ _ X))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A C) end@(Coord C C) (Board (Triple (Triple _ _ O) (Triple _ _ O) (Triple _ _ O))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord A A) end@(Coord C C) (Board (Triple (Triple X _ _) (Triple _ X _) (Triple _ _ X))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A A) end@(Coord C C) (Board (Triple (Triple O _ _) (Triple _ O _) (Triple _ _ O))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine start@(Coord A C) end@(Coord C A) (Board (Triple (Triple _ _ X) (Triple _ X _) (Triple X _ _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = X }
getScoringLine start@(Coord A C) end@(Coord C A) (Board (Triple (Triple _ _ O) (Triple _ O _) (Triple O _ _))) = Just $ Line { lineStart = start, lineEnd = end, linePiece = O }
getScoringLine _ _ _ = Nothing
