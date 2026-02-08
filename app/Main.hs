-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Data.Bool
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Control.Monad
import           Data.Maybe
import           Prelude
-----------------------------------------------------------------------------
import           Miso hiding (set)
import           Miso.Html hiding (title_)
import           Miso.Html.Property
import           Miso.String (ToMisoString(..))
import qualified Miso.String as MS
import qualified Miso.CSS as CSS
import           Miso.Lens hiding (set)
import           Miso.Lens.TH
-----------------------------------------------------------------------------
-- | Nothing indicates empty spot
type Board = IntMap Piece
-----------------------------------------------------------------------------
data Piece
  = X
  | O
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
instance ToMisoString Piece where
  toMisoString X = "X"
  toMisoString O = "O"
-----------------------------------------------------------------------------
next :: Piece -> Piece
next X = O
next O = X
-----------------------------------------------------------------------------
data Model
  = Model
  { _board :: Board
  , _piece :: Piece
  , _winners :: [Int]
  , _gameover :: Bool
  } deriving Eq
-----------------------------------------------------------------------------
$(makeLenses ''Model)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = IM.empty
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model emptyBoard X [] False
-----------------------------------------------------------------------------
data Action = Place Int | Reset
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents (component emptyModel updateModel viewModel)
#ifndef WASM
  { styles = [ Href "assets/style.css" ]
  }
#endif
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  Reset ->
    this .= emptyModel
  Place key ->
    IM.lookup key <$> gets _board >>= \case
      Just _ ->
        pure ()
      Nothing -> do
        currentPiece <- gets _piece
        board %= IM.insert key currentPiece
        currentBoard <- gets _board
        piece %= next
        case checkWinners currentPiece currentBoard of
          Just (X, wins) -> do
            piece .= X 
            winners .= wins
            gameover .= True
          Just (O, wins) -> do
            piece .= O
            winners .= wins
            gameover .= True
          Nothing -> do
            itsOver <- gameOver <$> gets _board
            when itsOver (gameover .= True)
-----------------------------------------------------------------------------            
githubStar :: View model action
githubStar = iframe_
    [ title_ "GitHub"
    , height_ "30"
    , width_ "170"
    , CSS.style_ [ CSS.marginLeft (CSS.px 60) ]
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=tic-tac-miso&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel m@Model {..} = div_ []
  [ div_
    [ class_ "game-container"
    ]
    [ h1_
      []
      [ "Tic Tac Miso 🍜"
      ]
    , div_
      [ id_ "status"
      , class_ "status"
      ]
      [ if | m ^. gameover && null (m ^. winners) -> text "Stalemate !"
           | not $ null (m ^. winners) -> text $ ms (m ^. piece) <> " wins !"
           | otherwise -> text ("Player " <> ms (show _piece) <> "'s turn")
      ]
    , div_
      [ id_ "board"
      , class_ "board"
      ]
      [ optionalAttrs
          button_
            [ classes_
              [ "cell"
              , maybe MS.empty (MS.toLower . ms) maybePiece
              , bool mempty "winning-cell" $ ix `elem` (m ^. winners)
              ]
            ] (not (m ^. gameover))
            [ onClick (Place ix)
            ]
        [ text (ms piece_)
        | Just piece_ <- pure maybePiece
        ]
      | (ix, maybePiece) <-
          [ (ix, _board IM.!? ix)
          | ix <- [0..8]
          ]
      ]
    , button_
      [ id_ "resetBtn"
      , class_ "reset-btn"
      , onClick Reset
      ]
      [ "Reset Game"
      ]
    , githubStar
    ]
  ]
-----------------------------------------------------------------------------
possibilities :: [[Int]]
possibilities = rows ++ cols ++ diag
  where
    rows = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    cols = [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
    diag = [[0, 4, 8], [2, 4, 6]]
-----------------------------------------------------------------------------
checkWinners :: Piece -> Board -> Maybe (Piece, [Int])
checkWinners p b = join $ listToMaybe
  [ x | x@Just{} <- checkWinner <$> possibilities ]
    where
      checkWinner [x,y,z]
        | winner = Just (p, [x,y,z])
        | otherwise = Nothing
        where
          winner = and
            [ b IM.!? x == Just p
            , b IM.!? y == Just p
            , b IM.!? z == Just p
            ]
      checkWinner _ = Nothing
-----------------------------------------------------------------------------
gameOver :: Board -> Bool
gameOver = (==9) . IM.size 
-----------------------------------------------------------------------------
