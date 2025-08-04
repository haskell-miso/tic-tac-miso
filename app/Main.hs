-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
import           Control.Monad
import           Data.Maybe
import           Control.Applicative
import           Data.List
import           Control.Lens (set, ix, imap)
import           Prelude
-----------------------------------------------------------------------------
import           Miso hiding (set)
import           Miso.String (ms)
import qualified Miso.Style as CSS
import           Miso.Lens hiding (set)
import           Miso.Lens.TH
-----------------------------------------------------------------------------
data Piece
  = X
  | O
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Nothing indicates empty spot
type Board = [[Maybe Piece]]        -- 3 x 3 list
-----------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model emptyBoard X
-----------------------------------------------------------------------------
data Model
  = Model
  { _board :: Board
  , _piece :: Piece
  } deriving Eq
-----------------------------------------------------------------------------
$(makeLenses ''Model)
-----------------------------------------------------------------------------
data Action = Place Piece (Int, Int)
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp $ (component emptyModel updateModel viewModel)
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  Place p (x, y) -> do
    modify $ \m -> m { _board = set (ix y . ix x) (Just p) (m ^. board) }
    piece %= \xo -> if xo == X then O else X
    currentBoard <- gets _board
    case checkWinner currentBoard of
      Just X -> do
        io_ (alert "X wins!")
        board .= emptyBoard
      Just O -> do
        io_ (alert "O wins!")
        board .= emptyBoard
      Nothing -> do
        when (gameOver currentBoard) $ do
          io_ (alert "Game over!")
          board .= emptyBoard
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel Model {..} =
  div_
  []
  [ h1_ [] [ "tic-tac-miso" ]
  , div_ [] (imap makeRow _board)
  ]
  where
    makeRow :: Int -> [Maybe Piece] -> View Model Action
    makeRow y row = div_ [] (imap makePiece row)
      where
        makePiece :: Int -> Maybe Piece -> View Model Action
        makePiece x p =
          div_
          [ className "piece"
          , CSS.style_
            [ CSS.display "inline-block"
            , CSS.width "50px"
            , CSS.margin "auto"
            , CSS.textAlign "center"
            ]
          ]
          [ case p of
              Nothing ->
                button_
                [ onClick $ Place _piece (x,y)
                , CSS.style_
                  [ CSS.height (CSS.px 25)
                  , CSS.width (CSS.px 25)
                  ]
                ]
                [ text "  "
                ]
              Just piece' ->
                span_
                [ CSS.style_
                  [ CSS.height (CSS.px 25)
                  , CSS.width (CSS.px 25)
                  ]
                ]
                [ text (ms (show piece'))
                ]
          ]
-----------------------------------------------------------------------------
checkWinner :: Board -> Maybe Piece
checkWinner board_
  | any (all (==Just X)) board_ = Just X
  | any (all (==Just O)) board_ = Just O
  | any (all (==Just X)) (transpose board_) = Just X
  | any (all (==Just O)) (transpose board_) = Just O
  | otherwise = checkDiagonals board_
-----------------------------------------------------------------------------
checkDiagonals :: Board -> Maybe Piece
checkDiagonals [x,y,z] = check X <|> check O
  where
    check p
      | all (==Just p) [ x !! 0, y !! 1, z !! 2 ] = Just p
      | all (==Just p) [ x !! 2, y !! 1, z !! 0 ] = Just p
      | otherwise = Nothing
checkDiagonals _ = Nothing
-----------------------------------------------------------------------------
gameOver :: Board -> Bool
gameOver = all isJust . concat
-----------------------------------------------------------------------------
