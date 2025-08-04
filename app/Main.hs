-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
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
