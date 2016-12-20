{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Control.Monad.Trans (liftIO)
import Data.Map as DM (Map, fromList,elems,lookup, findWithDefault, insert)
import Data.Text (Text, pack)
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)

type Pos = (Int, Int) 

data Cell = Cell { mined :: Bool }

initCell :: Cell
initCell = Cell {mined = False}

data Msg = Pick Pos

w :: Int
w = 1

h :: Int
h = 1

cellSize :: Int
cellSize = 20

cellAttrs :: Cell -> Map Text Text
cellAttrs c = 
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        pack $ "fill:" ++ if mined c then "red" else "black")
             , ( "oncontextmenu", "return false;")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showCell :: forall t m. MonadWidget t m => Dynamic t (Map Pos Cell) -> Pos -> m (Event t Msg)
showCell dBoard pos = do
    let dCell = fmap (findWithDefault initCell pos) dBoard
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (fmap cellAttrs dCell) $ 
                       return ()
    return $ Pick pos <$ leftmost [domEvent Click el , domEvent Contextmenu el]

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 ]

initBoard coords = (fromList (zip coords $ repeat initCell))

updateBoard (Pick pos) oldBoard = insert pos (Cell {mined = True}) oldBoard

showBoard :: forall t m. MonadWidget t m => m ()
showBoard = do 
                rec 
                    let indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 
                    board <- foldDyn updateBoard (initBoard indices) pickEv
                    (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices $ showCell board
                    let pickEv =  leftmost ev
                return ()

main :: IO ()
main = mainWidget showBoard

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

