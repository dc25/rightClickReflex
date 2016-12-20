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

data Msg = Pick Pos

cellSize :: Int
cellSize = 20

cellAttrs :: Bool -> Map Text Text
cellAttrs c = 
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        pack $ "fill:" ++ if c then "red" else "black")
             , ( "oncontextmenu", "return false;")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
               )
             ] 

showCell :: forall t m. MonadWidget t m => Dynamic t (Map Pos Bool) -> Pos -> m (Event t Msg)
showCell dBoard pos = do
    let dCell = fmap (findWithDefault False pos) dBoard
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (fmap cellAttrs dCell) $ 
                       return ()
    return $ Pick pos <$ leftmost [domEvent Click el , domEvent Contextmenu el]

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ cellSize)
                 , ("height", pack $ show $ cellSize)
                 , ("style" , "border:solid; margin:8em")
                 ]

initBoard coords = (fromList (zip coords $ repeat False))

updateBoard (Pick pos) oldBoard = insert pos True oldBoard

showBoard :: forall t m. MonadWidget t m => m ()
showBoard = do 
                rec 
                    let indices = [(0,0)]
                    board <- foldDyn updateBoard (initBoard indices) pickEv
                    (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices $ showCell board
                    let pickEv =  leftmost ev
                return ()

main :: IO ()
main = mainWidget showBoard

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

