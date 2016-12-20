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

data Msg = Pick 

cellSize :: Int
cellSize = 100

cellAttrs :: Bool -> Map Text Text
cellAttrs c = 
    fromList [ ( "x",            "5")
             , ( "y",            "5")
             , ( "width",        "90")
             , ( "height",       "90")
             , ( "style",        pack $ "fill:" ++ if c then "red" else "black")
             , ( "oncontextmenu", "return false;")
             ] 

showCell :: forall t m. MonadWidget t m => Dynamic t Bool -> m (Event t Msg)
showCell dCell = do
    (el, _) <- 
                   elSvgns "rect" (fmap cellAttrs dCell) $ 
                       return ()
    return $ Pick <$ leftmost [domEvent Click el , domEvent Contextmenu el]

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , "100")
                 , ("height", "100")
                 ]

updateBoard Pick  = not 

showBoard :: forall t m. MonadWidget t m => m ()
showBoard = do 
                rec 
                    board <- foldDyn updateBoard False  ev
                    (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ showCell board 
                return ()

main :: IO ()
main = mainWidget showBoard

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

