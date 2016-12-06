{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Control.Monad.Trans (liftIO)
import Data.Map as DM (Map, fromList,elems,lookup, findWithDefault)
import Data.Text (Text, pack)
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)

type Pos = (Int, Int) 

data Msg = Pick Pos

w :: Int
w =  2

h :: Int
h = 2

cellSize :: Int
cellSize = 20

cellAttrs :: Map Text Text
cellAttrs = 
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showCell :: forall t m. MonadWidget t m => Pos -> m (Event t Msg)
showCell pos = do
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (constDyn $ cellAttrs) $ 
                       return ()
    return $ Pick pos <$ domEvent Click el

showCell2 :: MonadWidget t m => Pos -> m (Event t Msg)
showCell2 pos = do
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (constDyn $ cellAttrs) $ 
                       return ()
    return $ Pick pos <$ domEvent Click el

groupAttrs2b :: Pos -> Map Text Text
groupAttrs2b (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show (x) ++ ", " ++ show y ++ ")" 
               )
             ] 

showCell2b :: forall t m. MonadWidget t m => Dynamic t Pos -> m (Event t Msg)
showCell2b dPos = do 
    (el, _) <- elSvgns "g"  (fmap groupAttrs2b dPos) $ 
                   elSvgns "rect" (constDyn $ cellAttrs) $ 
                       return ()
    return never

showCell3 :: forall t m. MonadWidget t m => Dynamic t (DM.Map Pos Pos)-> m (Event t Msg)
showCell3 posMap = do
           let x :: Dynamic t Pos = fmap (DM.findWithDefault (0,0) (0,0)) posMap
           showCell2b x

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 ]

showBoard :: forall t m. MonadWidget t m => m ()
showBoard = do 
           let indices = fromList [((x,y),(x,y)) | x <- [0..w-1], y <- [0..h-1]] 
           (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices showCell
           let pickEv =  leftmost $ elems ev
           dynIndices <- foldDyn (flip const) indices pickEv
           let x:: Dynamic t (Maybe Pos) = fmap (DM.lookup (0,0)) dynIndices
           (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ showCell3 dynIndices
           return ()

main :: IO ()
main = mainWidget showBoard

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
