{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, fromList, elems)
import Data.Text (Text, pack)

data Cell = Cell Int

data Board = Board { board :: (Map (Int,Int) Cell) }

data Cmd = Pick (Int, Int)

width :: Int
width =  40

height :: Int
height =  30

update :: Cmd -> Board -> Board
update (Pick (x,y)) b = b

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let cells = [((x,y),Cell 1) | x <- [0..width-1], y <- [0..height-1]]
    return $ Board $ fromList cells

cellToAttrs :: (Int, Int) -> Cell -> Map Text Text
cellToAttrs (x,y) cell = do
    fromList [ ( "cx",     pack $ show x)
             , ( "cy",     pack $ show y)
             ] 

showCell :: MonadWidget t m => (Int, Int) -> Dynamic t Cell -> m (Event t Cmd)
showCell pos dCell = do
    let dCellAttrs = fmap (cellToAttrs pos) dCell

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "circle" dCellAttrs $ return ()

    return $ fmap (const $ Pick (0,0)) $ domEvent Mousedown el
    

view :: MonadWidget t m => Dynamic t Board -> m (Event t Cmd)
view dboard = do
    let attrs = constDyn $ 
                    fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

        cellMap = fmap board dboard

    (_, dPopEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey cellMap showCell

    return $ leftmost [ 
                      switch $ (leftmost . elems) <$> current dPopEventMap
                      ]

main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
    rec 
        board <- foldDyn update initialBoard =<< view board
    return ()

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")