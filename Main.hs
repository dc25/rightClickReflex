{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, fromList, elems, lookup)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)

data Cell = Cell { hasBomb :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 }

type Pos = (Int, Int)
type Board = Map Pos Cell

data Cmd =   LeftPick  Pos Cell 
           | RightPick Pos Cell 

width :: Int
width =  25

height :: Int
height =  15

cellSize :: Int
cellSize = 30

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.2) False False

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
    cells <- sequence $ repeat mkCell
    return $ fromList $ zip positions cells 

getColor :: Cell -> String
getColor (Cell hasBomb exposed flagged) = 
    case (hasBomb, exposed, flagged) of
         (      _,       _,    True) -> "red"
         (      _,   False,       _) -> "green"
         (  True ,       _,       _) -> "black"
         (  False,       _,       _) -> "grey"

cellAttrs :: Cell -> Map Text Text
cellAttrs cell = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

    in fromList [ ( "x",            pack $ show placement)
                , ( "y",            pack $ show placement)
                , ( "width",        pack $ show size)
                , ( "height",       pack $ show size)
                , ( "style",        pack $ "fill:" ++ getColor cell)
                , ("oncontextmenu", "return false;")
                ] 

textAttrs :: Map Text Text
textAttrs = 
    fromList [ ( "x",            "0.5")
             , ( "y",            "0.6")
             , ("font-size",     "1.0" )
             , ("fill",          "blue" )
             , ("alignment-baseline", "middle" )
             , ("text-anchor",        "middle" )
             , ("oncontextmenu",      "return false;")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

bombCount :: Board -> Pos -> Int
bombCount board (x,y)  = 
    let localIndices = [(xx,yy) | xx <- [x-1..x+1], yy <- [y-1..y+1], (xx,yy) /= (x,y)]
        localMaybeCells = (fmap (\p -> Data.Map.lookup p board) localIndices)
        localCells = catMaybes localMaybeCells
        localBombs = filter hasBomb localCells
        count = length localBombs
    in count

showCell :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Cmd)
showCell board pos c@(Cell _ True _) = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  let count = bombCount board pos

                  (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()
                  (tEl,_) <- elSvgns "text" (constDyn textAttrs) $ text $ pack $ show count

                  let r_rEv = RightPick pos c <$ domEvent Contextmenu rEl
                      l_rEv = LeftPick  pos c <$ domEvent Click       rEl

                      r_tEv = RightPick pos c <$ domEvent Contextmenu tEl
                      l_tEv = LeftPick  pos c <$ domEvent Click       tEl

                  return $ leftmost [l_rEv, r_rEv, l_tEv, r_tEv]
    return ev

showCell board pos c@(Cell _ False _) = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do

                  (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()

                  let r_rEv = RightPick pos c <$ domEvent Contextmenu rEl
                      l_rEv = LeftPick  pos c <$ domEvent Click       rEl

                  return $ leftmost [l_rEv, r_rEv]
    return ev

fromLeftPick :: Board -> Pos -> Cell -> [(Pos, Maybe Cell)]
fromLeftPick board (x,y) c = 
    let localIndices = [(xx,yy) | xx <- [x-1..x+1], yy <- [y-1..y+1], (xx,yy) /= (x,y)]
        localMaybeCells = (fmap (\p -> Data.Map.lookup p board) localIndices)
        localCells = catMaybes localMaybeCells
        localBombs = filter hasBomb localCells
        count = length localBombs

        setExposed cl = case cl of
                           Just jcl -> Just jcl { exposed=True }
                           Nothing -> Nothing

    in [((x,y), Just c {exposed=True})] ++ if count == 0 then (zip localIndices $ fmap setExposed localMaybeCells) else []

fromPick :: Board -> Cmd -> [(Pos, Maybe Cell)]
fromPick board (LeftPick p c) = fromLeftPick board p c

fromPick _ (RightPick pos c) = [(pos, Just c {flagged=not $ flagged c})]

reactToPick :: Board -> Cmd -> Map Pos (Maybe Cell)
reactToPick b c = fromList $ fromPick b c

boardAttrs = fromList 
                 [ ("width" , pack $ show $ width * cellSize)
                 , ("height", pack $ show $ height * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]
main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            updateEv = fmap (reactToPick initialBoard) pick
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ listHoldWithKey initialBoard updateEv (showCell initialBoard)

    return ()

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
