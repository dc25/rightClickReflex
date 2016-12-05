{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, state, runState)
import Data.Map as DM (Map, fromList, elems, lookup, insert, mapWithKey, (!))
import Data.Text (Text, pack)
import Data.Functor.Misc (dmapToMap, mapWithFunctorToDMap)
import Data.Time.Clock (getCurrentTime)

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 } deriving Show

type Pos = (Int, Int)
type Board = Map Pos Cell

w :: Int
w =  40

h :: Int
h = 80

cellSize :: Int
cellSize = 20

mkCell :: Cell
mkCell = Cell False False False

mkBoard :: Board
mkBoard = 
    let positions = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
        cells = repeat mkCell
    in fromList $ zip positions cells 

cellAttrs :: Map Text Text
cellAttrs = 
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        "fill:red")
             , ("oncontextmenu", "return false;")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showCell :: MonadWidget t m => Board -> Pos -> Cell -> m ()
showCell board pos c@(Cell mined _ _) = do
    elSvgns "g"  (constDyn $ groupAttrs pos) $ 
        elSvgns "rect" (constDyn $ cellAttrs) $ 
            return ()
    return ()

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]

showBoard :: MonadWidget t m => m ()
showBoard = do
    let initial  = mkBoard 
    now <- liftIO getCurrentTime 
    advanceEvent <- fmap mempty <$> tickLossy 1.0 now
    rec 
        let 
            eventMap = listHoldWithKey initial advanceEvent (showCell initial)
        cm <- eventMap
        elSvgns "svg" (constDyn boardAttrs) eventMap
    return ()

main :: IO ()
main = mainWidget showBoard

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
