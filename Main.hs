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

data Msg = LeftPick Pos | RightPick Pos 

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

getColor :: Cell -> String
getColor (Cell _ exposed _) = if exposed then "#909090" else "#AAAAAA"

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

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

mouseEv :: Reflex t => Pos -> Cell -> El t -> [Event t Msg]
mouseEv pos c el = 
    let r_rEv = RightPick pos <$ domEvent Contextmenu el
        l_rEv = LeftPick  pos <$ domEvent Click       el
    in [l_rEv, r_rEv]


showSquare :: MonadWidget t m => Pos -> Cell -> m [Event t Msg]
showSquare pos c = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()
    return $ mouseEv pos c rEl

showWithoutText :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showWithoutText board pos c = do
                  rEv <- showSquare pos c
                  return (leftmost rEv ,c)

showCell :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showCell board pos c@(Cell mined _ _) = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ showWithoutText board pos c 

adjacents :: Pos -> [Pos]
adjacents (x,y) = 
    [(xx,yy) | xx <- [x-1..x+1]
             , yy <- [y-1..y+1]
             , (xx,yy) /= (x,y)
             , xx >= 0, yy >= 0
             , xx < w, yy < h]

mineCount :: Board -> Pos -> Int
mineCount board pos  = 
    length $ filter mined $ fmap (board !) $ adjacents pos

fromLeftPickM :: Pos -> State Board [(Pos, Maybe Cell)]
fromLeftPickM pos = 
    state $
        \board ->
            let indices = adjacents pos
                count = length $ filter mined $ fmap (board !) indices
                c = board ! pos
                
                updatedCell = if flagged c -- can't expose a flagged cell.
                              then c
                              else c {exposed=True} 

                updatedBoard = insert pos updatedCell board 

                checkList = (if exposed c || flagged c || mined c || count /= 0 
                             then [] 
                             else indices 
                             )

                neighborUpdater = mapM fromLeftPickM checkList
                (updatedNeighbors, updatedNeighborsBoard) = runState neighborUpdater updatedBoard
            in ((pos, Just updatedCell) : concat updatedNeighbors, updatedNeighborsBoard)

fromPick :: Board -> Msg -> [(Pos, Maybe Cell)]
fromPick board (LeftPick p) = 
    let (nc,_) = runState (fromLeftPickM p) board
    in nc

fromPick board (RightPick pos ) = 
    let c = board ! pos
    in if exposed c
       then [] -- can't flag a cell that's already exposed.
       else [(pos, Just c {flagged=not $ flagged c})]

reactToPick :: (Board,Msg) -> Map Pos (Maybe Cell)
reactToPick (b,c) = fromList $ fromPick b c

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
        let pick = switch $ (leftmost . elems) <$> current ev
            pickWithCells = attachPromptlyDynWith (,) cm pick
            updateEv = fmap reactToPick pickWithCells
            eventAndCellMap = listHoldWithKey initial advanceEvent (showCell initial)
            eventMap = fmap (fmap (fmap fst)) eventAndCellMap
            cellMap = fmap (fmap (fmap snd)) eventAndCellMap
        cm <- cellMap 
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
    return ()

main :: IO ()
main = mainWidget showBoard

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
