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

type Pos = (Int, Int)

w :: Int
w =  40

h :: Int
h = 80

cellSize :: Int
cellSize = 20

mkBoard :: Map Pos ()
mkBoard = 
    fromList $ zip [(x,y) | x <- [0..w-1], y <- [0..h-1]] $ repeat ()

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

showCell :: MonadWidget t m => Pos -> () -> m ()
showCell pos () = do
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

main :: IO ()
main = mainWidget $ do 
           now <- liftIO getCurrentTime 
           advanceEvent <- fmap mempty <$> tickLossy 1.0 now
           rec 
               let 
                   eventMap = listHoldWithKey mkBoard advanceEvent showCell
               cm <- eventMap
               elSvgns "svg" (constDyn boardAttrs) eventMap
           return ()

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
