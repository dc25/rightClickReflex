{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (Map, fromList)
import Data.Text (Text, pack)

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

rectAttrs :: Bool -> Map Text Text
rectAttrs c = 
    fromList [ ( "width",        "100%")
             , ( "height",       "100%")
             , ( "style",        pack $ "fill:" ++ if c then "red" else "black")
             , ( "oncontextmenu", "return false;")
             ] 

svgAttrs :: Map Text Text
svgAttrs = fromList 
                 [ ("width" , "100")
                 , ("height", "100")
                 ]

update _  = not 

main :: IO ()
main = mainWidget $ do 
                rec 
                    rb <- foldDyn update False ev
                    (_, ev) <- elSvgns "svg" (constDyn svgAttrs) $ do
                        (el, _) <- elSvgns "rect" (fmap rectAttrs rb) $ return ()
                        return $ () <$ leftmost [domEvent Click el , domEvent Contextmenu el]
                return ()
