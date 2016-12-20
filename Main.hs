{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (Map)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

size = "300"

rectAttrs :: Bool -> Map Text Text
rectAttrs c =   "width" =:        size
             <> "height" =:       size
             <> "style" =:        (pack $ "fill:" ++ if c then "red" else "black")
             <> "oncontextmenu" =: "return false;"

h1Attrs = ("oncontextmenu" =: "return false;")
-- h1Attrs = mempty

main = mainWidget $ do 
           (h1el, _) <- elAttr' "h1"  h1Attrs $ text "click square below and this text too."
           rec 
               rb <- foldDyn (\e b -> not b) False ev
               (_, ev) <- elSvgns "svg" (constDyn $ "width" =: size <> "height" =: size) $ do
                   (el, _) <- elSvgns "rect" (fmap rectAttrs rb) $ return ()
                   return $ leftmost [ domEvent Click el 
                                     , domEvent Contextmenu el 
                                     , domEvent Click h1el 
                                     , domEvent Contextmenu h1el 
                                     ]
           return ()
