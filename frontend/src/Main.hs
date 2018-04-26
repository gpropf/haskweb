{-# LANGUAGE OverloadedStrings #-}


module Main where

--import Control.Monad.State
import Data.Monoid
import Data.Typeable
--import Data.IntMap (IntMap, assocs, elems, empty, fromList, size, singleton)
import Data.Text (Text, pack)
--import qualified Data.Text as T
--import Data.Vector (Vector)
--import qualified Data.Vector as Vector
import Reflex.Dom
--import Data.FastMutableIntMap
--import System.Random

-- import Data.Map



main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Haskweb Frontend (V2), type something in the textbox..."
  --ga <- graphicArea $ return ()
  t <- textInput def
  b1 <- button "Push Me!"
  el "div" $ dynText $ _textInput_value t;
  -- let pe = performEvent (putStrLn "I'm happening!!")
  (cnvs, _) <- elAttr' "canvas" ("width" =: "600" <> "height" =: "400") blank
  --return ();
  el "div" $ dynText $ fmap (pack . show) $ _textInput_hasFocus t
  let kpe = fmap (pack . show) $ _textInput_keypress t
  kpd <- holdDyn "None" kpe 
  dynText kpd
  
  


graphicArea :: (MonadWidget t m) => m a -> m a
graphicArea = el "svg" 
