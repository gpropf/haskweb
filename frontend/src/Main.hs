{-# LANGUAGE OverloadedStrings #-}


module Main where

--import Control.Monad.State
import Data.Monoid
--import Data.IntMap (IntMap, assocs, elems, empty, fromList, size, singleton)
import Data.Text (Text)
--import qualified Data.Text as T
--import Data.Vector (Vector)
--import qualified Data.Vector as Vector
import Reflex.Dom
--import Data.FastMutableIntMap
--import System.Random

-- import Data.Map



main :: IO ()
main = mainWidget $ el "span" $ do 
  el "p" $ text "Haskweb Frontend (V2), type something in the textbox..."
  ga <- graphicArea $ return ()
  t <- textInput def
  el "div" $ dynText $ _textInput_value t
  (cnvs, _) <- elAttr' "canvas" ("width" =: "600" <> "height" =: "400") blank
  return ()
  

graphicArea :: (MonadWidget t m) => m a -> m a
graphicArea = el "svg" 
