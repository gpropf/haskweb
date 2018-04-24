{-# LANGUAGE OverloadedStrings #-}



module Main where

import Reflex.Dom



main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Haskweb Frontend, type something in the textbox..."
  ga <- graphicArea $ return ()
  t <- textInput def
  el "div" $ dynText $ _textInput_value t
  return ()

graphicArea :: MonadWidget t m => m a -> m a
graphicArea = el "svg" 
