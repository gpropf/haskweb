{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom



main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Reflexamples, my collection of Haskell demos using the Reflex framework."
  ga <- graphicArea $ return ()
  t <- textInput def
  dynText $ _textInput_value t
  return ()

graphicArea :: MonadWidget t m => m a -> m a
graphicArea = el "svg" 
