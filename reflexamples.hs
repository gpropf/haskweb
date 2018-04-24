{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom



main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Haskweb, my collection of Haskell demos using GHCJS and the Reflex framework."
  ga <- graphicArea $ return ()
  t <- textInput def
  el "div" $ dynText $ _textInput_value t
  return ()

graphicArea :: MonadWidget t m => m a -> m a
graphicArea = el "svg" 
