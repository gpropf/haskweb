{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Monoid
import Data.Typeable
import Data.Text (Text, pack, unpack)
import Reflex.Dom
import System.Random
import Data.Map (Map, fromList)


width  = 400
height = 300

svgNamespace = Just "http://www.w3.org/2000/svg"

svgAttrs = fromList [ ( "viewBox" , pack (
                                        show (-width / 2) 
                                     ++ " " ++ show (-height / 2) 
                                     ++ " " ++ show width 
                                     ++ " " ++ show height) )
                    , ( "width" ,    (pack . show) width)
                    , ( "height" ,   (pack . show) height)
                    , ( "style", "border: 1px solid black")
                    ] 


main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Haskweb Frontend (V2), type something in the textbox..."
  tix <- textInput $ def { _textInputConfig_initialValue = "50" }
  tiy <- textInput $ def { _textInputConfig_initialValue = "40" }
  dtx <- dynText $ _textInput_value tix

--  dty <-
  el "div" $ dynText $ _textInput_value tix
  el "div" $ dynText $ _textInput_value tiy
--  (cnvs, _) <- elAttr' "canvas" ("width" =: "600" <> "height" =: "400") blank
  el "div" $ dynText $ fmap (pack . show) $ _textInput_hasFocus tix
  b1 <- button "Push Me!"

  let kpe = fmap (pack . show) $ _textInput_keypress tix
  kpd <- holdDyn "None" kpe 
  dynText kpd
  let xStr = value tix
      yStr = value tiy

--  x <- mapM (read . unpack) xStr
  --y <- mapM (read . unpack) yStr
  --ourCircle <- fmap $ (mapM showCircle) [(("Green", 45), (x,y))]
  elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ blank
  return ()
      
  
--stringToCircle = mapM showCircle
                 
graphicArea :: (MonadWidget t m) => m a -> m a
graphicArea = el "svg" 


showCircle :: MonadWidget t m => ((String,Double), (Double,Double)) -> m ()
showCircle ((color, radius), (x, y)) = do
    let circleAttrs = fromList [ ( "cx", (pack . show) x)
                               , ( "cy", (pack . show) y)
                               , ( "r",  (pack . show) radius)
                               , ( "style",  pack $ "fill:" ++ color) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()

    return ()
