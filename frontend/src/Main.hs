{-# Language RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}


module Main where

import Data.Monoid
import Data.Typeable
import Data.Text (Text, pack, unpack)
import Reflex.Dom
import System.Random
import Data.Map (Map, fromList)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)

type Point = (Double,Double)
type Vector = (Double,Double)

width  = 400
height = 300

svgNamespace = Just "http://www.w3.org/2000/svg"

updateFrequency :: NominalDiffTime
updateFrequency = 0.3

svgAttrs = fromList [ ( "viewBox" , pack (
                                        show (-width / 2) 
                                     ++ " " ++ show (-height / 2) 
                                     ++ " " ++ show width 
                                     ++ " " ++ show height) )
                    , ( "width" ,    (pack . show) width)
                    , ( "height" ,   (pack . show) height)
                    , ( "style", "border: 1px solid black")
                    ]



--showCircle :: MonadWidget t m => ((String,Double), (Double,Double)) -> m ()
--showCircle ::
--  (Show t1, Show t2, Show t3, PostBuild t m, DomBuilder t m) =>
--  (t3, t2, t1) -> m ()

--showCircle ::
--  (PostBuild t (DynamicWriterT t w m), DomBuilder t (DynamicWriterT t w m)) => (Text, Text) -> m ()
  
showCircle ((x, y),(r,color)) = do
  let circleAttrs = fromList [ ( "cx", x)
                             , ( "cy", y)
                             , ( "r",  r)
                             , ( "style",  pack $ "fill:" ++ unpack color) ] 

  elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()

  return ()

updateFlips flips =
  flips ++ "T"

coinFlipper :: (MonadWidget t m, RandomGen g) => g -> m ()
coinFlipper g = 
  do
    rec
      el "div" $ text "Sequence of Coin Flips: "
      bFlip <- button "Flip Coin"
      
      --let flips = "H"
      
      flipDyn <- foldDyn (\_ flips ->
                             let (r::Int,g') = randomR (0::Int, 2::Int) g
                             in
                               case r of
                                 0 -> flips ++ "T"
                                 1 -> flips ++ "H"
                                 _ -> flips ++ "X"
                         ) "H" bFlip
      el "div" $ dynText $ fmap pack flipDyn
  --    text $ pack flips;
    return ()
      
     



main :: IO ()
main =
  do
    g <- getStdGen
    
    mainWidget $ el "div" $ do
  
      rec
        let xStr = value tix
            yStr = value tiy
            rStr = value tir
            cStr = value tic
            xy = zipDynWith (,) xStr yStr
            rc = zipDynWith (,) rStr cStr
            deltaStr = fmap (\(dx,dy) -> (pack (show dx), pack (show dy))) deltas
            values = zipDynWith (,) deltaStr rc
            ourCircle = fmap showCircle values
            
        

        el "p" $ text "Haskweb Frontend (V6), type something in the textbox..."
    
        tickEvent <- tickLossy updateFrequency =<< liftIO getCurrentTime
        deltas <- foldDyn (\d -> \(x,y) -> (x+1,y+1)) (20,15) tickEvent
        
        coinFlipper g
        tix <- textInput $ def { _textInputConfig_initialValue = "50" }
    
        tiy <- textInput $ def { _textInputConfig_initialValue = "40" }
        tir <- textInput $ def { _textInputConfig_initialValue = "10" }
        tic <- textInput $ def { _textInputConfig_initialValue = "Red" }
    --
        el "div" $ dynText $ xStr
        el "div" $ dynText $ fmap (pack . show) deltas
    --
    {-
This bTag stuff grabs the value of the X input field and then turns
it into a dyn to display when the button is clicked.
    -}
        bTag <- holdDyn "" $ tag (current (value tix)) b1
        el "div" $ dynText bTag


        b1 <- button "Push me"
    --bEv <- widgetHold (fmap (read . unpack) xStr,fmap (read . unpack) yStr)  b1
    
    {-
      Below: A little experiment to see how one goes
      about modifying values and then putting them
      back in the dynamic monad.
    -}
    --bEv <- b1
        el "div" $ dynText $ fmap (pack . (++ "FOO") . unpack) $ yStr

    --el "div" $ dynText $ fmap (pack . (++ "XXX") . unpack) $ values
    
    --x <- liftM unpack xStr
    --y <- fmap unpack yStr
        elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ dyn ourCircle
        el "br" $ return ()
  
--    return()
      return ()
    --dtx <- dynText $ _textInput_value tix
  
  --  dty <-
  {-
    el "div" $ dynText $ _textInput_value tix
    el "div" $ dynText $ _textInput_value tiy
    --  (cnvs, _) <- elAttr' "canvas" ("width" =: "600" <> "height" =: "400") blank
    el "div" $ dynText $ fmap (pack . show) $ _textInput_hasFocus tix
    
  
    let kpe = fmap (pack . show) $ _textInput_keypress tix
    kpd <- holdDyn "None" kpe 
    dynText kpd;
    
    return ()
    -}    

      
  
--stringToCircle = mapM showCircle
                 
graphicArea :: (MonadWidget t m) => m a -> m a
graphicArea = el "svg" 
