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

svgAttrs = fromList [ ( pack "viewBox" , pack (
                                        show (-width / 2) 
                                     ++ " " ++ show (-height / 2) 
                                     ++ " " ++ show width 
                                     ++ " " ++ show height) )
                    , ( pack "width" ,    (pack . show) width)
                    , ( pack "height" ,   (pack . show) height)
                    , ( pack "style", pack "border: 1px solid black")
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

last3 flips = reverse $ take 3 $ reverse flips

coinSeq s rs i = let r = rs!!i
                   in
                     case r of
                       0 -> s ++ "T"
                       1 -> s ++ "H"
                       _ -> s ++ "X"

isHighlighted (t1,t2)
  | t1 == t2 = fromList [(pack "style" , pack "background-color:yellowgreen")]
  | otherwise = fromList [(pack "t1" , (pack . show) t1), (pack "t2" , (pack . show) t2)]

coinFlipper :: (MonadWidget t m) => [Int] -> m ()
--coinFlipper :: (Dynamic t m) => [Int] -> m
coinFlipper rs = 
  do
    elAttr "div" ( "class" =: "container") $ do
      rec
        el "div" $ text "Sequence of Coin Flips: "
        bFlip <- button "Flip Coin"
        bCount <- count bFlip
      
    --  flipDyn2 <- fmap coinSeq 
      
      --let flips = "H"
--      let
        --rsDyn = foldDyn (\_ rs -> rs) rs bFlip
        --iDyn = fmap (\_ i -> i + 1) 0 bFlip
        
        let rDyn = zipDynWith (,) flipDyn bCount
            tdynAttrs = fromList [("class" , "normal")]
            fstFlipDyn = fmap (last3 . fst) flipDyn
            coinComparison = zipDynWith (,) fstFlipDyn (constDyn "HTH")
            

        flipDyn <- foldDyn (\_ (flips,bc) ->
                            
                              let --i = fmap (\i -> i) gDyn
                                r = rs!!bc
                              in
                                case r of
                                  0 -> (flips ++ "T", bc + 1)
                                  1 -> (flips ++ "H", bc + 1)
                                  _ -> (flips ++ "X", bc + 1)
                           ) ("H",0) bFlip
 --    
        el "div" $ dynText $ fmap (pack . show) flipDyn
        el "div" $ dynText $ fmap (pack . show . coinSeq "GOO" rs) bCount
        el "table" $ do
          el "tr" $ do
          
            --elDynAttr "td" (isHighlighted flipDyn flipDyn) $ dynText $ fmap (pack . show . last3. fst) flipDyn
            elDynAttr "td" (fmap isHighlighted coinComparison) $ dynText $ fmap (pack . show . last3. fst) flipDyn

          
              --   blank
    --      blank
          --    text $ pack flips;
      return ()
    return ()

      -- {-

textFieldDemo ::  (MonadWidget t m) => m ()
textFieldDemo = do
 
  elAttr "div" ("class" =: "container column") $ mdo
    let xStr = value tix
        yStr = value tiy
        rStr = value tir
        cStr = value tic
        xy = zipDynWith (,) xStr yStr
        rc = zipDynWith (,) rStr cStr
        deltaStr = fmap (\(dx,dy) -> (pack (show dx), pack (show dy))) deltas
        values = zipDynWith (,) deltaStr rc
        ourCircle = fmap showCircle values
    (tix,tiy,tir,tic) <- elAttr "div" ("class" =: "row") $ mdo
      tix <- textInput $ def { _textInputConfig_initialValue = "50" }
      tiy <- textInput $ def { _textInputConfig_initialValue = "40" }
      tir <- textInput $ def { _textInputConfig_initialValue = "10" }
      tic <- textInput $ def { _textInputConfig_initialValue = "Red" }
      return (tix,tiy,tir,tic)
    deltas <- elAttr "div" ("class" =: "row") $ mdo
      elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ dyn ourCircle
      tickEvent <- tickLossy updateFrequency =<< liftIO getCurrentTime
      deltas <- foldDyn (\d -> \(x,y) -> (x+1,y+1)) (20,15) tickEvent
      el "div" $ dynText $ xStr
      el "div" $ dynText $ fmap (pack . show) deltas
      return deltas


    elAttr "div" ("class" =: "row") $ mdo
      {-
This bTag stuff grabs the value of the X input field and then turns
it into a dyn to display when the button is clicked.
-}
      bTag <- holdDyn "" $ tag (current (value tix)) b1
      el "div" $ dynText bTag
      b1 <- button "Push me"
      blank
    
    {-
      Below: A little experiment to see how one goes
      about modifying values and then putting them
      back in the dynamic monad.
    -}

      el "div" $ dynText $ fmap (pack . (++ "FOO") . unpack) $ yStr
      el "div" $ dynText $ fmap (pack . show) $ _textInput_hasFocus tix
      let kpe = fmap (pack . show) $ _textInput_keypress tix
      kpd <- holdDyn "None" kpe 
      dynText kpd;
      
      return ()
    return ()

-- -}




main :: IO ()
main =
  do
    g <- getStdGen
    mainWidget $ el "div" $ do  
      rec
        let -- xStr = value tix
            -- yStr = value tiy
            -- rStr = value tir
            -- cStr = value tic
            -- xy = zipDynWith (,) xStr yStr
            -- rc = zipDynWith (,) rStr cStr
            -- deltaStr = fmap (\(dx,dy) -> (pack (show dx), pack (show dy))) deltas
            -- values = zipDynWith (,) deltaStr rc
--            ourCircle = fmap showCircle values
            rs = randomRs (0::Int,1::Int) g


        el "p" $ text "Haskweb Frontend (V8), type something in the textbox..."
--        tickEvent <- tickLossy updateFrequency =<< liftIO getCurrentTime
--        deltas <- foldDyn (\d -> \(x,y) -> (x+1,y+1)) (20,15) tickEvent

        coinFlipper rs
        textFieldDemo
    
        return ()
      return ()
    return ()

--         tix <- textInput $ def { _textInputConfig_initialValue = "50" }
--         tiy <- textInput $ def { _textInputConfig_initialValue = "40" }
--         tir <- textInput $ def { _textInputConfig_initialValue = "10" }
--         tic <- textInput $ def { _textInputConfig_initialValue = "Red" }
--         elAttr "div" ("class" =: "container") $ do
--           rec
--             el "div" $ dynText $ xStr
--             el "div" $ dynText $ fmap (pack . show) deltas

--     {-
-- This bTag stuff grabs the value of the X input field and then turns
-- it into a dyn to display when the button is clicked.
--     -}
--             bTag <- holdDyn "" $ tag (current (value tix)) b1
--             el "div" $ dynText bTag

--             b1 <- button "Push me"

    
--     {-
--       Below: A little experiment to see how one goes
--       about modifying values and then putting them
--       back in the dynamic monad.
--     -}

--             el "div" $ dynText $ fmap (pack . (++ "FOO") . unpack) $ yStr


            -- el "div" $ dynText $ fmap (pack . show) $ _textInput_hasFocus tix
            -- let kpe = fmap (pack . show) $ _textInput_keypress tix
            -- kpd <- holdDyn "None" kpe 
            -- dynText kpd;
            -- return ()
        
            -- el "br" $ return ()
    --  (cnvs, _) <- elAttr' "canvas" ("width" =: "600" <> "height" =: "400") blank
   
  
--stringToCircle = mapM showCircle
                 
graphicArea :: (MonadWidget t m) => m a -> m a
graphicArea = el "svg" 
