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
updateFrequency = 0.1

svgAttrs = fromList [ ( pack "viewBox" , pack (
                                        show (-width / 2) 
                                     ++ " " ++ show (-height / 2) 
                                     ++ " " ++ show width 
                                     ++ " " ++ show height) )
                    , ( pack "width" ,    (pack . show) width)
                    , ( pack "height" ,   (pack . show) height)
                    , ( pack "style", pack "border: 1px solid black")
                    ]
  
showCircle ((x, y),(r,color)) = do
  let circleAttrs = fromList [ ( "cx", x)
                             , ( "cy", y)
                             , ( "r",  r)
                             , ( "style",  pack $ "fill:" ++ unpack color) ] 
  elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()
  return ()

last3 flips = reverse $ take 3 $ reverse flips

coinSeq s rs i = let r = rs!!i
                   in
                     case r of
                       0 -> s ++ "T"
                       1 -> s ++ "H"
                       _ -> s ++ "X"

isHighlighted (t1,t2)
  | t1 == t2 = fromList [(pack "style" , pack "background-color:yellowgreen")]
  -- This t1,t2 stuff is for debugging, it doesn't do anything.
  | otherwise = fromList [(pack "t1" , (pack . show) t1), (pack "t2" , (pack . show) t2)]

coinFlipper :: (MonadWidget t m) => [Int] -> m ()
coinFlipper rs =
  
  {- This creates a block of controls containing a button to push to cause
 a "coin" to be flipped. It tracks the last three coin flips and shows
 which of the 8 possible sequences of three it represents. This was
 suggested to me by a video on Numberphile about "Penney's Problem"
 which has to do with the probability of getting various 3 flip
 sequences and which are likely to come first. I worked out that this
 is a Markov process in my notebook and drew the state
 diagram. Perhaps it would be fun to show the diagram here at some
 point with the states lighting up as you play. For now it's just the
 table.
-}
  do
    elAttr "div" ( "class" =: "column") $ do
      rec
        el "div" $ text "Sequence of Coin Flips: "
        bFlip <- button "Flip Coin"
        bCount <- count bFlip        
        let rDyn = zipDynWith (,) flipDyn bCount
            tdynAttrs = fromList [("class" , "normal")]
            fstFlipDyn = fmap (last3 . fst) flipDyn
            coinTriplets = [i ++ j ++ k | i <- ["H","T"], j <- ["H","T"], k <- ["H","T"]]
            coinComparisons = map (\t -> zipDynWith (,) fstFlipDyn (constDyn t)) coinTriplets 
        flipDyn <- foldDyn (\_ (flips,bc) ->                            
                              let r = rs!!bc
                              in
                                case r of
                                  0 -> (flips ++ "T", bc + 1)
                                  1 -> (flips ++ "H", bc + 1)
                                  _ -> (flips ++ "X", bc + 1)
                           ) ("H",0) bFlip
        el "div" $ dynText $ fmap (pack . show) flipDyn
        el "hr" blank
        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" $ text "Last 3 Flips"
          mapM (\cc -> el "tr" $
                       elDynAttr "td" (fmap isHighlighted cc) $ dynText $ fmap (pack . show . last3. snd) cc) coinComparisons
        return ()
      return ()
    return ()

textFieldDemo ::  (MonadWidget t m) => m ()
textFieldDemo = do 
  elAttr "div" ("class" =: "container column") $ mdo
    let xStr = value tix
        yStr = value tiy
        rStr = value tir
        cStr = value tic
        xy = zipDynWith (,) xStr yStr
        rc = zipDynWith (,) rStr cStr
        deltaStr = fmap (\(x,y,xv,yv) -> (pack (show x), pack (show y))) deltas
        values = zipDynWith (,) deltaStr rc
        ourCircle = fmap showCircle values
        -- We return the textInput values because they are not visible
        -- in the above let block otherwise, mdo does not seem to be
        -- mutually recursive (outer blocks can't see what's below in inner
        -- blocks).
    (tix,tiy,tir,tic) <- elAttr "div" ("class" =: "row") $ mdo
      tix <- textInput $ def { _textInputConfig_initialValue = "50" }
      tiy <- textInput $ def { _textInputConfig_initialValue = "40" }
      tir <- textInput $ def { _textInputConfig_initialValue = "10" }
      tic <- textInput $ def { _textInputConfig_initialValue = "Red" }
      return (tix,tiy,tir,tic)
    deltas <- elAttr "div" ("class" =: "row") $ mdo
      elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ dyn ourCircle
      tickEvent <- tickLossy updateFrequency =<< liftIO getCurrentTime
      deltas <- foldDyn (\d -> \(x,y,xv,yv) ->
                            if x > width/2 || x < -width/2 then (x-xv,y+yv,-xv,yv)
                            else if y > height/2 || y < -height/2 then (x+xv,y-yv,xv,-yv)
                            else (x+xv,y+yv,xv,yv))
                (20,15,5,5) tickEvent
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

main :: IO ()
main =
  do
    g <- getStdGen
    mainWidget $ el "div" $ do  
      rec
        let rs = randomRs (0::Int,1::Int) g
        el "p" $ text "Haskweb (V9) Some experiments with Reflex-Dom..."
        coinFlipper rs
        textFieldDemo
        return ()
      return ()
    return ()
