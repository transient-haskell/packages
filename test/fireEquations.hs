{-# LANGUAGE OverloadedStrings #-}
import           Transient.Internals hiding (input)
import           Transient.Move
import           Transient.EVars
import           GHCJS.HPlay.View
import           Transient.Move.Utils
import           Transient.Indeterminism
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Data.String
import           Prelude hiding (id)
-- import           GHCJS.Types
-- import Data.JSString

main= keep $ do 
 accelTVar <- newEVar
 clockTVar <- newIORef 0
 initNode  $ server <|> display

server= onServer $ local $ clockticks clockTVar <|>
                           accelerometer accelTVar

display= onBrowser $ do
    local $ setRData (0 :: Double,0 :: Double)
    localIO $ forElems_ "body"  $ svg ! id "graphArea" ! width "500" ! height "500" $ noHtml


    local $ render $  inputSubmit  "begin" `fire` OnClick  :: Cloud String

    atRemote $ do
        (t,a) <-  readEVar accelTVar 

        let s =  v * t  + v0
            v =  a * t  + a0
    

        atRemote $ do
                (told,sold) <- local $ getRData  :: Cloud (Double, Double) -- <|>  return (0 :: Double,0 :: Double)
                let ssval= fs $ show s
                    stval= fs $ show t

                localIO $ forElems_ "#graphArea" $
                    line ! atr "x1" (fs $ show told) ! atr "y1" ( fs $ show sold) ! atr "x2" stval 
                    ! atr "y2" ssval ! style "stroke:rgb(255,0,0);stroke-width:2"
                local $ setRData (t,s)
        empty
        return ()
    where
    svg e= nelemNS "http://www.w3.org/2000/svg" "svg" `child` e
    line = nelemNS "http://www.w3.org/2000/svg" "line"
    fs= fromString
    stroke= atr "stroke"
    strokeWidth= atr "strokeWidth"
    fill= atr "fill"

    v0=  0
    a0=  0
   
clockticks :: EVar Double -> TransIO ()
clockticks mv= threads 1 $ do
         x <- choose[1..10]
         liftIO $ threadDelay 1000000
         writeMVar mv x
    
accelerometer :: EVar Double -> TransIO ()
accelerometer ev = threads 1 $ do
         x <- choose[1..10]
         liftIO $ threadDelay 1000000
         writeIORef ev x

-- #ifdef ghcjs_HOST_OS
-- nelemNS :: JSString -> JSString -> Perch
-- nelemNS namespace s= Perch $ \e ->do
--         e' <- newElemNS namespace s
--         addChild e' e
--         return e'

-- newElemNS ns e  = Elem <$> js_documentCreateNodeNS ns e

-- foreign import javascript unsafe "document.createElementNS($1,$2)"
--   js_documentCreateNodeNS :: JSString ->JSString -> IO JSVal

-- #else
-- nelemNS= empty
-- #endif