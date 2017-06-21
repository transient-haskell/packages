{-# LANGUAGE ScopedTypeVariables #-}
import           Transient.Internals
import           Transient.Indeterminism
import           Transient.EVars
import           Network
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative
import           Control.Exception
import  Data.Monoid
import System.Directory
import System.Process
import Control.Concurrent
import Control.Monad

service= [("service","test suite")
         ,("executable", "test-transient1")
         ,("package","https://github.com/agocorona/transient-universe")]


main= keep $ 
  do
     r<- (do
        liftIO $ print "1"
    
        liftIO $ print "2" 
        setState "hello"
        choose["AA","BB"]
        error "error3"
        return "III")
           `catcht` \(e :: ErrorCall) -> do liftIO (putStr "INSTALL error: " >> print e) ;  return "PP" -- >> choose ["PP","QQ"] 
     liftIO $ print r
     lstack
     liftIO $ print "4"
     error "error1"
     r <- getState
     liftIO $ putStrLn r

lstack= do
      Backtrack _ stack <- getData  `onNothing`  backStateOf  (undefined :: SomeException)           
      liftIO $ print $ length stack

main2= keep $ do
    return () `catcht`  \(e:: SomeException) ->  undefined 
    replicateM 2 ((liftIO $print "hello")   `catcht` \(e:: SomeException) -> liftIO (print e))