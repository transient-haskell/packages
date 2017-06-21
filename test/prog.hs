{-# LANGUAGE ScopedTypeVariables #-}
import           Transient.Internals
import           Transient.Indeterminism
import           Transient.EVars
import           Transient.Move
import           Transient.Move.Utils
import           Network
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative
import           Control.Exception  hiding (onException)
import  Data.Monoid


main4= keep $ initNode $ inputNodes <|> do
       nodes <- local getNodes
       connect' $ nodes !! 1
       return ()

main= keep $ do       
  setRState (0 :: Int)
  onException $ \(e :: SomeException) ->  liftIO (print e) >>continue
  dat <- oneThread $ async  (return "hello")  <> do
                              abduce
                              topState >>= showThreads
                              r <- getRState
                              if r== (0 :: Int) 
                                then 
                                    do (setRState (1 :: Int)) ; error "world"
                                else return " world"
  liftIO $ print dat
            

main3= (keep' $ do
    -- onException $ \(e :: SomeException) -> do liftIO (putStrLn $ show e ++ " 2222222222") ; continue
    -- async $ print "AAAAA"
    onException $ \(e :: SomeException) -> do liftIO $ putStrLn $ show e ++ " 1111111111"  ; continue
    async $ print "BBBBB"
    liftIO $ getChar
    async $ print "CCCC"
    lstack
    error "ERR1"
    -- return () `onBack`  \s -> liftIO $ putStrLn ("2"++s)
    -- liftIO $ print "222222"
    -- return () `onBack`   \s ->  do liftIO $ putStrLn ("1"++s)  ; forward ""
    -- liftIO $ print "111111"
    -- back  "hello"  !> "invoking back"
    return ())
 `catch` \(e :: SomeException) -> do print e; return Nothing

lstack= do
      Backtrack _ stack <- getData  `onNothing`  backStateOf  (undefined :: SomeException)           
      liftIO $ print $ length stack

main2 = (error "hello" `catch` (\(e:: SomeException) -> print "1111")) `catch` (\(e:: SomeException) -> print "2222")