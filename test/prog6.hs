import           Transient.Base
import           Transient.Indeterminism
import           Transient.EVars
import           Network
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Monoid
import           Control.Concurrent

main= keep $ do
    setState "hello"
    choose [1..10::Int]             -- many threads produced
    x <- getState
    liftIO $ putStrLn x             -- print “hello”
    setState "world"                -- even if the state is changed dowmstream



