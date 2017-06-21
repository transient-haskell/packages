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
     x <-  threads 3 $ choose[1..10]
     
     th <- liftIO $ myThreadId
     liftIO $ print (x, th)

