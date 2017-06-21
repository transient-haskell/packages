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
  ev <- newEVar
  r  <- readEVar ev <|>  readEVar ev <|> (mapM_ (writeEVar  ev) [1..10::Int] >> empty)
  liftIO $ print r


