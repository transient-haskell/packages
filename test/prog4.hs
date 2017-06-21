import           Transient.Base
import           Transient.Indeterminism
import           Transient.EVars
import           Network
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative
import  Data.Monoid

main= keep $ do
     (do option "a" "say hello" ; liftIO $ print "hello") <|>
          (do option "b" "say world" ; liftIO $ print "world")
     liftIO $ print "next"

