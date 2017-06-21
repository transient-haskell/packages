import           Transient.Base
import           Transient.Indeterminism
import           Transient.EVars
import           Transient.Move.Utils
import           Transient.Move.Internals
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative
import  Data.Monoid

main= keep $ initNode $ inputNodes <|>  do
      local $ option "f" "fire"
      nodes <- local getNodes
      r <- runAt (nodes !! 1) $ return "hello"
      localIO $ print r
