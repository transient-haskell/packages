-- execute all the computation to the end
runCont= closure >>= continuation
-- stop the execution
empty

delimited continuations:

   shift : Delimit the beginning the continaution
   reset: delimit the end and execute it

