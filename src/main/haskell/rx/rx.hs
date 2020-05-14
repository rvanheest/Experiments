module Main where

import Control.Exception

data Event t = OnNext t | OnError SomeException | OnCompleted deriving Show

type Observer t = Event t -> IO ()
type Observable t = Observer t -> IO ()

subscribe :: Observable t -> Observer t -> IO ()
subscribe obs obv = obs obv

observer :: (t -> IO()) -> (SomeException -> IO()) -> IO() -> Observer t
observer on _ _ (OnNext t) = on t
observer _ oe _ (OnError e) = oe e
observer _ _ oc OnCompleted = oc

create :: (Observer t -> IO ()) -> Observable t
create f = f

from :: [t] -> Observable t
from xs = create $ \obv -> foldr (>>) (return ()) ([(obv $ OnNext x) | x <- xs] ++ [obv $ OnCompleted])

just :: t -> Observable t
just x = from [x]

exception :: SomeException -> Observable t
exception ex = create $ \obv -> obv (OnError ex)

empty :: Observable t
empty = create $ \obv -> obv OnCompleted

never :: Observable t
never = create $ \_ -> return ()

-- tests and examples

observer1 :: Observer Int
observer1 = observer print (print . show) (print "done")

observer2 :: Observer Int
observer2 = observer print (\_ -> return ()) (print "done")

observer3 :: Observer Int
observer3 = observer print (\_ -> return ()) (return ())

example1 :: Observable Int
example1 = from [1, 2]

example2 :: Observable Int
example2 = create $ \obv -> do
   obv $ OnNext 1
   obv $ OnNext 2
   obv $ OnError (error "hello world")

example3 :: Observable t
example3 = exception $ error "hello world"

example4 :: Observable t
example4 = empty

example5 :: Observable t
example5 = never
