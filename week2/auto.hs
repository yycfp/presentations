{-# LANGUAGE Arrows #-}

import Control.Applicative
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)


newtype Func i o = Func (i -> o)

runFunc (Func f) i = f i

double :: Func Int Int
double = Func (*2)

-- runFunc double 5

newtype Auto i o = Auto (i -> (o, Auto i o))

runAuto (Auto f) [] = []
runAuto (Auto f) (x:xs) = let (o, n) = f x in (o : runAuto n xs)

summer :: Auto Int Int
summer = summer' 0
  where summer' x = Auto (\i -> (x+i, summer' (x+i)))

counter :: Auto a Int
counter = counter' 0
  where counter' n = Auto (\_ -> (n+1, counter' (n+1)))

-- runAuto summer [1,2,3,4]
-- runAuto counter [1,0,0,0]

settable :: Auto (Maybe Int) Int
settable = settable' 0
  where settable' x = Auto (\i -> case i of Nothing -> (x, settable' x)
                                            Just n  -> (n, settable' n))

-- runAuto settable [Nothing, Just 3, Nothing, Nothing, Just 5, Nothing]
                             
compose :: Auto a b -> Auto b c -> Auto a c
compose (Auto a2b) (Auto b2c) = 
  Auto (\a -> let (b, na2b) = a2b a
                  (c, nb2c) = b2c b 
               in (c, compose na2b nb2c))

settableSummer = compose settable summer

-- runAuto settableSummer [Nothing, Just 3, Nothing, Nothing, Just 5, Nothing]

convert :: (i -> o) -> Auto i o
convert f = Auto (\i -> (f i, convert f))

-- runAuto (convert (*2)) [1,2,3,4]

instance Category Auto where
  id = convert (\x -> x)
  a . b = compose b a

-- runAuto (summer . settable) [Nothing, Just 3, Nothing, Nothing, Just 5, Nothing]

doTwice :: Category c => c a a -> c a a
doTwice f = f . f

-- doTwice (*2) 5
-- runAuto (doTwice summer) [1,2,3,4]

instance Arrow Auto where
  arr f = convert f
  first (Auto a) = Auto (\(il, ir) -> let (ol, nl) = a il
                                       in ((ol, ir), first nl))
                               
average1 :: Auto Int Double
average1 = average1' 1 0
  where average1' n sum = Auto (\i -> 
          (fromIntegral (i+sum) / fromIntegral n, average1' (n+1) (i+sum)))

average2 :: Auto Int Double
average2 = proc i -> do
  n <- counter -< i
  sum <- summer -< i
  id -< fromIntegral sum / fromIntegral n

average3 :: Auto Int Double
average3 = 
  (counter &&& summer) >>> (arr (\(n, sum) -> fromIntegral sum / fromIntegral n))

-- runAuto average1 [1,1,1,2,3]
-- runAuto average2 [1,1,1,2,3]
-- runAuto average3 [1,1,1,2,3]
