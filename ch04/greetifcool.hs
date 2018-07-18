module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
     then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool =
          coolness == ""

myAbs x
  | x < 0 = negate x
  | otherwise =  x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))
