module Print3Flipped whnere

myGreeting :: String
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO () 
  putStrLn myGreeting
  putStrLn  secondGreeting
  where secondGreeting =
          (++) hello ((++) " " world)
