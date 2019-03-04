module Main 

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

main : IO ()
main = putStrLn $ show $ even $ sum [1,3,4,5,2]