module Main

data TestData = Con1 (List TestData) String (List (List Int))
    | Con2 ((Int, Int), String)

func : TestData -> Bool -> String
func (Con1 [] x []) False = x ++ "p1"
func (Con1 [] x ([] :: xs)) False = x ++ "p2"
func (Con1 [] x ((y :: ys) :: [])) False = x ++ "p3"
func (Con1 [] x ((y :: ys) :: (z :: xs))) False = x ++ "p4"
func (Con1 ((Con1 zs y ws) :: xs) x ys) False = x ++ "p5"
func (Con1 ((Con2 y) :: xs) x ys) False = x ++ "p6"
func (Con2 ((a,c), b)) False = b ++ "p7"
func (Con1 (x :: xs) y _) True = "super"
func (Con1 [] y _) True = "supertare"
func _ True = "defacto"

t1 : TestData
t1 = Con1 [] "t1" [[]]

ptrStr : Bool -> String -> String
ptrStr False "asd" = reverse "asd"
ptrStr x "2"   = "22"
ptrStr _ _     = reverse "asdauysgdansd"

ptrFlt : Bool -> Double -> String
ptrFlt False 3.14 = reverse "asd"
ptrFlt x 2.11   = "22"
ptrFlt _ _     = reverse "asdauysgdansd"

abcd : Integer -> String
abcd 300040000200 = "WOW"
abcd _      = "NOWOW"

main : IO ()
main = do
     putStrLn $ func t1 False
     putStrLn $ func (Con1 [] "t2" []) False
     putStrLn $ func t1 True
     x <- getLine
     putStrLn $ ptrStr False x
     putStrLn $ ptrFlt False 3.14 
     putStrLn $ ptrFlt False 2.11
     if (2.11 == (cast x)) then putStrLn "hey" else putStrLn "x"
    --  putStrLn $ ptrFlt True (cast x - 0.01)
     y <- getLine
     putStrLn $ abcd (cast y)
     putStrLn $ abcd (cast y)