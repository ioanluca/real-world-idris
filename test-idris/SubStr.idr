module Main 

ok : String -> String
ok s = if length s `mod` 2 == 0 then reverse s else s

main : IO ()
main = putStrLn $ substr 3 2 ( ok "abcdefghjklmnopqrstuvw")