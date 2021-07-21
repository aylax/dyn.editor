module Geometry.InputOut where


p = do
    putStrLn "hello"
    line <- getLine
    putStrLn ("Hey" ++ line ++ "!")
    

q = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]



