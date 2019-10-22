{-
Julien Dehos. La programmation fonctionnelle - Introduction et application en Haskell à l'usage de l'étudiant et du développeur, Ellipses, 2019.
-}

import Control.Monad (forever)
import System.Exit (exitSuccess)

data Expr = Add Expr Expr 
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Val Double

eval :: Expr -> Double
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Val v) = v

parse :: [String] -> (Expr, [String])
parse [] = error "empty expression"
parse (x:xs) = case x of 
  "+" -> (Add e1 e2, xs2)
  "-" -> (Sub e1 e2, xs2)
  "/" -> (Div e1 e2, xs2)
  "*" -> (Mul e1 e2, xs2)
  _ -> (Val v, xs) 
  where (e1, xs1) = parse xs
        (e2, xs2) = parse xs1
        v = read x :: Double

main = forever $ do
  putStrLn "Enter expression:"
  line <- getLine
  if null line
    then exitSuccess
    else print $ eval $ fst $ parse $ words line

