main :: IO ()
main = do
  putStrLn "Entrez un texte :"
  line <- getLine
  (putStrLn . reverse) line
