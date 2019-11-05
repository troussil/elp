main :: IO ()
main =
  putStrLn "Entrez un texte :"
  >> getLine
  >>= putStrLn . reverse
