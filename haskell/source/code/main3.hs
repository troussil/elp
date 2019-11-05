main :: IO ()
main = do
  putStrLn "Entrez un texte :"
  line <- getLine
  let rline = reverse line
  putStrLn rline
