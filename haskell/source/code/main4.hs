main :: IO ()
main =
  putStrLn "Entrez un texte :"
  >> getLine
  >>= (\line -> let rline = reverse line in putStrLn rline)
