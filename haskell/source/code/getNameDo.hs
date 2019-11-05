getName :: IO (Maybe String)
getName = do
  putStrLn "Name : "
  name <- getLine
  let maybeName = if null name then Nothing else Just name
  return maybeName
  
main :: IO ()
main = getName >>= print
