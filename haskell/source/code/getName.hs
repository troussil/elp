getName :: IO (Maybe String)
getName = 
  putStrLn "Name : "
  >> getLine
  >>= (\name -> let maybeName = if null name
                                then Nothing
                                else Just name
                in return maybeName )
  
main :: IO ()
main = getName >>= print
