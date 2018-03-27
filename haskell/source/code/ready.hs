readY :: IO Bool
readY = do c <- getChar
           return (c == 'y') -- not just (c == 'y')
