readY :: IO Bool
readY = do c <- getChar
           return (c == 'y') -- not (c == 'y')
