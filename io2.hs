main = do
    c <- getChar
    d <- getChar
    e <- getChar
    if c /= ' '
        then do
            putChar c
            putChar d
            putChar e
            putChar e
            putChar e
            main
        else return ()