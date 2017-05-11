type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
return a = Logger (a, [])
firstLogger >>= logf = Logger (secondVal, firstLog ++ secondLog)
    where
        (firstVal, firstLog)   = execLogger firstLogger
        secondLogger           = logf firstLogger
        (secondVal, secondLog) = execLogger secondLogger
