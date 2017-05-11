import Data.List

divides x y = y `mod` x == 0

divvy xs n = divvy' xs n []
    where
        divvy' [] n acc = acc
	divvy' xs 1 acc = xs:acc
	divvy' xs n acc = 
	    let (bucket, rest) = partition (n `divides`) xs
	    in divvy' rest (n-1) (bucket:acc)

main = print (map length (divvy [1..1000] 4))
