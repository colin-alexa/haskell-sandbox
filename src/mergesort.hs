type Comparator a = (a -> a -> Ordering)

uinsort :: Comparator a -> [a] -> [a] -> [a]
uinsort _ [] xs = xs
uinsort _ xs [] = xs
uinsort comp xl@(x:xs) yl@(y:ys)
    | x `comp` y == GT = y:(uinsort comp xl ys)
    | otherwise        = x:(uinsort comp xs yl)

insort :: (Ord a) => [a] -> [a] -> [a]
insort = uinsort compare

umergesort :: Comparator a -> [a] -> [a]
umergesort _ []    = []
umergesort _ [x]   = [x]
umergesort comp xs = uinsort comp sorted_f sorted_b
    where
        half                 = length xs `div` 2
        (front, back)        = (take half xs, drop half xs)
        (sorted_f, sorted_b) = (umergesort comp front, umergesort comp back)

mergesort :: (Ord a) => [a] -> [a]
mergesort = umergesort compare

main = do
    print (mergesort "hello, world!")
