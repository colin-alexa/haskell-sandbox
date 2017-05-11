data Sorted a = Sorted Ordering [a]
getList (Sorted _ xs) = xs


-- friendly interface for insertion sort
insertSort :: (Ord a) => Ordering -> [a] -> [a]

insertSort _ [] = []
insertSort o xs = insertSort' (Sorted o []) xs

-- | recursive insertion sort, by inserting one element at
-- a time from the unsorted tail into the sorted head of the list
insertSort' :: (Ord a) => Sorted a -> [a] -> [a]

insertSort' (Sorted _ sl) [] = sl
insertSort' sl (x:xs)        = insertSort' (insert x sl) xs

-- add an element to a sorted list
insert :: (Ord a) => a -> Sorted a -> Sorted a

insert y (Sorted o [])     = Sorted o [y]
insert y (Sorted o (z:[]))
	| y `compare` z == o = (Sorted o [y,z])
	| otherwise          = (Sorted o [z,y])
insert y (Sorted o list@(x:z:zs))
	| y `compare` x == o = (Sorted o (y:list))
	| y `compare` z == o = (Sorted o (x:y:z:zs))
	| otherwise          = 
		let rest = getList $ insert y (Sorted o (z:zs))
		in (Sorted o (x:rest))

-- MAIN
main = print (insertSort LT [10,9..0])
