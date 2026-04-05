import Data.Foldable (Foldable(toList))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

-- Ex. 1 (Functor for Sequence)
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append x y) = Append (fmap f x) (fmap f y)

-- Ex. 2 (Foldable for Sequence)
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap f Empty = mempty
    foldMap f (Single x) = f x 
    foldMap f (Append left right) = foldMap f left <> foldMap f right

seqToList :: Sequence a -> [a]
seqToList = toList 

seqLength :: Sequence a -> Int
seqLength = length

-- Ex. 3 (Semigroup and Monoid for Sequence)
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    Empty <> x = x 
    x <> Empty = x 
    x <> y = Append x y     
instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

-- Ex. 4 (Tail recuresion and Sequence Search)
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem a seq = go [seq] 
    where
        go [] = False
        go (Empty : stack) = go stack
        go ((Single x) : stack) = (x == a) || go stack
        go ((Append x y) : stack) = go (x : y : stack)

-- Ex. 5 (Tail recuresion an Sequence Flatten)
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
    where
        go [] acc = reverse acc
        go (Empty : stack) acc = go stack acc
        go ((Single x) : stack) acc = go stack (x:acc)
        go ((Append x y) : stack) acc = go (x:y:stack) acc

-- Ex. 6 (Tail recursion and Reverse Polish Notation)
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens [] 
    where 
        go [] [x] = Just x 
        go (TNum x:rest) stack = go rest (x:stack)
        go (TAdd:rest) (a:b:stack) = go rest ((b+a):stack)
        go (TSub:rest) (a:b:stack) = go rest ((b-a):stack)
        go (TMul:rest) (a:b:stack) = go rest ((b*a):stack)
        go (TDiv:rest) (a:b:stack) 
            | a == 0 = Nothing
            | otherwise = go rest ((b `div` a):stack)
        go _ _ = Nothing

-- Ex. 7. (Expressing functions via foldr and foldl)

-- (a)

myReverse :: [a] -> [a]
myReverse = foldl (\seed x -> x:seed) []

-- (b)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile predicate = foldr(\x seed -> if not (predicate x) then [] else x:seed) []

-- (c)

decimal :: [Int] -> Int
decimal = foldl (\seed x -> 10 * seed + x) 0 

-- Ex 8. (Run-length encoding via folds)
tail' :: [a] -> [a]
tail' (x:xs) = xs

head' :: [a] -> a
head' (x:xs) = x

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr (\x seed -> 
    if null seed 
            then [(x, 1)] 
        else 
            let pair = head' seed
            in  if fst pair == x 
                    then
                        (x, snd pair + 1) : tail' seed
                    else    
                        (x, 1): seed 
                ) []

decode :: [(a, Int)] -> [a]
decode = foldr (\x seed -> replicate (snd x) (fst x) ++ seed) []