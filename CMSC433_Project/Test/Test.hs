f1 :: (Int -> Char -> Bool) -> [Int] -> [Char] -> [Bool]
f1 f ia ca = case ia of 
    i:is -> case ca of 
        c:cs -> [(f i c)]
        _ -> []
    _ -> []

f2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
f2 f ia ca = case ia of 
    Just i -> case ca of 
        Just c -> Just (f i c)
        _ -> Nothing
    _ -> Nothing

f3 :: (a -> b) -> (b -> Bool) -> [a] -> [b]
f3 f1 f2 l = case l of 
    (x:xs) | (f2 (f1 x))-> [f1 x]
    _ -> []

f4 :: Maybe a -> (a -> [b]) -> [(a,b)]
f4 a f = case a of 
    Just x -> case (f x) of 
        (y:ys) -> [(x,y)]
        _ -> []
    Nothing -> []

f5 :: (Eq a, Show a) => a -> [a] -> [a]
f5 a arr = case arr of 
    (x:xs) | (a == x) -> arr
    _ -> []

f6 :: Show a => [a] -> IO String
f6 arr = case arr of 
    (x:xs) | (show x == "a") -> getLine

f7 :: (a,b) -> (a -> b -> c) -> c
f7 t f = case t of
    (a,b) -> f a b

f8 :: a -> Maybe b
f8 a = Nothing

--傻逼玩意
f9 :: ((a,b) -> c) -> c
f9 = undefined

powerset :: [a] -> [[a]]
powerset a = case a of 
    (x:xs) -> (map (x:) (powerset xs)) ++ (powerset xs)
    [] -> [[]]
