-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

flip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip f a b = f b a


-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

uncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry f (a,b) = f a b


-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).

curry :: ((a, b) -> t) -> a -> b -> t
curry f a b = f (a,b)


-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 

hasUppercase :: String -> Bool
hasUppercase = any (`elem` ['A' .. 'Z'])

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: String -> Int
count team = length $ filter (== team) votes

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

leftBrandCars :: String -> Bool
leftBrandCars brand = snd (head (filter (\x -> fst x == brand) cars)) > 0

anyXCarsLeft :: String -> Bool
anyXCarsLeft x = any (\(_, cars) -> cars > 0) . filter (\(brand, _) -> brand == x) $ cars

