{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Move = GoLeft | GoRight | GoForward

data Forest work = Exit | Branch work (Forest work) (Forest work) (Forest work) deriving (Show)

move :: Num a => (a, Forest a) -> Move -> (a, Forest a)
move (stamina, Branch work leftForest _ _) GoLeft = (stamina - work, leftForest)
move (stamina, Branch work _ forwardForest _) GoForward = (stamina - work, forwardForest)
move (stamina, Branch work _ _ rightForest) GoRight = (stamina - work, rightForest)
move (stamina, Exit) _ = (stamina, Exit)

testForest :: Forest Integer
testForest = Branch 5 (Branch 6 Exit Exit Exit) (Branch 2 (Branch 10 Exit Exit Exit) Exit (Branch 6 (Branch 4 Exit Exit Exit) Exit Exit)) Exit

showCurrentChoice :: (Ord a, Num a, Show a) => (a, Forest work) -> String
showCurrentChoice (stamina, Exit)
    | stamina > 0 = "YOU'VE FOUND THE EXIT!!"
    | otherwise = "You ran out of stamina and died -.-!"
showCurrentChoice (stamina, Branch {})
    | stamina > 0 = "You have " ++ show stamina ++ " and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
    | otherwise = "You ran out of stamina and died -.-!"

solveForest :: (Ord a, Num a, Show a) => Forest a -> [Move] -> String
solveForest forest mvs = showCurrentChoice $ foldl move (10, forest) mvs
