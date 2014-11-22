-- Joe Loser
-- CS4450 HW2
-- 10/11/2014

-- 3 Constructor types: false, true, and unknown
-- #1.
data Bool3 = F | T | U 
  deriving (Eq, Show)

-- #2.
not' :: Bool3 -> Bool3
not' F = T
not' T = F
not' U = U

and' :: Bool3 -> Bool3 -> Bool3
and' F _ = F 
and' T F = F
and' T T = T
and' T U = U
and' U U = U
and' U T = U
and' U F = F

or' :: Bool3 -> Bool3 -> Bool3
or' T _ = T 
or' F F = F
or' F T = T
or' F U = U
or' U T = T
or' U F = U
or' U U = U

xor' :: Bool3 -> Bool3 -> Bool3
-- if one argument is U, then the result is always U
xor' U _ = U 
xor' T F = T
xor' T T = F
xor' T U = U
xor' F F = F
xor' F T = T
xor' F U = U

-- returns T iff both operands are F
nor' :: Bool3 -> Bool3 -> Bool3
nor' x y = not' (or' x y)

nand' :: Bool3 -> Bool3 -> Bool3
nand' x y = not' (and' x y)
-- By definition, it is handled by the three cases below directly though:
--nand' F _ = T
--nand' _ F = T
--nand' _ _ = F

-- #3.
toTernaryL :: (a -> b -> a) -> a -> b -> b -> a
toTernaryL f a b c = f (f a b) c

toTernaryR :: (a -> b -> b) -> a -> a -> b -> b
toTernaryR f a b c = f a (f b c)

-- #4.
equiv1 :: (Bool3 -> Bool3) -> (Bool3 -> Bool3) -> Bool
equiv1 f g = 
  let vs = [T, F, U]
  in [ f x | x <- vs] == [ g x | x <- vs]

equiv2 :: (Bool3->Bool3->Bool3) -> (Bool3->Bool3->Bool3) -> Bool
equiv2 f g =
  let vs = [T, F, U]
  in [ f x y | x <- vs, y <- vs] == [ g x y | x <- vs, y <- vs]

equiv3 :: (Bool3->Bool3->Bool3->Bool3) -> (Bool3->Bool3->Bool3->Bool3) -> Bool
equiv3 f g =
  let vs = [T, F, U]
  in [ f x y z | x <- vs, y <- vs, z <- vs] == [ g x y z | x <- vs, y <- vs, z <- vs]

-- #5.
assoc :: (Bool3 -> Bool3 -> Bool3) -> Bool
assoc f = equiv3 (toTernaryR f) (toTernaryL f)

-- #6.
equiv5 :: (Bool3->Bool3->Bool3->Bool3->Bool3->Bool3) -> (Bool3->Bool3->Bool3->Bool3->Bool3->Bool3) -> Bool
equiv5 f g =
  let vs = [T, F, U]
  in [ f v w x y z | v <- vs, w <-  vs, x <- vs, y <- vs, z <- vs] == [ g v w x y z | v <- vs, w <- vs, x <- vs, y <- vs, z <- vs]

toQuinaryL :: (a -> b -> c -> a) -> a -> b -> c -> b -> c -> a
toQuinaryL f a b c d e = f (f a b c) d e

toQuinaryM :: (a -> b -> c -> b) -> a -> a -> b -> c -> c -> b
toQuinaryM f a b c d e = f a (f b c d) e

toQuinaryR :: (a -> b -> c -> c) -> a -> b -> a -> b -> c -> c
toQuinaryR f a b c d e = f a b (f c d e)

assoc3 :: (Bool3->Bool3->Bool3->Bool3) -> Bool
assoc3 f = equiv5 (toQuinaryL f) (toQuinaryR f) && equiv5 (toQuinaryR f) (toQuinaryM f)
