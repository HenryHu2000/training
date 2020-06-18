module ISO where

import Data.Void
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.
import Data.Bifunctor
import Data.Maybe
import Data.Either

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (bimap ab cd, bimap ba dc)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> (cd . ac . ba) , \bd -> (dc . bd . ab))

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab', ba') = ((fromMaybe (fromJust (ab' Nothing))) . ab' . Just, (fromMaybe (fromJust (ba' Nothing))) . ba' . Just)
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (Left . (either (\x -> () : x) (\x -> [])), isoEUFunc)

isoEUFunc :: (Either [()] Void) -> (Either [()] ())
isoEUFunc x
  | null (fromLeft [] x) = Right ()
  | otherwise = Left (tail (fromLeft [] x))
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (\(ab, ba) -> (ba, ab), \(ba, ab) -> (ab, ba))

-- Please copy your code of Isomorphism to here.

-- Sometimes, we can treat a Type as a Number:
-- if a Type t has n distinct value, it's Number is n.
-- This is formally called cardinality.
-- See https://en.wikipedia.org/wiki/Cardinality

-- Void has cardinality of 0 (we will abbreviate it Void is 0).
-- () is 1.
-- Bool is 2.
-- Maybe a is 1 + a.
-- We will be using peano arithmetic so we will write it as S a.
-- https://en.wikipedia.org/wiki/Peano_axioms
-- Either a b is a + b.
-- (a, b) is a * b.
-- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = ((either (Right) (Left)), (either (Right) (Left)))

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (either (either (Left) (Right . Left)) (Right . Right), either (Left . Left) (either (Left . Right) (Right)))

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (\(a, b) -> (b, a) , \(b, a) -> (a, b))

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))

fromLeft' :: (Either a b) -> a
fromLeft' (Left a) = a
fromRight' :: (Either a b) -> b
fromRight' (Right b) = b

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (distFunc1, distFunc2)
  where
    distFunc1 :: (a, (Either b c)) -> (Either (a, b) (a, c))
    distFunc1 (a, x)
      | isLeft x  = Left (a, fromLeft' x)
      | otherwise = Right (a, fromRight' x)
    distFunc2 :: (Either (a, b) (a, c)) -> (a, (Either b c))
    distFunc2 x
      | isLeft x  = (fst (fromLeft' x), (Left . snd . fromLeft') x)
      | otherwise = (fst (fromRight' x), (Right . snd . fromRight') x)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (\x -> if x then Just Nothing else Nothing, maybe False (const True))

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (plusSFunc1, plusSFunc2)
  where
    plusSFunc1 :: (Either (Maybe a) b) -> (Maybe (Either a b))
    plusSFunc1 (Left x)  = maybe Nothing (Just . Left) x
    plusSFunc1 (Right b) = Just (Right b)
    plusSFunc2 Nothing          = Left Nothing
    plusSFunc2 (Just (Left a))  = Left (Just a)
    plusSFunc2 (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (absurd . fst, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (multSFunc1, multSFunc2)
  where
    multSFunc1 (Nothing, b) = Left b
    multSFunc1 (Just a, b)  = Right (a, b)
    multSFunc2 (Left b)       = (Nothing, b)
    multSFunc2 (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (powSFunc1, powSFunc2)
  where
    powSFunc1 funcLeft  = (funcLeft Nothing, funcLeft . Just)
    powSFunc2 (a, funcRight) = powSFunc2'
      where
        powSFunc2' Nothing  = a
        powSFunc2' (Just b) = funcRight b

-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = 
  isoPow one refl `trans` powS `trans` isoProd refl powO `trans` multComm `trans` multSO `trans` refl
-- Here's a trick: 
-- replace undefined with the rhs of the comment on previous line
-- When you're not sure what to fill in for a value,
-- Have it as a _
-- GHC will goes like
-- "Found hole `_' with type: ISO (() -> a) (Maybe b0 -> a0)"
-- So you can immediately see value of what type are needed
-- This process can be repeat indefinitely -
-- For example you might replace `_` with `isoFunc _ _`
-- So GHC hint you on more specific type.
-- This is especially usefull if you have complex type.
-- See https://wiki.haskell.org/GHC/Typed_holes
-- And "stepwise refinement" for more details.
