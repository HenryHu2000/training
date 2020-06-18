{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where
{-
import Kata.TimesComm.Definitions
-}
{- Preloaded code. Maybe helpful for local editing.
-}
-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)


-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS (symmetric eq)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS eq1) (EqlS eq2) = EqlS (transitive eq1 eq2)

-- This will be helpful
plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' EqlZ a = reflexive a
plus' (EqlS eq) a = EqlS (plus' eq a)

plusReverse' :: Natural a -> Equal n m -> Equal (a :+: n) (a :+: m)
plusReverse' a EqlZ = transitive (plusZeroCommutes a NumZ) (symmetric (plusZeroCommutes a NumZ)) 
plusReverse' NumZ (EqlS eq) = (EqlS eq)
plusReverse' (NumS a) (EqlS eq) = EqlS (plusReverse' a (EqlS eq))

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusRefl :: Natural a -> Natural b -> Equal (a :+: b) (a :+: b)
plusRefl NumZ b = reflexive b
plusRefl (NumS a) b = EqlS (plusRefl a b)

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ b c = plusRefl b c
plusAssoc (NumS a) b c = EqlS (plusAssoc a b c)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusZeroCommutes :: Natural a -> Natural Z -> Equal (a :+: Z) (Z :+: a)
plusZeroCommutes NumZ NumZ = EqlZ
plusZeroCommutes (NumS a) NumZ = EqlS (plusZeroCommutes a NumZ)

moveSucc :: Natural a -> Natural b -> Equal (S a :+: b) (a :+: S b)
moveSucc NumZ b = EqlS (reflexive b)
moveSucc (NumS a) b = EqlS (moveSucc a b)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ b = symmetric (plusZeroCommutes b NumZ)
plusComm (NumS a) b = transitive (EqlS (plusComm a b)) (moveSucc b a)


-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = reflexive NumZ
zeroComm (NumS a) = zeroComm a

naturalPlus :: Natural a -> Natural b -> Natural (a :+: b)
naturalPlus NumZ b = b
naturalPlus (NumS a) b = NumS (naturalPlus a b)

naturalTimes :: Natural a -> Natural b -> Natural (a :*: b)
naturalTimes NumZ b = NumZ
naturalTimes (NumS a) b = naturalPlus b (naturalTimes a b)

lemma :: Natural a -> Natural b -> Equal ((a :*: b) :+: a) (a :*: S b)
lemma NumZ b = EqlZ
lemma (NumS a) b = transitive (symmetric (moveSucc (naturalPlus b (naturalTimes a b)) a)) (transitive (EqlS (symmetric (plusAssoc b (naturalTimes a b) a))) (plusReverse' (NumS b) (lemma a b)))

{-
test1 :: Natural (S a) -> Natural b -> Equal ((S a :*: b) :+: S a) (S (b :+: (a :*: b)) :+: a)
test1 (NumS a) b = (symmetric (moveSucc (naturalPlus b (naturalTimes a b)) a))

test2 :: Natural (S a) -> Natural b -> Equal (S (b :+: (a :*: b)) :+: a) (S a :*: S b)
test2 (NumS a) b = transitive (EqlS (symmetric (plusAssoc b (naturalTimes a b) a))) (plusReverse' (NumS b) (lemma a b))
-}

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ b = zeroComm b
timesComm (NumS a) b = transitive (transitive (plusComm b (naturalTimes a b)) (plus' (timesComm a b) b)) (lemma b a)
