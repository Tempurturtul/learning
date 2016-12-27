{-# LANGUAGE GeneralizedNewtypeDeriving #-}

type Name = String

data Employee = Employee { empName :: Name, empPhone :: String }
  deriving (Show, Read, Eq)

names  = ["Joe", "Sara", "Mae"]
phones = ["555-5555", "123-456-7890", "555-4321"]

{----------------------------------------------------------
  More Applicative Examples
----------------------------------------------------------}

-- Consider an instance of Applicative for lists. There are
-- two possible instances: one that "zips" and one that
-- combines in all possible ways.

-- Instance that does all possible combinations (default):
--
-- instance Applicative [] where
--   pure a        = [a]
--   [] <*> _      = []
--   (f:fs) <*> as = (map f as) ++ (fs <*> as)

employees1 = Employee <$> names <*> phones

-- Example use: nondeterministic arithmetic.
--   (.+) = liftA2 (+)  -- addition lifted to some Applicative context
--   (.*) = liftA2 (*)
--   n = ([4,5] .* pure 2) .+ [6,1]  -- either 4 or 5, times 2, plus either 6 or 1


-- Instance that does elementwise combining:

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure = ZipList . repeat  -- Because the longer list is truncated, we need
                           -- to ensure that pure is always the longer list.
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

employees2 = getZipList $ Employee <$> ZipList names <*> ZipList phones


-- Reader/Environment Applicative:
--
-- instance Functor ((->) e) where
--   fmap = (.)
--
-- instance Applicative ((->) e) where
--   pure = const
--   f <*> x = \e -> (f e) (x e)

data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

r = BR "Brent" "XXX-XX-XXX4" 6000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r

{----------------------------------------------------------
  Aside: Levels of Abstraction
----------------------------------------------------------}

-- When working with things like Applicative and Monad, keep in mind that there
-- are multiple levels of abstraction involved.

-- For example, we worked at the "raw Haskell" level when implementing an
-- Applicative instance for Parser. Once that instance was implemented though,
-- we were able to "move up a layer" and program with Parsers using the
-- Applicative interface without thinking about the details.

{----------------------------------------------------------
  The Applicative API
----------------------------------------------------------}

-- Can write generic tools and control structures that work with any type which
-- is an instance of Applicative.

-- Example:

-- pair :: Applicative f => f a -> f b -> f (a,b)
-- -- pair fa fb = (\x y -> (x,y)) <$> fa <*> fb
-- -- pair fa fb = (,) <$> fa <*> fb
-- -- pair fa fb = liftA2 (,) fa fb
-- pair = liftA2 (,)

-- If f =
--   Maybe: Nothing if either is Nothing, Just pair otherwise.
--   []: Cartesian product of two lists.
--   ZipList: Same as standard zip function.
--   IO: Runs two IO actions in sequence and returns a pair of their results.
--   Parser: Runs two parsers in sequence and returns a pair of their results.
