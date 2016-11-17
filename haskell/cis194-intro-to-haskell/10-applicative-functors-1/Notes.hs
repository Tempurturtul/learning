{----------------------------------------------------------
  Motivation
----------------------------------------------------------}

-- Consider:

type Name = String

data Employee = Employee { name  :: Name
                         , phone :: String }
                deriving Show

-- Prelude> :t Employee
-- Employee :: Name -> String -> Employee

-- What if we have a Maybe Name and a Maybe String?
-- Can we make a Maybe Employee?
--
-- (Name -> String -> Employee) ->
-- (Maybe Name -> Maybe String -> Maybe Employee)

-- What about a list of names and strings?
--
-- (Name -> String -> Employee) ->
-- ([Name] -> [String] -> [Employee])

-- What about (e -> Name) and (e -> String) for some e?
--
-- (Name -> String -> Employee) ->
-- ((e -> Name) -> (e -> String) -> (e -> Employee))

{----------------------------------------------------------
  Generalizing
----------------------------------------------------------}

-- The generalized function looks like this:
-- (a -> b -> c) -> (f a -> f b -> f c)

-- Very similar to fmap:
-- fmap :: (a -> b) -> (f a -> f b)

-- Attempting to rewrite fmap to look like the generalized function
-- above fails however, because we end up trying to apply functions
-- which are themselves in a Functor context to values which are
-- also in a functor context.

-- Stuck:
-- fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
-- fmap2 h fa fb = undefined

-- Because:
--   h  :: a -> b -> c
--   fa :: f a
--   fb :: f b
--   ...
--   h         :: a -> (b -> c)
--   fmap h    :: f a -> f (b -> c)
--   fmap h fa :: f (b -> c)
--     Problem here.

{----------------------------------------------------------
  Applicative
----------------------------------------------------------}

-- Functors for which "contextual application" is possible are called
-- applicative.

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- (<*>) pronounced "ap" short for "apply".

-- Note that the Applicative class requires instances to be instances
-- of Functor as well. (Meaning fmap is always available for use.)

-- Notice:
--   pure  :: a             -> f a
--   fmap  :: (a -> b)      -> f a -> f b
--   fmap2 :: (a -> b -> c) -> f a -> f b -> f c

-- fmap2 using (<*>), defined as liftA2 in the standard library:
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 h fa fb = (h `fmap` fa) <*> fb

-- (<$>) is a synonym for fmap.
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) = fmap

-- liftA2 h fa fb = h <$> fa <*> fb

-- The next step, liftA3:
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc

-- Note: The parantheses above are unnecessary due to the way the
-- precedence and associativity of <$> and <*> are defined.

-- Typically, f <$> x <*> y <*> ... is used directly instead of liftAN,
-- though the liftANs are handy for partial application.

-- Regarding pure:
--   Used when a function is applied to some arguments in the context of
--   a functor, but one of the arguments is not in the context of that
--   functor.

-- Example for pure:
-- liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
-- liftX h fa b fc = h <$> fa <*> pure b <*> fc

{----------------------------------------------------------
  Applicative Laws
----------------------------------------------------------}

-- Only really interesting one:
-- f `fmap` x === pure f <*> x

-- Mapping a function f over a container x should give the same results
-- as injecting the function into the container, then applying it to x
-- with <*>.

{----------------------------------------------------------
  Applicative Examples
----------------------------------------------------------}

-- Example for Maybe:

-- instance Applicative Maybe where
--     pure              = Just
--     Nothing <*> _     = Nothing
--     _ <*> Nothing     = Nothing
--     Just f <*> Just x = Just (f x)

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1  -- Nothing
ex02 = Employee <$> m_name1 <*> m_phone2  -- Nothing
ex03 = Employee <$> m_name2 <*> m_phone1  -- Nothing
ex04 = Employee <$> m_name2 <*> m_phone2  -- Just
