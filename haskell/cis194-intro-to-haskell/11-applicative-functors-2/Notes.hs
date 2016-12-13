{----------------------------------------------------------
  More Applicative Examples
----------------------------------------------------------}

-- Consider an instance of Applicative for lists. There are
-- two possible instances: one that "zips" and one that
-- combines in all possible ways.

-- Instance that does all possible combinations (default).
-- instance Applicative [] where
--   pure a        = [a]
--   [] <*> _      = []
--   (f:fs) <*> as = (map f as) ++ (fs <*> as)

-- Example use: nondeterministic arithmetic.
--   (.+) = liftA2 (+)  -- addition lifted to some Applicative context
--   (.*) = liftA2 (*)
--   n = ([4,5] .* pure 2) .+ [6,1]  -- either 4 or 5, times 2, plus either 6 or 1
