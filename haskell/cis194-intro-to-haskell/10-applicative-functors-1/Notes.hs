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
