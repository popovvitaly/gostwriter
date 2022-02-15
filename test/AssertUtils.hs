module AssertUtils where

data AssertResult a b 
  = Failed 
      { actual ::  a
      , expected :: b
      , errorMessage :: String
      } 
  | Success 
  deriving (Show)
  
assertEqualsWithMessage :: Eq v => v -> v -> String -> AssertResult v v  
assertEqualsWithMessage r v m = if r == v then Success else Failed { actual = v, expected = r, errorMessage = m }

assertEquals :: Eq v => v -> v -> AssertResult v v
assertEquals r v = assertEqualsWithMessage r v "Not equals!"