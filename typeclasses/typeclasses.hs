-- | typeclasses are like interfaces. A typeclass defines some behavior
-- | (like comparing for equality, comparing for ordering, enumeration)
-- | and then types that can behave in that way are made instances of that typeclass.
-- | The behavior of typeclasses is achieved by defining functions or just type declarations that we then implement.
-- | So when we say that a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with that type.

-- | implementation of Eq from Prelude


-- | class Eq a where  
-- |    (==) :: a -> a -> Bool  
-- |    (/=) :: a -> a -> Bool  
-- |    x == y = not (x /= y)  
-- |    x /= y = not (x == y)  

data TrafficLight = Red | Yellow | Green

-- | Because == was defined in terms of /= and vice versa in the class declaration, 
-- | we only had to overwrite one of them in the instance declaration. That's called the minimal complete definition for the typeclass
instance Eq TrafficLight where 
    Red    == Red    = True
    Yellow == Yellow = True
    Green  == Green  = True
    _      == _      = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 