import Data.Map as Map

data Person = Person {
    firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
} deriving (Show)

-- | type constructors

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vmult :: (Num t) => Vector t ->  t -> Vector t
vmult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

vscmult :: (Num t) => Vector t -> Vector t -> t
vscmult (Vector i j k) (Vector l m n) = i*l + j*m + k*n 

-- | type synonims : e.g [Char] and String are 

-- | a high-school has lockers so that students 
-- | have some place to put their Guns'n'Roses posters.
-- | Each locker has a code combination. When a student wants a
-- | new locker, they tell the locker supervisor which locker
-- | number they want and he gives them the code.
-- | However, if someone is already using that locker, 
-- | he can't tell them the code for the locker and they have
-- | to pick a different one.



data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just(state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is alread taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]


-- | Recursive data type
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
