import qualified Data.Map as M

data  LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

-- Two ways to fail:
-- 1. the locker is taken, in which case we refuse to reveal its code
-- 2. the locker does not exist
-- In case of failure, it will explain why
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockers = case M.lookup lockerNumber lockers of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state == Taken
                          then Left $ "Locker " ++ show lockerNumber ++ " is taken!"
                          else Right code

lockers :: LockerMap
lockers = M.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]
