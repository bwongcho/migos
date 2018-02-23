
module Eval where

data Expr= Express| Statements deriving Show

data Value= Reals Float| Integr Int deriving Show

negateV:: Value-> Value
negateV x= case x of 
               Reals x-> Reals (-x)
               Integr x-> Integr(-x)

roundV:: Value-> Value
roundV x = case x of 
               Reals x-> Reals(round(x))
               Integr x-> Integr(x)

arithOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> Value
arithOp fInt fFloat Val1 Val2= case Val1 of
                                   Integr _-> case Val2 of
                                                  Integr _-> Integr (fInt Val1 Val2)
                                                  Reals _-> Reals (fFloat (fromIntegral(Val1)) Val2)
                                   Reals _-> case Val2 of
                                                  Integr _-> Reals (fFloat Val1 (fromIntegral(Val 2)))
                                                  Reals _-> Reals (fFloat Val1 Val2)

multV :: Value-> Value-> Value
multV x y = arithOp (*) (*) x y

minusV :: Value-> Value-> Value
minusV x y = arithOp (-) (-) x y

plusV :: Value-> Value-> Value
plusV x y = arithOp (+) (+) x y

modV :: Value-> Value-> Value
modV x y = arithOp (*) (*) x y

intDiv :: Value-> Value -> Value
intDiv x y = Integr div (toInteger(floor x)) (toInteger(floor y)))

realDiv :: Value-> Value -> Value
realDiv x y = Reals ((fromIntegral x)/(fromIntegral y))
