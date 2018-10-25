
--definido por el usuario clases de tipo,Eq se utiliza para una comparacion o un diferente
-- Eq es una instancia de tipos
data Frame = Open Int Int
	|Spare Int Int
	|Strike Int Int
	deriving(Eq, Show)

frameToPoints :: Frame ->Int
frameToPoints (Open x y) = x + y
frameToPoints (Spare _ y) = 10 + y
frameToPoints (Strike x y)= 10 + x + y

toFrame :: [Int] ->Maybe [Frame]
toFrame pins = go 1 pins
	where
		go 10 [x, y]
			| x + y < 10 = Just [Open x y]
			| otherwise = Nothing
		go 10[x,y,z]
			| x==10 = Just [Strike y z]
			| x+y==10 = Just [Spare x z]
			| otherwise = Nothing
		go n (x:y:z:ys)
		--con el fmap es un traslador
			| x ==10 =fmap(Strike y z :) $ go (n+1)(y:z:ys)
			| x + y==10=fmap(Spare x z :) $ go (n+1)(z:ys)
			| x +y <10=fmap(Open x y:) $ go (n+1)(z:ys)
			| otherwise= Nothing
		go _ _ =Nothing


bolos :: Maybe [Frame] -> Int
bolos (Just []) = 0
bolos (Just (fr:frms)) = frameToPoints fr + bolos (Just frms)
bolos Nothing = -1

points :: [Int] -> Int
points tiros = bolos $ toFrame tiros

