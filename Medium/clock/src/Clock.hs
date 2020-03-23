module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock nh nm
  where
    nh = (h + m `div` 60) `mod` 24
    nm = m `mod` 60

toString :: Clock -> String
toString (Clock h m) = fmt h ++ ":" ++ fmt m

fmt :: Int -> String
fmt n =  drop (length xs - 2) xs
  where xs = '0' : show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock h m) = fromHourMin (h+dh) (m+dm)
