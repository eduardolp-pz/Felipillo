import System.Random

-- Función para verificar si un número es primo utilizando el pequeño teorema de Fermat
isPrime :: Integer -> Int -> IO Bool
isPrime n k = do
    a <- randomRIO (2, n - 2)
    return $ all (\x -> expMod a x n == 1 || expMod a x n == n - 1) [0..k-1]

-- Función para calcular (a^b) mod m
expMod :: Integer -> Integer -> Integer -> Integer
expMod a b m
    | b == 0 = 1
    | b `mod` 2 == 0 = (expMod a (b `div` 2) m) ^ 2 `mod` m
    | otherwise = (a * expMod a (b - 1) m) `mod` m

-- Generar un número primo de 2048 bits
generatePrime :: IO Integer
generatePrime = do
    let bits = 2048
    candidate <- randomRIO (2^(bits-1), 2^bits - 1)
    isPrime' <- isPrime candidate 10
    if isPrime' then return candidate else generatePrime

main :: IO ()
main = do
    prime1 <- generatePrime
    prime2 <- generatePrime
    putStrLn $ "Primo mayor: " ++ show prime1
    putStrLn $ "Primo menor: " ++ show prime2