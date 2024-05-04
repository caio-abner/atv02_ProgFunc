-- Nome: Caio Abner Soares Araujo
-- nUSP: 4822220

import System.Exit (exitFailure)

ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo 2 = True
ehPrimo n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
          | otherwise = True         


proxPrimo :: Int -> Int
proxPrimo n | ehPrimo n = n
            | otherwise = proxPrimo (n + 1)

verificaXY :: Int -> Int -> IO ()
verificaXY x y =
           if x > y
               then exitFailure
               else return ()
               
maiorIntervaloPrimo :: Int -> Int -> Int
maiorIntervaloPrimo x y = go (proxPrimo x) 0
    where
        go atual intervMax
            | atual >= y = intervMax
            | otherwise = do
                let atualAux = atual + 1
                let proximo = proxPrimo atualAux
                if proximo > y
                    then intervMax
                    else do
                        let intervalo = proximo - atual
                        go proximo (max intervalo intervMax)


main :: IO ()
main = do
    x <- readLn
    y <- readLn
    verificaXY x y
    let intervalo = maiorIntervaloPrimo x y
    print intervalo