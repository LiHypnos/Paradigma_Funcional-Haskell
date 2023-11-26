------------------------------------------------------------------- Verifica Ocorrencia (1)
--aparece só uma vez na lista
unicoOcorrimento :: Eq x => x -> [x] -> Bool --unicoOcorrimento valorASerVerificado Lista
unicoOcorrimento _ [] = False
unicoOcorrimento elem lista = 
    case filter (== elem) lista of
        [_] -> True
        _ -> False

------------------------------------------------------------------------ Maiores elementos (7)
maioresElementos :: (Ord a) => Int -> [a] -> [a]
maioresElementos 0 _      = []  -- Caso base: retorna uma lista vazia quando x é 0
maioresElementos _ []     = []  -- Caso base: retorna uma lista vazia quando a lista é vazia
maioresElementos x lista  = take x (interseccao lista (maxElement : maioresElementos (x - 1) (removeElement maxElement lista)))
  where
    maxElement = maximum lista
    removeElement :: (Eq a) => a -> [a] -> [a]
    removeElement _ [] = []
    removeElement e (x:xs)
      | e == x    = xs
      | otherwise = x : removeElement e xs

-------------------------------------------------------------------------- interssecção (13)
-- Função principal de interseção
interseccao :: Eq a => [a] -> [a] -> [a]
interseccao [] _ = []
interseccao (x:xs) ys
  | estaNaLista x ys = x : interseccao xs ys
  | otherwise = interseccao xs ys
  where
    estaNaLista :: Eq a => a -> [a] -> Bool
    estaNaLista _ [] = False
    estaNaLista x (y:ys) = x == y || estaNaLista x ys

-------------------------------------------------------------------------- Mediana (19)
mediana :: (Fractional a, Ord a) => [a] -> a
mediana [] = error "A lista está vazia"
mediana xs
  | odd n = sortedList !! meio
  | otherwise = (sortedList !! meio - 1 + sortedList !! meio) / 2
  where
    sortedList = ordenaLista xs
    n = length sortedList
    meio = n `div` 2
    -- Função auxiliar para ordenar uma lista
    ordenaLista :: Ord a => [a] -> [a]
    ordenaLista [] = []
    ordenaLista (x:xs) = ordenaLista menores ++ [x] ++ ordenaLista maiores
      where
        menores = filter (< x) xs
        maiores = filter (>= x) xs

---------------------------------------------------------------------------- média (25)
media :: Fractional a => [a] -> a
media [] = error "A lista está vazia"
media xs = sum xs / fromIntegral (length xs)

--------------------------------------------------------------------------------- Primo (31)
-- Função principal para verificar se um número é primo
ePrimo :: Integral a => a -> Bool
ePrimo n
  | n <= 1 = False
  | otherwise = not (any (eDivisivelPorAlgo n) [2..isqrt n])
  where
    isqrt = floor . sqrt . fromIntegral
    eDivisivelPorAlgo :: Integral a => a -> a -> Bool -- Função auxiliar para verificar se um número é divisível por algum outro número
    eDivisivelPorAlgo x y = x `mod` y == 0

--------------------------------------------------------------------------------- Quadrado Perfeito sem Square
ehQuadradoPerfeito :: Integer -> Bool
ehQuadradoPerfeito n = n == somaDosImpares 1 0
  where
    somaDosImpares :: Integer -> Integer -> Integer
    somaDosImpares i acumulador
      | acumulador == n = acumulador
      | acumulador > n  = 0
      | otherwise       = somaDosImpares (i + 2) (acumulador + i)