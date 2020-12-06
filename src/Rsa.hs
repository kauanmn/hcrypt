module Rsa
( criptografar
, descriptografar
, criarChavePrivada
, criarChavePublica
, obterPQ
, listaPrimos ) where 



import Data.Char
import System.IO
import System.Random



-- retorna o texto criptografado codificado em uma lista de inteiros
criptografar :: String -> (Integer, Integer) -> String
criptografar plainText (n, e)  = show plainTextList 
    where plainTextList = map (\x -> moduloExp x e n) $ codificarTextoInt plainText



-- retorna o texto original de um texto criptografado e codificado em uma lista de inteiros
descriptografar :: String -> (Integer,Integer) -> String
descriptografar secretText (n,d) = decodificarIntTexto $ map (\x -> moduloExp x d n ) $ secretTextList
    where secretTextList = read secretText :: [Integer]



-- retorna a chave publica, representada por uma string (n,e)
criarChavePublica :: (Integer, Integer) -> String
criarChavePublica (p, q) = show (n,e)
    where n = p*q
          e = 3



-- retorna a chave privada, representada por uma string (n,d)
criarChavePrivada :: (Integer, Integer) -> String
criarChavePrivada (p,q) = show (n,d) 
    where n = p*q
          d = obterD $ ((p-1)*(q-1))



-------------------------------------------------------------------------------------
-- FUNCOES AUXILIARES --



-- algoritmo de Euclides para achar o maximo divisor comum
obterMDC :: (Integer,Integer) -> (Integer, Integer)
obterMDC (a,b)
    | (b == 0)  = (1,0)
    | otherwise = (t, z)
    where (s,t) = obterMDC(b, a `mod` b)
          z     = s - ( (a `div` b) * t)



-- retorna d
obterD :: Integer -> Integer
obterD phi = if y < 0 then (y + phi) else y
    where (_,y) = obterMDC (phi, e)
          e     = 3



-- retorna uma lista de numeros entre com 512 bits de tamanho
listaPrimos :: StdGen -> [Integer]
listaPrimos gen = filter (primoMillerRabin gen) numAleatorios
    where numAleatorios = randomRs ( (2^511), (2^513) ) gen



-- retorna p
obterP :: [Integer] -> Int -> (Integer, Int)
obterP primos x = if ( atual `mod` e /= 1 ) 
                  then (atual, x)
                  else obterP primos (x + 1)
    where atual = primos !! x
          e     = 3



-- retorna o par (p,q)
obterPQ :: [Integer] -> (Integer, Integer)
obterPQ primos = (p, fst $ obterP primos (pi+1))
    where (p,pi) = obterP primos 0



-- codifica um texto em uma lista de inteiros
codificarTextoInt :: String -> [Integer]
codificarTextoInt texto = map (blocoInt 128 0) $ dividirBloco 64 $ textoBloco texto 



-- decodifica um texto codificado em uma lista de inteiros
decodificarIntTexto :: [Integer] -> String 
decodificarIntTexto texto = unwords textoDividido 
    where textoDividido = map (blocoTexto . intBloco) texto



-- funcao para calcular b^e `mod` x
moduloExp :: (Integral a, Integral b) => a -> b -> a -> a
moduloExp b e x 
    | e == 0 = 1
    | (odd e) = ( b * (moduloExp (k) (e `div` 2) x)) `mod` x 
    | otherwise = (moduloExp (k) (e `div` 2) x)
    where k = (b*b) `mod` x



-- substitui uma lista de caracteres por uma lista dos respectivos valores ASCII
textoBloco :: [Char] -> [Int]
textoBloco texto = map (ord) texto



-- substitui os valores inteiros por seu respectivo caracter ASCII
blocoTexto :: [Int] -> [Char]
blocoTexto texto = map (chr) texto 



-- dividir os valores ASCII em blocos 
dividirBloco :: Int -> [Int] -> [[Int]]
dividirBloco _ [] = []
dividirBloco len bloco = inicioBloco : dividirBloco len caudaBloco
    where inicioBloco = take len bloco
          caudaBloco  = drop len bloco 



-- cria uma base inteira
blocoInt :: Integer -> Integer -> [Int] -> Integer
blocoInt _ _ [] = 0
blocoInt base expoente (x:xs) = (fromIntegral (x) * base^expoente) + (blocoInt base (expoente+1) xs)



-- cria uma lista de digitos inteiros na base 128
intBloco :: Integer -> [Int]
intBloco 0 = []
intBloco n = fromIntegral (n `mod` 128) : intBloco (n `div` 128)



-------------------------------------------------------------------------------------
-- TESTE DE PRIMITIVIDADE DE MILLER-RABIN --
{-

Teste probabilístico da primitividade de um dado número n. 
Se um número n não passar pelo teste, n com certeza não é primo.
Se o número passar no teste, ele é primo, com uma probabilidade P >= 0,75

O teste de Miller-Rabin é o mais utilizado, pois
utilizar o método de fatoração simples é impraticável neste caso.

Em resumo, este algoritmo é o coração da nossa biblioteca, cuja segurança
se baseia na dificuldade de fatorar números primos gigantes.

-}

-- funcao que recebe um StdGen (fonte de aleatoriedade) e um inteiro n, e testa se n é primo
primoMillerRabin :: StdGen -> Integer -> Bool
primoMillerRabin gen n = if n `mod` 2 == 0 
                           then False
                           else resultado 
    where resultado = foldl (\acc x -> (acc == True && x == True)) True rlist
          rlist     = map (iteracaoMillerRabin s n) (listaMillerRabin n 30 gen)
          (_,s)     = formaMillerRabin n

formaMillerRabin :: (Integral a) => a -> (a, a)
formaMillerRabin n = (d, fromIntegral s)
    where listaFatores = iterate (`div` 2) (pred n)
          s            = length $ takeWhile (\x -> x `mod` 2 == 0) $ listaFatores
          d            = listaFatores !! s

iteracaoMillerRabin :: (Integral a, Integral b) => b -> a -> a -> Bool
iteracaoMillerRabin s n x
    | k == 1     = False
    | s == 1     = False 
    | k == (n-1) = True
    | otherwise  = iteracaoMillerRabin (s-1) n k
    where k = moduloExp x 2 n

listaMillerRabin :: Integer -> Int -> StdGen -> [Integer]
listaMillerRabin _ 0 _ = []
listaMillerRabin n k gen  = 
    let (d,s)         = formaMillerRabin n
        (a,newGen)    = randomR (2, (n-2)) gen
        testeRabin    = moduloExp a d n
        lista         = if ( testeRabin == 1 || testeRabin == (n-1) ) 
                        then listaMillerRabin n (k-1) newGen
                        else testeRabin : listaMillerRabin n (k-1) newGen  
    in  lista
