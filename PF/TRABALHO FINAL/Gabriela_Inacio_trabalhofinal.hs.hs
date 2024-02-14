
import System.IO
import Data.Char
import Data.List

type Word' = String
type Line  = String
type Doc = String

data Tree = No Word' [Int] Tree Tree | Folha deriving Show


-- Enumera as linhas
numeraLinhas :: Int -> [String] -> [(Int, String)]
numeraLinhas n = zip [n..]

-- Associar a cada ocorrência de uma palavra do documento, o número da linha em que essa palavra ocorre:  
linhaPalavra :: [(Int, String)] -> [(String, Int)]
linhaPalavra = concatMap (\(n, linha) -> zip (words (removePontuacao (map toLower linha))) (repeat n))
  where
    removePontuacao = filter (\c -> isAlpha c || isDigit c || isSpace c)

eliminaPalavrasRepitidas [] = []
eliminaPalavrasRepitidas ((a,n):t) = (a, [n] ++ aux) : eliminaPalavrasRepitidas (drop tamAux t)
    where aux = eliminaPalavrasRepitidasAux a t
          tamAux = length aux

eliminaPalavrasRepitidasAux a [] = []
eliminaPalavrasRepitidasAux a ((b,n):t) = if a == b then n : eliminaPalavrasRepitidasAux a t else []

eliminaNumeroRepitido [] = []
eliminaNumeroRepitido ((f,s):t) = (f, retiraDuplicados s) : eliminaNumeroRepitido t
retiraDuplicados [] = []
retiraDuplicados (h:[]) = [h]
retiraDuplicados (h:t) = if h == head t then retiraDuplicados t else h : retiraDuplicados t

insereOrd n [] = [n]
insereOrd n (x:xs) | n == x = (x:xs) 
                   | n < x = n:(x:xs)
                   | otherwise = x: insereOrd n xs 

ins w l Folha = No w [l] Folha Folha
ins w l (No w' ls esq dir) | w == w' = No w' (insereOrd l ls) esq dir 
                           | w < w' = No w' ls (ins w l esq) dir
                           | otherwise = No w' ls esq (ins w l dir)

mIndexTree ls = makeIndex ls Folha

makeIndex [] arv = arv  
makeIndex ((palavra, linha):xs) arv = makeIndex xs (ins palavra linha arv)  


main :: IO ()
main = do
  putStrLn "Digite o nome do arquivo txt para ser lido."
  hFlush stdout
  arquivo <- getLine
  texto <- readFile arquivo
  putStrLn "Índice de palavras:"
  print (mIndexTree (linhaPalavra (numeraLinhas 1 (lines texto))))
  putStrLn "\n"




