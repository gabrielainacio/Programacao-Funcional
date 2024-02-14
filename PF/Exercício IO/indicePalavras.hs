import System.IO
import Data.Char
import Data.List

-- Enumera as linhas
numeraLinhas n [] = []   
numeraLinhas n (h:t) = (n, h) : numeraLinhas (n+1) t

-- Associa cada palavra a linha em que ela paarece
linhaPalavra [] = []
linhaPalavra ((n,a):t) = linhaPalavraAux n (words a) ++ linhaPalavra t
linhaPalavraAux n [] = []
linhaPalavraAux n (h:t) = (h,n) : linhaPalavraAux n t


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


main = do putStrLn "Digite o nome do arquivo txt para ser lido."
          hFlush stdout
          arquivo <- getLine
          texto <- readFile arquivo
          putStrLn "Indice de palvras:"
          print (eliminaNumeroRepitido (eliminaPalavrasRepitidas (sort (linhaPalavra (numeraLinhas 1 (lines (map toLower texto)))))))
          putStrLn "\n"
          