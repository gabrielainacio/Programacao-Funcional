
import System.IO
import Data.Char
import Data.List

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

main :: IO ()
main = do
  putStrLn "Digite o nome do arquivo txt para ser lido."
  hFlush stdout
  arquivo <- getLine
  texto <- readFile arquivo
  putStrLn "Índice de palavras:"
  print (eliminaPalavrasRepitidas (linhaPalavra (numeraLinhas 1 (lines texto))))
  putStrLn "\n"




