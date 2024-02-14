import System.IO
import Data.List
import Data.Char

numeraLinhas :: Int -> [String] -> [(Int, String)]
numeraLinhas n = zip [n..]

filtro [] = []
filtro ((a,b): xs) = if length limpo > 2 then (a, limpo) : filtro xs else filtro xs
    where limpo = filter (isAlphaNum) b


numeraPalavra [] = []
numeraPalavra ((a,b):xs) = numeraLinhasaux a (words b) ++ numeraPalavra xs  


numeraLinhasaux n [] = []
numeraLinhasaux n (x:xs) = (n,x) : numeraLinhasaux n xs

inversoDupla [] = []
inversoDupla ((a,b):xs) = (b,a): inversoDupla xs

ordenar l = inversoDupla (sort (inversoDupla l))  

agrup :: [Int] -> [(Int, String)] -> [([Int], String)]
agrup ls [] = []
agrup ls ((a,b):[]) = (ls ++ [a], b) : []
agrup ls ((a,b):xs) = if b == snd hd then agrup (ls ++ [a]) xs else (ls ++ [a], b) : agrup [] xs
    where hd = head xs

eliminaRepitido [] = []
eliminaRepitido ((a,b):xs) = (retiraDuplicados [] a, b) : eliminaRepitido xs

retiraDuplicados ls [] = ls
retiraDuplicados ls (x:xs) = if elem x ls then retiraDuplicados ls xs else retiraDuplicados (ls ++ [x]) xs

main = do putStrLn "Digite o nome do arquivo"
          arquivo <- getLine
          texto <- readFile arquivo
          let result = eliminaRepitido (agrup [] (ordenar (filtro (numeraPalavra (numeraLinhas 1 (lines texto))))))
          print (result)