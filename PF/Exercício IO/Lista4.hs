import System.IO
import Data.Char

numLines (x:xs) = numLines' 1 (x:xs)

numLines' _ [] = []
numLines' n (x:xs) = (n, x) : numLines' (n+1) xs

allNumWords [] = []
allNumWords ((n,l):xs) = numWords n (words l) ++ allNumWords xs

numWords _ [] = []
numWords n (x:xs) = (n, x) : numWords n xs

sortLs [] = []
sortLs ((n,p):xs) = menor ((n,p):xs) : sortLs (removePrimeiro (menor ((n,p):xs)) ((n,p):xs))

-- Auxiliares sortLs --------------------------------------------------------------------
minusculas [] = []
minusculas (x:xs) = toLower x : minusculas xs

menor ((n,p):[]) = (n,p)
menor ((n,p):xs) = if minusculas p < minusculas (snd(menor xs)) then (n,p) else menor xs

removePrimeiro _ [] = []
removePrimeiro (n,p) (x:xs) = if (n,p) == x then xs else x: removePrimeiro (n,p) xs
-----------------------------------------------------------------------------------------

amalgamate [] = []
amalgamate ((n,p):xs) = (agrupaLinhas [] ((n,p):xs), p) : amalgamate (filtraPalavra p ((n,p):xs))

-- Auxiliares amalgamate ----------------------------------------------------------------------
agrupaLinhas ln ((n,p):[]) = n:ln
agrupaLinhas ln ((n,p):xs) = if p == snd (pegaTupla xs) then agrupaLinhas (n:ln) xs else n:ln

filtraPalavra _ [] = []
filtraPalavra a ((n,p):xs) = if p == a then filtraPalavra a xs else (n,p) : filtraPalavra a xs

pegaTupla [] = (0,"")
pegaTupla ((n,p):xs) = (n,p)
-----------------------------------------------------------------------------------------------

shorten [] = []
shorten (((n:ns),p):xs) = (percorreLista (n:ns), p) : shorten xs

-- Auxiliares shorten --------------------------------------------------------
percorreLista [] = []
percorreLista (n:ns) = n : percorreLista (filtraNum n ns)

filtraNum _ [] = []
filtraNum n (x:xs) = if n == x then filtraNum n xs else x : filtraNum n xs
------------------------------------------------------------------------------

indice [] = putStr "\n"
indice (((n:ns),p):xs) = do putStr(pegaNum (n:ns) ++ "\t- " ++ p ++ "\n")
                            indice xs

-- Auxiliares indice -------------------------------------------
pegaNum [] = []
pegaNum (n:ns) = numString n ++ ", " ++ pegaNum ns

numString'::Int->[Char]
numString' 0 = [] 
numString' n = toEnum ((rem n 10) + 48) : numString' (div n 10)

numString n = inverso (numString' n)

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
-----------------------------------------------------------------

main = do putStr "Arquivo: "
          hFlush stdout
          a <- getLine
          txt <- readFile a
          indice (shorten (amalgamate (sortLs (allNumWords (numLines (lines txt))))))
