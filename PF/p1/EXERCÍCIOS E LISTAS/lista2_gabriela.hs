-- 1) Declare uma função que verifica se um elemento pertence a uma lista; a função deve retornar 
--True se o elemento estiver na lista, e False caso contrário.
--Exemplo: pertence 3 [1, 4, 3, 2] => True

pertence [] = False 
pertence n (x:xs) = if n == x then True else pertence n xs 

--2) Declare uma função que retorne a interseção entre duas listas.
-- Exemplo: intersecao [1, 3, 5, 7, 9] [2, 5, 3, 6, 9] => [3, 5, 9]
intersecao _ [] = []
intersecao [] _ = []
intersecao (x:xs) (cab:cauda) = if pertence x (cab:cauda) then x : (cab:cauda) else intersecao xs (cab:cauda)
