--questão 1) (\x.\y.(x(\u.\v.u))y) (\a.\b.a) (\c.\d.d)
(\y.(\a.\b.a(\u.\v.u)y)(\c.\d.d)
(\a.\b.a(\u.\v.u))(\c.\d.d)
(\b.(\u.\v.u))(\c.\d.d)
(\u.\v.u) =a (\a.\b.a)

--questão 2)
compDuplas [] = []
compDuplas [(a,b):xs]= (b,a):compDuplas xs

--ex.: compDuplas [(1,2),(7,11), (2,3)] = (2,1) : compDuplas [(7,11), (2,3)]
                                                  compDuplas [(7,11), (2,3)] = (11,7) : compDuplas [(2,3)]
                                                                                        compDuplas [(2,3)] = (3,2) : compDuplas [] => []
       compDuplas [(1,2),(7,11), (2,3)] = [(2,1), (11,7), (3,2)]                                                                                

--questão 3)
zip' [] _ = []
zip' _ [] = [] 
zip'(a:as)(b:bs)= (a,b): zip' as bs 

--a função zip' recebe duas listas, na qual separa elas em cabeça e calda. Posteriormente, junta a cabeça de cada uma das listas em uma dupla, e chama novamente a função, utilizando com parâmetro a calda das --listas. Percorrendo dessa forma, todos os elementos de ambas as listas. 

--ex.: zip'[1,2,3,4] "abc"= (1,'a'): zip' [2,3,4] "bc"
                                     zip' [2,3,4] "bc" = (2,'b') : zip' [3,4] "c"
                                                                   zip' [3,4] "c" = (3,'c'): zip' [4] [] => [] 
       zip'[1,2,3,4] "abc"= [(1,'a'),(2,'b'),(3,'c')]

--questão 4) 
import Data.Char 

semvogal' []=[]
semvogal' (x:xs) |toLower x == 'a' = semvolgal' xs
                 |toLower x == 'e' = semvolgal' xs
                 |toLower x == 'i' = semvolgal' xs
                 |toLower x == 'o' = semvolgal' xs
                 |toLower x == 'u' = semvolgal' xs
                 |otherwise = x:semvogal' xs

--a função toLower é uma função da biblioteca Data.Char, a qual converte as letras em letras minúsculas. Ela é necessária nessa função para que seja possível a verificação da condição imposta pelo "if". 

--ex.: semvogal' "theend" = t : semvogal' "heend" (caiu no otherwise)
       semvogal' "heend" = h : semvogal' "eend" (caiu no otherwise)
       semvogal' "eend" = semvogal' "end" (caiu na segunda condição)
       semvogal' "end" = semvogal' "nd" (caiu na segunda condição)
       semvogal' "nd" = n : semvogal' "d" (caiu no otherwise)
       semvogal' "d" = d : semvogal' [] (caiu no otherwise)
       semvogal' [] = [] (caso de parada)
       semvogal' "theend" = "thnd"

--questão 5) 
maiorArv' a Folha = a 
maiorArv' a (No e esq dir) = maiorArv' e dir
--se a arvore for uma Folha vai ter como retorno o valor de a, sendo este o maior elemento.
--se a arvore for um nó ela chama a função maiorArv' como subarvore esquerda (e) e subarvore direita (dir)

maiorArv (No e esq dir) = maiorArv' e dir













