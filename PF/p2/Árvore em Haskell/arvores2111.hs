
--definição da arvore
data Arvore = Nulo | No Int Arvore Arvore deriving Show

arvoreBin = No 4 (No 2 Nulo Nulo)(No 10 (No 5 Nulo Nulo)(No 15 Nulo Nulo))

--definicao da função que acesse as folhas da arvore
folhas Nulo = []
folhas (No n Nulo Nulo) = [n]
folhas (No _ esq dir) = folhas esq ++ folhas dir --folhas q n sao terminais

            --no terminal chama "folhas arvoreBin e retorna [2,5,15] "

--acessa todos os nós de uma árvore
todoNos Nulo = []
todosNos (No n esq dir) = n : todosNos esq ++ todosNos dir

--acessar apenas nos internos
nosInternos Nulo = [] 
nosInternos (No _ Nulo Nulo) = [] 
nosInternos (No n esq dir) = n : nosInternos esq ++ nosInternos dir