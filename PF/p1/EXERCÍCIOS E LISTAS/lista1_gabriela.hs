-- questão 1 
ehTriangulo a b c = a < (b+c) && b< (a+c) && c < (b+a)

-- questão 2 
tipoTriangulo a b c = if a==b && b==c then "equilatero" else if a /= b && b==c || a==b && b /= c || a==c && a/= b then "isosceles" else "escaleno"

-- questão 3
f3 l1 l2 l3  = if ehTriangulo l1 l2 l3 then tipoTriangulo l1 l2 l3 else "nao eh triangulo"

-- questão 4
somapares 1 = 0  
somapares n = if mod n 2 == 0 then somapares (n-1) + n else somapares (n-1)

-- questão 5
somapot2m m 0 = m  
somapot2m m n = (2^n) * m + somapot2m m (n - 1)  

-- questão 6
primo n = if n <= 1 then False 
		else primo2 n (n-1)
primo2 n 1 = True 
primo2 n d = if mod n d == 0 
			 then False 
			 else True
			
-- questão 7
seriepi n = adf 1 1 n 

adf d s n = if n > d then 4/d*s + adf (d+2) (s*(-1)) n else 0 