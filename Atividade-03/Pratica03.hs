--1 
--a)
ou1 :: Bool->Bool->Bool
ou1 True _ = True
ou1 _ True = True
ou1 False False = False

ou2 :: Bool->Bool->Bool
ou2 False False = False
ou2 _ _ = True

ou3 :: Bool->Bool->Bool
ou3 False b = b
ou3 True _ = True

--b)
ou4 :: Bool->Bool->Bool
ou4 x y = if (x == False) && (y == False)  then False 
else True 

ou5 :: Bool->Bool->Bool
ou5 x y = if  ou1 (x == True) (y == True) then True
else False

--2
distAB :: (Float,Float)->(Float,Float)->Float
hipotenusa x y = sqrt ((x ** 2) + (y ** 2))
distAB a b = (hipotenusa (fst(b) - fst(a)) (snd(b) - snd(a)))

--3
{-
ghci> 1:[2,3,4]
[1,2,3,4] //Obs Adiciona inteiro na cabeça da lista
ghci> 'a':['b','c','d']
"abcd" //Obs Adiciona caractere na cabeça da lista
ghci> head [1,2,3]
1 //Obs Retorna a cabeça da lista
ghci> tail [1,2,3]
[2,3] //Obs Retorna o corpo da lista.
ghci> [1,5,2,3]!!1
5 //Obs Retorna elemento na posição 1 da lista
ghci> [1,5,2,3]!!3
3 //Obs Retorna elemento na posição 3 da lista
ghci> elem 2 [1,5,2,3]
True //Obs Verifica se determinado elemento existe na lista
ghci> take 2 [1,5,2,3,7]
[1,5] //Obs Retorna os 2 primeiros elementos da lista
ghci> drop 2 [1,5,2,3,7]
[2,3,7] //Obs Remove os 2 primeiros elementos da lista
ghci> [1,2] ++ [3,4]
[1,2,3,4] //Obs Concatena as duas listas
ghci> [1..10]
[1,2,3,4,5,6,7,8,9,10] //Obs Cria lista com valores de 1 a 10 ordem crescente
ghci> [7,6..3]
[7,6,5,4,3] //Obs Cria lista com valores de 7 a 3 ordem decrescente
ghci> ['b'..'g']
"bcdefg" //Obs Cria lista em ordem alfabetica de 'b' a 'g'
ghci> take 5 [1,3..]
[1,3,5,7,9] //Obs Retorna 5 primeiros elementos da lista n+2
ghci> sum [1..10]
55 //Obs Somatorio dos elementos da lista de 1 a 10
ghci> maximum [1,5,2,3,7]
7 //Obs Maior valor d alista
ghci> minimum [1,5,2,3,7]
1 //Obs menor valor da lista
-}

--4
-- Guarda
fatG::Int->Int
fatG x | x == 0 = 1
 | otherwise = x * fatG(x-1)
-- Casamento
fatC::Int->Int
fatC 0 = 1
fatC x = x * fatC(x-1)


--5
fibo::Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

--6
n_tri::Int->Int
n_tri 0 = 0
n_tri n = n + n_tri(n-1)


--7
--a)
type Par = (Int,Int)

--b)
passo::Par->Par
passo (x,y) = (y,x+y)

--c) d)
fibo2::Int->Par
fibo2 0 = (0,1)
fibo2 n = passo(fibo2 (n-1))


--8
potencia2::Int->Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 n = 2 * potencia2 (n-1)

--9
--a)
prodIntervalo::Int->Int->Int
prodIntervalo m n | m+1 == n = m*n
 | m >= n = 0
 | otherwise = m * prodIntervalo (m+1) (n)

--b)
fatorial::Int->Int
fatorial 0 = 1
fatorial x = prodIntervalo 1 x

--11
resto_div::Int->Int->Int
resto_div m n | n > m = m
 | otherwise = resto_div (m-n) n

div_inteira::Int->Int->Int
div_inteira m n | n > m = 0
 | otherwise = 1 + (div_inteira (m-n) n)


--12
-- Guardas
mdcG::Int->Int->Int
mdcG m n | n == 0 = m
 | otherwise = mdcG n (mod m n) 
-- Casamento
mdcC::Int->Int->Int
mdcC m 0 = m
mdcC m n = mdcC n (mod m n)


--13
--Guardas
binomialG::(Int,Int)->Int
binomialG (n,k) 
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialG (n-1, k) + binomialG (n-1,k-1)

--Casamento
binomialC::(Int,Int)->Int
binomialC (n,0) = 1
binomialC (n,k) = if (n == k) then 1
else binomialC (n-1, k) + binomialC (n-1,k-1)


--14
{-
a)
ghci> [5,4..1]
[5,4,3,2,1]

b)
ghci> take 3 ['a','c'..'z']
"ace"

c) 
ghci> [1,4..16] 
[1,4,7,10,13,16]

d)
ghci> let list1 [1,(-2)..(-11)]
ghci> let list2 [1,5..17]
ghci> zip list1 list2
[(1,1),(-2,5),(-5,9),(-8,13),(-11,17)]
-}


--15
--a)
list1::Int->Int->[Int]
list1 a b = [a..b]

--b)
list2::Int->Int->[Int]
list2 a b | a == b = [] 
 | (mod a 2 == 0) = [a, a+2..b] 
 | otherwise =  [a+1, a+3..b]






