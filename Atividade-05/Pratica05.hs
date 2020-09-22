--1 b)
bissexto::Int->Bool
bissexto x | f1 x = True
 | f2 x = True
 | otherwise = False
 where 
 f1 x = (mod x 400) == 0
 f2 x = (mod x 100) /= 0 && (mod x 4) == 0

 -- a)
valida::(Int,Int,Int)->Bool
valida (d,m,a) | not (qmes m) && d > 0 && d <= 31 && emes m = True
 | qmes m && m /= 2 && d > 0 && d <= 30 && emes m = True
 | (bissexto a) && m == 2 && d > 0 && d <= 29 && emes m = True
 | not (bissexto a) && m == 2 && d > 0 && d <= 28 && emes m = True
 | otherwise = False
 where 
 qmes x = mod x 2 == 0
 emes x = x > 0  && x <= 12
 
-- c)
type Data = (Int,Int,Int)
type Emprestimo = (String, String, Data, Data, String)

e1::Emprestimo
e2::Emprestimo
e3::Emprestimo

e1 = ("1","BCC63",(01,05,2020),(01,07,2020),"encerrado")
e2 = ("2","BCC62",(01,08,2020),(28,08,2020),"aberto")
e3 = ("3","BSI50",(05,08,2020),(20,08,2020),"aberto")

emprestimos::[Emprestimo]
emprestimos = [e1,e2,e3]

precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) | a1 < a2 && val (d1,m1,a1) (d2,m2,a2) = True
 | am a1 a2 m1 m2 && val (d1,m1,a1) (d2,m2,a2)= True
 | amd a1 a2 m1 m2 d1 d2 && val (d1,m1,a1) (d2,m2,a2) = True
 | otherwise = False
 where
 am a1 a2 m1 m2 = a1 == a2 && m1 < m2
 amd a1 a2 m1 m2 d1 d2 = a1 == a2 && m1 == m2 && d1 < d2
 val (d1,m1,a1) (d2,m2,a2) = (valida (d1,m1,a1)) && (valida (d2,m2,a2))

verifEmprestimo::Emprestimo->Data->Bool
verifEmprestimo emp (d,m,a) | precede (dataDev emp) (d,m,a) && head(situacao emp) == 'a' = False
 | otherwise = True
 where 
 dataDev(_,_,_,dt,_) = dt
 situacao(_,_,_,_,sit) = sit


atrasados::[Emprestimo]->Data->[Emprestimo]
atrasados list dat = [n | n <- list, not (verifEmprestimo n dat)]

-- d)
type Par = (Int,Int)

fibo2::Int->Par
fibo2 n | n == 0 = (0,1)
 | otherwise = passo (fibo2 (n-1))
 where
 passo (x,y) = (y, x+y)

-- e)
prodIntervalo::Int->Int->Int
prodIntervalo m n | m == n = m
 | m > n = 0
 | otherwise = m * prodIntervalo (m+1) n 

fatorial::Int->Int
fatorial n = prod n
 where 
 prod x = prodIntervalo 1 x


-------------------------------------------------
--2 b)
bissexto2::Int->Bool
bissexto2 x | let n = x in mod n 400 == 0 || (mod n 100) /= 0 && (mod n 4) == 0 = True
 | otherwise = False

-- a)
valida2::(Int,Int,Int)->Bool
valida2 (d,m,a) | let {x = d; y = m; z = a} in not (qmes y) && x > 0 && x <= 31 && emes y ||
 qmes y && y /= 2 && x > 0 && x <= 30 && emes y ||
 (bissexto z) && y == 2 && x > 0 && x <= 29 && emes y ||
 not (bissexto z) && y == 2 && x > 0 && x <= 28 && emes y = True
 | otherwise = False
 where 
 qmes x = mod x 2 == 0
 emes x = x > 0  && x <= 12

 -- c)
verifEmprestimo2::Emprestimo->Data->Bool
verifEmprestimo2 emp (d,m,a) | let {x=d; y=m; z=a; e=emp; dataDev(_,_,_,dt,_) = dt; situacao(_,_,_,_,sit) = sit} 
 in precede (dataDev e) (x,y,z) && head(situacao e) == 'a' = False
 | otherwise = True


-- d)
fibo3::Int->Par
fibo3 n | n == 0 = (0,1)
 | otherwise = let {x = n; passo (x,y) = (y, x+y)} in passo (fibo2 (n-1))

-- e)
fatorial2::Int->Int
fatorial2 n = let x = n in prodIntervalo 1 x


------------------------------------------
-- 3
{-
1)
2 * 3 + 1 = 7

2) 
5 - 7 = -2

3)
7 - 5 = 2

4)
((\z -> z/2) -y) = -y/2

5)
(\ xy -> x-y) (6/2) 1
(\ xy -> x-y) 3 1
3 - 1 = 2

6)
9 - 4 = 5

7)
(\y -> y)(\y -> y)
(\y -> y) = y

-}

------------------------------------------
-- 4
{-
Prelude> (\x -> x + 3) 5
8

Prelude> (\x -> \y -> x * y + 5) 3 4
17

Prelude> (\(x,y) -> x * y^2) (3,4)
48

Prelude> (\(x,y,_) -> x * y^2) (3,4,2)
48

Prelude> (\xs -> zip xs [1,2,3]) [4,5,6]
[(4,1), (5,2), (6,3)]
-}

-- 5
{-
a)
Prelude> (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w)
5

b)
Prelude> ((f -> (\x -> f (f x)))(\y -> (y * y))) 3
81

c)
Prelude> ((\f -> (\x -> f (f x)))(\y -> (y + y))) 5
20

d)
Prelude> ((\x -> (\y -> x + y) 5)((\y -> y - 3) 7))
9

e)
Prelude> (((\f -> (\x -> f (f (f x)))) (\y -> (y * y))) 2)
256

f)
Prelude> (\x -> \y -> x + ((\x -> x -3) y)) 5 6
8
-}

