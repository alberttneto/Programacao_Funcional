--1
{-
> lst1 = [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]

> lst2 = [ x | x <- [50..100], mod x 7 == 3]
[52,59,66,73,80,87,94]=

lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]

lst4=[(x,y)| x <- [1..4], y <- [x..5]]
[(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]
-}

--2
quadrados::Int->Int->[Int]
quadrados x y = [n^2 | n <- [x..y]]


--3
seleciona_impares::[Int]->[Int]
seleciona_impares list = [n | n <- list, odd n]


--4
tabuada::Int->[Int]
tabuada x = [n | n <- [x..(x*10)], mod n x == 0]


--5
bissexto::Int->Bool
bissexto x | ((mod x 400) == 0) = True
 | ((mod x 100) /= 0) && ((mod x 4) == 0) = True
 | otherwise = False

bissextos::[Int]->[Int]
bissextos list = [n | n <- list, bissexto n]


--6
sublista::[[Int]]->[Int]
sublista list = [n | n2 <- list, n <- n2]


--7
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos

bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) | a1 < a2 = True
 | a1 == a2 && m1 < m2 = True
 | a1 == a2 && m1 == m2 && d1 < d2 = True
 | otherwise = False

data_dev::Emprestimo->Data
data_dev (_,_,_,dt,_) = dt

atrasados::Emprestimos->Data->Emprestimos
atrasados emp data_atual = [n | n <- emp, precede (data_dev n) data_atual] 


--8
npares::[Int]->Int
npares [] = 0
npares (a:b) = if even a then 1 + npares b else npares b 


--9
produtorio::[Float]->Float
produtorio [] = 0
produtorio [a] = a
produtorio (a:b) = a * produtorio b


--10
comprime::[[Int]]->[Int]
comprime [] = []
comprime [a] = a
comprime (a:b) = a ++ comprime b


--11
tamanho::Num t => [a] -> t
tamanho [] = 0
tamanho (a:b) = 1 + tamanho b


--12
uniaoNRec::[Int]->[Int]->[Int]
uniaoNRec list1 list2 = [n | x <- [list1,list2], n <- x]


--13
uniaoRec2::[Int]->[Int]->[Int]
uniaoRec2 [] [] = []
uniaoRec2 [] (a2:b2) = a2 : uniaoRec2 [] b2
uniaoRec2 (a1:b1) list2 = a1 : uniaoRec2 b1 list2















