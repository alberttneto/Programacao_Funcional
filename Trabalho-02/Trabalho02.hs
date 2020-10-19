
l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]


--1
--a) OK
selecao1::(Ord a)=>[a]->[a]
selecao1 [] = []
selecao1 xs = [x] ++ selecao1 ([n | n <- xs, not (n == x)])
 where x = foldr1 min xs

--b) OK
insercao::(Ord a)=>[a]->[a]
insercao x = foldr (insereOrd) [] x 

insereOrd::(Ord a)=>a->[a]->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y: (insereOrd x ys)

 --c) OK
quicksort1::(Ord a)=>[a]->[a]
quicksort1 [] = []
quicksort1 (s:xs) = 
 quicksort1 (filter (<s) xs)
 ++ [s] ++
 quicksort1 (filter (>=s) xs)


--2
-- Variação 1 OK

bolha1::(Ord a)=>[a]->[a]
bolha1 [] = []
bolha1 lista = bolhaOrd1 lista (length lista)

bolhaOrd1::(Ord a)=>[a]->Int->[a]
bolhaOrd1 lista 0 = lista
bolhaOrd1 lista n = 
 if (head lista == head (troca1 lista)) then lista 
 else bolhaOrd1 (troca1 lista) (n-1)

troca1::(Ord a)=>[a]->[a]
troca1 [x] = [x]
troca1 (x:y:zs)
 | x > y = y: troca1 (x:zs)
 | otherwise = x: troca1 (y:zs)


-- Variação 2 OK
bolha2::(Ord a)=>[a]->[a]
bolha2 [] = []
bolha2 lista = bolhaOrd2 lista (length lista)

bolhaOrd2::(Ord a)=>[a]->Int->[a]
bolhaOrd2 lista 0 = lista
bolhaOrd2 lista n = bolhaOrd2 ((troca2 (take n lista)) ++ (drop (n) lista)) (n-1)

troca2::(Ord a)=>[a]->[a]
troca2 [x] = [x]
troca2 (x:y:zs)
 | x > y = y: troca2 (x:zs)
 | otherwise = x: troca2 (y:zs)

-- Variação 3 OK
bolha3::(Ord a)=>[a]->[a]
bolha3 [] = []
bolha3 lista = bolhaOrd3 lista (length lista)

bolhaOrd3::(Ord a)=>[a]->Int->[a]
bolhaOrd3 lista 0 = lista
bolhaOrd3 lista n = 
 if (head lista == head (troca3 lista)) then lista 
 else bolhaOrd3 ((troca3 (take n lista)) ++ (drop (n) lista)) (n-1)

troca3::(Ord a)=>[a]->[a]
troca3 [x] = [x]
troca3 (x:y:zs)
 | x > y = y: troca3 (x:zs)
 | otherwise = x: troca3 (y:zs)

-- Variação 1 cont OK

bolhacont1::(Ord a)=>[a]->([a],Int)
bolhacont1 [] = ([],0)
bolhacont1 lista = bolhaOrdcont1 lista (length lista)

bolhaOrdcont1::(Ord a)=>[a]->Int->([a],Int)
bolhaOrdcont1 lista 0 = (lista,0)
bolhaOrdcont1 lista n = if (head lista == head a) then (lista,d+0) 
 else (c,y)
 where
 (a,b) = trocacont1 lista
 (c,d) = bolhaOrdcont1 a (n-1)
 y = b+d


trocacont1::(Ord a)=>[a]->([a],Int)
trocacont1 [x] = ([x],0)
trocacont1 (x:y:zs)
 | x > y = (y:a,d+1)
 | otherwise = (x:c,0)
 where
 (a,b) = trocacont1 (x:zs)
 (c,d) = trocacont1 (y:zs)


-- Variação 2 cont OK
bolhacont2::(Ord a)=>[a]->([a],Int)
bolhacont2 [] = ([],0)
bolhacont2 lista = bolhaOrdcont2 lista (length lista)

bolhaOrdcont2::(Ord a)=>[a]->Int->([a],Int)
bolhaOrdcont2 lista 0 = (lista,0)
bolhaOrdcont2 lista n = (c,y)
 where
 (a,b) = trocacont2 (take n lista)
 (c,d) = bolhaOrdcont2 (a ++ (drop (n) lista)) (n-1)
 y = b+d

trocacont2::(Ord a)=>[a]->([a],Int)
trocacont2 [x] = ([x],0)
trocacont2 (x:y:zs)
 | x > y = (y:a,d+1)
 | otherwise = (x:c,0)
 where
 (a,b) = trocacont2 (x:zs)
 (c,d) = trocacont2 (y:zs)

-- Variação 3 cont OK

bolhacont3::(Ord a)=>[a]->([a],Int)
bolhacont3 [] = ([],0)
bolhacont3 lista = bolhaOrdcont3 lista (length lista)

bolhaOrdcont3::(Ord a)=>[a]->Int->([a],Int)
bolhaOrdcont3 lista 0 = (lista,0)
bolhaOrdcont3 lista n = 
 if (head lista == head a) then (lista,d+0) 
 else (c,y)
 where
 (a,b) = trocacont3 (take n lista)
 (c,d) = bolhaOrdcont3 (a ++ (drop (n) lista)) (n-1)
 y = b+d

trocacont3::(Ord a)=>[a]->([a],Int)
trocacont3 [x] = ([x],0)
trocacont3 (x:y:zs)
 | x > y = (y:a,d+1)
 | otherwise = (x:c,0)
 where
 (a,b) = trocacont3 (x:zs)
 (c,d) = trocacont3 (y:zs)
 
{-
	O algoritmo v3 é mais eficiente pois ele não percorre a lista n-1 vezes
	de acordo com quantidade de valores que já foram comparados
-}



--3
-- Variação 1 OK
selecaov1::(Ord a)=>[a]->[a]
selecaov1 [] = []
selecaov1 xs = x: selecaov1 (remove x xs)
 where x = minimo xs

remove::(Ord a)=>a->[a]->[a]
remove a [] = []
remove a (x:xs)
 | a==x = xs
 | otherwise = x: (remove a xs)

minimo::(Ord a)=>[a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
 | x <= (minimo xs) = x
 | otherwise = minimo xs

-- Variação 2 OK
selecaov2::[Integer]->[Integer]
selecaov2 [] = []
selecaov2 xs = [menor] ++ selecaov2 lst
 where 
 removeM = remove_menor xs
 menor = snd removeM
 lst = fst removeM

remove_menor::[Integer]->([Integer],Integer)
remove_menor lst = (sem_menor, menor)
 where
 menor = foldr1 min lst 
 sem_menor = [n | n <- lst, not (n == menor)]

-- Variação 2 com contador OK
selecaov22::[Integer]->([Integer],Int)
selecaov22 lst = selecaoCont(lst, 0)

selecaoCont::([Integer],Int)->([Integer],Int)
selecaoCont ([],_) = ([],0)
selecaoCont (xs,cont1) = ([menor removeM] ++  fst (selecaoCont ((lst removeM),cont2 removeM)), cont2 removeM)
 where 
 removeM = remove_menor2 (xs, cont1)
 menor(_,b,_) = b
 lst(a,_,_) = a
 cont2(_,_,c) = c

remove_menor2::([Integer],Int)->([Integer],Integer,Int)
remove_menor2 (lst,cont) = (sem_menor, menor, cont2)
 where
 menor = foldr1 min lst 
 sem_menor = [n | n <- lst, not (n == menor)]
 cont2 = (length sem_menor) + cont


{-
	1 - O numero de comparação é o mesmo entre a função original e a variação2
	2 - o desempenho da variação 2 é melhor que a funação original
	3 - Avariação 2 é melhor devido percorre rmenos vezes a lista.
-}


--4
-- Variação 1
quicksortv1::(Ord a)=>[a]->[a]
quicksortv1 [] = []
quicksortv1 (s:xs) = 
 quicksortv1 (fst (dividev1 s xs))
 ++ [s] ++
 quicksortv1 (snd (dividev1 s xs))

dividev1::(Ord a)=>a->[a]->([a],[a])
dividev1 x lista = (menor,maior)
 where
 menor = [n | n <- lista, n < x]
 maior = [n | n <- lista, n >= x] 

-- Variação 2
quicksortv2::(Ord a)=>[a]->[a]
quicksortv2 [] = []
quicksortv2 lst = 
 quicksortv2 (fst (dividev2 pivo corpo))
 ++ [pivo] ++
 quicksortv1 (snd (dividev2 pivo corpo))
 where
 pivo = if (length lst) <= 3 then head lst
 else mediano (take 3 lst)
 corpo = if (length lst) <= 3 then drop 1 lst
 else [n | n <- lst, n /= pivo]

mediano::(Ord a)=>[a]->a
mediano lst = head [n | n <- lst, n /= minimum lst, n /= maximum lst]

dividev2::(Ord a)=>a->[a]->([a],[a])
dividev2 x lista = (menor,maior)
 where
 menor = [n | n <- lista, n < x]
 maior = [n | n <- lista, n >= x] 

--5
--Função junta ordenando
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

--Funcao divide a lista pela metade
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))

firstHalf  xs = let { n = length xs } in take (div n 2) xs
secondHalf xs = let { n = length xs } in drop (div n 2) xs

--6
-- a)
data Exp a = Val a -- um numero
 | Add (Exp a) (Exp a) -- soma de duas expressoes
 | Sub (Exp a) (Exp a) -- subtração
 | Mult (Exp a) (Exp a) -- Multiplicação
 | Pot (Exp a) (Exp a) -- Potencia
 deriving (Show)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2) 
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

-- b)
expressao1 :: Exp Integer
expressao1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expressao2 :: Exp Integer
expressao2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))


--7
--a)
data Meses = Jan | Fev | Mar | Abr
 deriving (Eq, Show, Enum)

data Hora = PM Int Int 
 | AM Int Int 
 deriving (Eq, Show, Ord)

horasDecorridas::Hora->Int
horasDecorridas (AM x _) = x
horasDecorridas (PM x _) = x + 12

minutosDecorridos::Hora->Int
minutosDecorridos (AM h m) = (h*60)+m
minutosDecorridos (PM h m) = ((h+12)*60)+m 

segundosDecorridos::Hora->Int
segundosDecorridos (AM h m) = ((h*60)+m)*60
segundosDecorridos (PM h m) = (((h+12)*60)+m)*60

--b)
checkahora :: Int -> Bool
checkahora h
  | h >= 0 && h <= 11 = True
  | otherwise = False

checkamins :: Int -> Bool
checkamins m
  | m >= 0 && m <= 59 = True
  | otherwise = False

horapassou :: Hora -> Int
horapassou (AM hora min)
  | checkahora (hora) == True && checkamins (min) == True = hora
  | otherwise = -1
horapassou (PM hora min)
  | checkahora (hora) == True && checkamins (min) == True = 12 + hora
  | otherwise = -1

minutopassou :: Hora -> Int
minutopassou (AM hora min)
  | checkahora (hora) == True && checkamins (min) == True = hora * 60 + min
  | otherwise = -1
minutopassou (PM hora min)
  | checkahora (hora) == True && checkamins (min) == True = ((12 + hora) * 60) + min
  | otherwise = -1

segundopassou :: Hora -> Int
segundopassou (AM hora min)
  | checkahora (hora) == True && checkamins (min) == True = (hora * 60 + min) * 60
  | otherwise = -1
segundopassou (PM hora min)
  | checkahora (hora) == True && checkamins (min) == True = (((12 + hora) * 60) + min) * 60
  | otherwise = -1


--8

--a)
data Contato
  = Nome String
  | Fone String

type Texto = String

type Data = (Int, Int, Int)

data Mensagem
  = WhatsApp Contato Texto Hora Data
  | LinkedIn Contato Texto Hora Data
  | Facebook Contato Texto Hora Data

--a:
msgRecebidas :: [Mensagem]
msgRecebidas =
  [ (WhatsApp (Nome "dalton") "fghfdgh" (AM 10 30) (13, 08, 20)),
    (LinkedIn (Fone "464563") "dfghdfgh" (AM 10 31) (13, 08, 20)),
    (Facebook (Nome "faskdflans") "ertyerty" (AM 10 32) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "cvbxcbv" (AM 10 33) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "vcnmghjm" (AM 10 37) (13, 08, 20)),
    (Facebook (Nome "faskdflans") "qwerqwer" (AM 11 30) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "sdfasdf" (AM 11 35) (13, 08, 20)),
    (Facebook (Fone "464563") "gbdfgsdfg" (AM 11 37) (13, 08, 20)),
    (LinkedIn (Nome "dalton") "ertert" (AM 11 39) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "yukuiol" (AM 11 42) (13, 08, 20)),
    (LinkedIn (Nome "faskdflans") "uidfghjgfh" (AM 11 42) (13, 08, 20)),
    (Facebook (Fone "464563") "tiyuityui" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "dalton") "hjkghjk" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "faskdflans") "adfgsdfg" (AM 11 54) (13, 08, 20)),
    (LinkedIn (Nome "faskdflans") "sdfgeryt" (AM 11 54) (13, 08, 20)),
    -- ======================================================
    (Facebook (Nome "dalton") "wertygdfhfg" (PM 3 25) (14, 08, 20)),
    (LinkedIn (Fone "dalton") "dfghrty" (PM 3 25) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "retyfghdfg" (PM 3 24) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "nfdghfgh" (PM 3 27) (14, 08, 20)),
    (LinkedIn (Nome "dalton") "fdghfgn" (PM 3 30) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "cvbnvcbn" (PM 3 33) (14, 08, 20)),
    (Facebook (Nome "faskdflans") "rtyerty" (PM 3 49) (14, 08, 20)),
    (WhatsApp (Fone "464563") "ertygfhdfg" (PM 4 50) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "sdrtert" (PM 4 57) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "cvbncvbn" (PM 4 30) (14, 08, 20)),
    (WhatsApp (Nome "dalton") "wertwert" (PM 4 30) (14, 08, 20)),
    (Facebook (Fone "464563") "fnmdhnjm" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Nome "faskdflans") "wertyey" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Fone "464563") "wertyrty" (PM 4 30) (14, 08, 20)),
    (Facebook (Nome "dalton") "weyrwty" (PM 4 30) (14, 08, 20))
  ]


--9)
data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt
  deriving (Show)

arvDados :: ArvBinInt
arvDados =
  No 23 (No 15 Nulo Nulo) (No 200 (No 150 Nulo Nulo) (No 678 Nulo Nulo))

-- a arvore sera:
--         23
--      56     200
--          5345   678
--
--a:
internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No n Nulo Nulo) = []
internos (No n esq dir) = [n] ++ internos esq ++ internos dir

--b:
somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No n Nulo Nulo) = n --no folha
somaNos (No n esq dir) = n + somaNos esq + somaNos dir --soma n com a soma dos filhos da dir e da esq

--c:
pertenceArv :: Int -> ArvBinInt -> Bool
pertenceArv x Nulo = False
pertenceArv x (No v esq dir)
 | x == v = True
 | otherwise =  if x < v then (pertenceArv x esq)
 else (pertenceArv x dir)

--10
data ArvBinEA a = Vazia| Folha a
                       | NoEA (Char, ArvBinEA a, ArvBinEA a)
                        deriving (Show)

arvEA :: ArvBinEA Float
arvEA = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)


inOrder::(Fractional a)=>ArvBinEA a->a
inOrder Vazia = 0
inOrder (Folha valor) = valor
inOrder (NoEA (op, esq, dir)) = result
 where
 result = operacao op (inOrder esq) (inOrder dir)

operacao::(Fractional a)=>Char->a->a->a
operacao op v1 v2
 | op == '*' = v1 * v2
 | op == '/' = v1 / v2
 | op == '+' = v1 + v2
 | op == '-' = v1 - v2
 | otherwise = 0