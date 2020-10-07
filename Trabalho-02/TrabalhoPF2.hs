--Grupo:
--Pedro Henrique Goncalves Teixeira-11821BCC008
--Alberto Ferreira Neto-11811BCC041

--Testes no GHCI:
l1 :: [Integer]
l1 = [1 .. 1000]

l2 :: [Integer]
l2 = [1000, 999 .. 1]

l3 :: [Integer]
l3 = l1 ++ [0]

l4 :: [Integer]
l4 = [0] ++ l2

l5 :: [Integer]
l5 = l1 ++ [0] ++ l2

l6 :: [Integer]
l6 = l2 ++ [0] ++ l1

l7 :: [Integer]
l7 = l2 ++ [0] ++ l2

x1 :: [Integer]
x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 :: [Integer]
x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 :: [Integer]
x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 :: [Integer]
x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 :: [Integer]
x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 :: [Integer]
x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 :: [Integer]
x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]

--EX1:

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort (x : xs) =
  let least = foldr1 (min) (x : xs)

      remove _ [] = []
      remove n (h : t) = if n == h then t else h : (remove n t)
   in [least] ++ selectionSort (remove least (x : xs))


insertionSort :: (Ord a) => [a] -> [a]
insertionSort l = foldr (insereOrd) [] l
  where
    insereOrd x [] = [x]
    insereOrd x (h : t) = if x <= h then (x : h : t) else h : (insereOrd x t)


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (piv : xs) = quickSort (filter (< piv) xs) ++ [piv] ++ quickSort (filter (>= piv) xs)

--EX2:
--var1
catch :: (Ord a) => ([a], Int) -> ([a], Int)
catch ([x], marc) = ([x], marc)
catch ((x : y : xs), marc) = if x > y then add (catch ((x : xs), 1)) y else add (catch ((y : xs), marc)) x
  where
    add (l, f) e = (e : l, f)

bubble :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubble (l, marc) 0 = (l, marc)
bubble (l, marc) n
					  | marc == 0 = (l, marc)
					  | otherwise = bubble (catch (l, 0)) (n -1)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort list = fst (bubble (list, -1) (length list))


--var2
bubbleSortx :: (Ord a) => [a] -> [a]
bubbleSortx [] = []
bubbleSortx lst =
  let catch [x] = [x]
      catch (x : y : xs) = if x > y then y : catch (x : xs) else x : catch (y : xs)

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble [x] = [x]
      bubble l = (bubble hacatchr) ++ ultimoElm
        where
          listaMod = catch l
          (hacatchr, ultimoElm) = split listaMod
   in bubble lst


--var3
bubbleSorty :: (Ord a) => [a] -> [a]
bubbleSorty [] = []
bubbleSorty l =
  let add (l, f) y = (y : l, f)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      catch ([x], marc) = ([x], marc)
      catch ((x : y : xs), marc) = if x > y then add (catch ((x : xs), 1)) y else add (catch ((y : xs), marc)) x

      bubble ([x], marc) = ([x], marc)
      bubble (lst, marc)
        | n_marc == 0 = (lst, marc)
        | otherwise = (fst (bubble (catchar, 0)) ++ lastelem, 0)
        where
          (catcha, n_marc) = catch (lst, marc)
          (catchar, lastelem) = split catcha
   in fst (bubble (l, -1))

--contador
--var1
bubbleScont :: (Ord a) => [a] -> ([a], Int)
bubbleScont [] = ([], 0)
bubbleScont list = format (bubleCont (list, -1, 0) (length list))
  where
    format (l, _, c) = (l, c)

catchCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
catchCont ([x], marc, n) = ([x], marc, n)
catchCont ((x : y : xs), marc, n) = if x > y then add (catchCont ((x : xs), 1, n + 1)) y else add (catchCont ((y : xs), marc, n + 1)) x
  where
    add (l, f, c) e = (e : l, f, c)

bubleCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubleCont (l, marc, c) 0 = (l, marc, c)
bubleCont (l, marc, c) n
  | marc == 0 = (l, marc, c)
  | otherwise = bubleCont (catchCont (l, 0, c)) (n -1)

--var2
bubbleScont2 :: (Ord a) => [a] -> ([a], Int)
bubbleScont2 [] = ([], 0)
bubbleScont2 lst =
  let add (l, c) e = (e : l, c)

      catch ([x], c) = ([x], c)
      catch ((x : y : xs), c) = if x > y then add (catch (x : xs, c + 1)) y else add (catch (y : xs, c + 1)) x

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble :: (Ord a) => ([a], Int) -> ([a], Int)
      bubble ([x], c) = ([x], c)
      bubble (l, c) = (steps ++ lastelem, rec_c)
        where
          (catchx, c1) = (catch (l, c))
          (catcha, lastelem) = split catchx
          (steps, rec_c) = bubble (catchx, c1)
   in bubble (lst, 0)

--var3
bubbleScont3 :: (Ord a) => [a] -> ([a], Int)
bubbleScont3 [] = ([], 0)
bubbleScont3 l =
  let add (l, f, c) y = (y : l, f, c)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
      format (l, _, c) = (l, c)

      catch ([x], marc, c) = ([x], marc, c)
      catch ((x : y : xs), marc, c) = if x > y then add (catch ((x : xs), 1, c + 1)) y else add (catch ((y : xs), marc, c + 1)) x

      bubble ([x], marc, c) = ([x], marc, c)
      bubble (lst, marc, c)
        | n_marc == 0 = (lst, marc, c)
        | otherwise = (steps ++ lastelem, 0, rec_c)
        where
          (catcha, n_marc, c1) = catch (lst, marc, c)
          (catchar, lastelem) = split catcha
          (steps, _, rec_c) = bubble (catchar, 0, c1)
   in format (bubble (l, -1, 0))

--a var3 é a melhor, pois realiza menos verificacoes

--EX3:
--var1
selectionSEX3 :: (Ord a) => [a] -> [a]
selectionSEX3 [] = []
selectionSEX3 [x] = [x]
selectionSEX3 (x : xs) =
  let least = foldr1 (min) (x : xs)

      remove _ [] = []
      remove n (h : t) = if n == h then t else h : (remove n t)
   in least : selectionSEX3 (remove least (x : xs))

--var2
removeMenor :: (Ord a) => (a, [a]) -> (a, [a])
removeMenor (m, [x]) = if x < m then (x, [m]) else (m, [x])
removeMenor (menor, (x : xs))
  | x < menor = add menor (removeMenor (x, xs))
  | otherwise = add x (removeMenor (menor, xs))
  where
    add a (n, l) = (n, a : l)

selectionS3 :: (Ord a) => [a] -> [a]
selectionS3 [] = []
selectionS3 [x] = [x]
selectionS3 lst =
  let (least, novoUlt) = removeMenor (head lst, tail lst)
   in least : (selectionS3 novoUlt)


removeMcont :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
removeMcont (m, [x], c) = if x < m then (x, [m], c + 1) else (m, [x], c + 1)
removeMcont (menor, (x : xs), c1)
  | x < menor = add menor (removeMcont (x, xs, c1 + 1))
  | otherwise = add x (removeMcont (menor, xs, c1 + 1))
  where
    add a (n, l, c) = (n, a : l, c)

selectionS3Cont :: (Ord a) => [a] -> ([a], Int)
selectionS3Cont [] = ([], 0)
selectionS3Cont [x] = ([x], 0)
selectionS3Cont (x : xs) =
  let (least, novoUlt, cont) = removeMcont (x, xs, 0)

      (steps, nCont) = selectionS3Cont novoUlt
   in (least : steps, cont + nCont)

--a var1 é a melhor, pois na 2 para ela retirar o elemento ela precisa mante-lo fora da lista. Esse processo gasta muito.

--EX4:
--var1
divide :: (Ord a) => a -> [a] -> ([a], [a])
divide x [] = ([], [])
divide x [e] = if e < x then ([e], []) else ([], [e])
divide x (e : es)
  | e < x = addEsq e (divide x es)
  | otherwise = addDir e (divide x es)
  where
    addEsq a (l, r) = (a : l, r)
    addDir a (l, r) = (l, a : r)

quickSEX4 :: (Ord a) => [a] -> [a]
quickSEX4 [] = []
quickSEX4 (piv : xs) =
  let (left, right) = divide piv xs
   in (quickSEX4 left) ++ [piv] ++ (quickSEX4 right)

--var2
quickS2 :: (Ord a) => [a] -> [a]
quickS2 [] = []
quickS2 lst =
  let firstThree = take 3 lst
      piv = if length (firstThree) < 3 then firstThree !! 0 else foldr1 (min) (firstThree)

      deletaPrimOcorrencia x [] = []
      deletaPrimOcorrencia x (y : ys)
        | x == y = ys
        | otherwise = y : deletaPrimOcorrencia x ys

      (left, right) = divide piv (deletaPrimOcorrencia piv lst)
   in (quickS2 left) ++ [piv] ++ (quickS2 right)


divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont x [] n = ([], [], n)
divideCont x [e] n = if e < x then ([e], [], n + 1) else ([], [e], n + 1)
divideCont x (e : es) n
  | e < x = addEsq e (divideCont x es (n + 1))
  | otherwise = addDir e (divideCont x es (n + 1))
  where
    addEsq a (l, r, c) = (a : l, r, c)
    addDir a (l, r, c) = (l, a : r, c)

quickScont :: (Ord a) => [a] -> ([a], Int)
quickScont [] = ([], 0)
quickScont (piv : xs) =
  let (left, right, n) = divideCont piv xs 0

      (sortedL, n_L) = quickScont left
      (sortedR, n_R) = quickScont right
   in (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)

quickScont2 :: (Ord a) => [a] -> ([a], Int)
quickScont2 [] = ([], 0)
quickScont2 lst =
  let piv = foldr1 (min) (take 3 lst)

      deleteFrstOc :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleteFrstOc x [] n = ([], n)
      deleteFrstOc x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = add y (deleteFrstOc x ys (n + 1))
        where
          add e (l, c) = (e : l, c)

      (novoUlt, checks) = deleteFrstOc piv lst 0

      (left, right, n1) = divideCont piv novoUlt 0
      (sortedL, n_L) = quickScont2 left
      (sortedR, n_R) = quickScont2 right
   in (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3) -- Comps. atuais + comps recursivas + comps do deletaPrimOcorrencia + 3 comps. do foldr1

--a var2 é a melhor, pois ela faz menos testes de verificacao que as demais.



--Todo: Questão 5

--EX6:
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)

checka :: Floating a => Exp a -> a
checka (Val x) = x
checka (Add exp1 exp2) = (checka exp1) + (checka exp2)
checka (Sub exp1 exp2) = (checka exp1) - (checka exp2)
checka (Mult exp1 exp2) = (checka exp1) * (checka exp2)
checka (Pot exp1 exp2) = (checka exp1) ** (checka exp2)

expressao1 :: Exp Integer
expressao1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expressao2 :: Exp Integer
expressao2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

--EX7:

data Hora = PM Int Int | AM Int Int deriving (Eq, Show, Ord)

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

--EX8:
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

--b:
myBubblesort [] = []
myBubblesort lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (catch lista) (n -1)

catch [x] = [x]
catch (x : y : zs)
  | x > y = y : catch (x : zs)
  | otherwise = x : catch (y : zs)
  
--c:
msg :: Mensagem -> Mensagem -> Bool
msg (Msg _ _ d1 h1 _) (Msg _ _ d2 h2 _)
  | d1 == d2 = (minpass h1) < (minpass h2)
  | otherwise = precede d1 d2

ordenaDataHora :: [Mensagem] -> [Mensagem]
ordenaDataHora [] = []
ordenaDataHora (piv : xs) =
  (ordenaDataHora [x | x <- xs, (msg x piv) == False])
    ++ [piv]
    ++ (ordenaDataHora [x | x <- xs, (msg x piv) == True])
	
--d:
last :: Contato -> [Mensagem] -> [Mensagem]
last cont msgs = take 2 [(Msg c m d h a) | (Msg c m d h a) <- msgOrd, c == cont]
  where
    msgOrd = ordenaDataHora msgs

--EX9:
data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt
  deriving (Show)

arvDados :: ArvBinInt
arvDados =
  No
    23
    (No 56 Nulo Nulo)
    ( No
        200
        (No 5345 Nulo Nulo)
        (No 678 Nulo Nulo)
    )

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
pertenceArv x (No v esq dir) =
  x == v --compara o valor com o no
    || if x < v --escolhe pra qual lado da arvore vai
      then (pertenceArv x esq)
      else (pertenceArv x dir)

--EX10:
data ArvBinEA a = Vazia| Folha a
                       | NoEA (Char, ArvBinEA a, ArvBinEA a)
                        deriving (Show)

arvEA :: ArvBinEA Float
arvEA = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

inOrder :: Show a => ArvBinEA a -> [Char]
inOrder Vazia = []
inOrder (Folha valor) = show valor
inOrder (NoEA (operacao, esq, dir)) = inOrder esq ++ [operacao] ++ inOrder dir