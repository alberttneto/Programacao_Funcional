-- 1
paridade::[Int]->[Bool]
paridade l = map even l

-- 2
prefixos::[String]->[String]
prefixos l = map (take 3) l

-- 3
saudacao::[String]->[String]
saudacao l = map ("Oi " ++) l

-- 4
filtrar::(a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar cond (a:b) 
 | cond a = a:filtrar cond b
 | otherwise = filtrar cond b

filtrar2::(a -> Bool) -> [a] -> [a]
filtrar2 cond list = [n | n <- list, cond n]

-- 5
pares::[Int]->[Int]
pares lst = filter (even) lst

-- 6
solucao::[Int]->[Int]
solucao lst = filter (\x -> 5*x + 6 < x*x) lst

-- 7
maior::[Int]->Int
maior lst = foldr1 max lst

-- 8
menor_min10::[Int]->Int
menor_min10 lst = foldr min 10 lst

-- 9
junta_silabasplural::[String]->String
junta_silabasplural lst = foldr (++) "s" lst

-- 10

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3

-- BubbleSort
bolha::(Ord a)=>[a]->[a]
bolha [] = []
bolha lista = bolhaOrd lista (length lista)

bolhaOrd::(Ord a)=>[a]->Int->[a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca::(Ord a)=>[a]->[a]
troca [x] = [x]
troca (x:y:zs)
 | x > y = y: troca (x:zs)
 | otherwise = x: troca (y:zs)

-- SelectionSort
selecao::(Ord a)=>[a]->[a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
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

-- InsertionSort
insercao::(Ord a)=>[a]->[a]
insercao [] = []
insercao (x:xs) = insereOrd x (insercao xs)

insereOrd::(Ord a)=>a->[a]->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y: (insereOrd x ys)

-- QuiclSort
quicksort::(Ord a)=>[a]->[a]
quicksort [] = []
quicksort (s:xs) =
 quicksort [x | x <- xs, x < s]
 ++ [s] ++
 quicksort [x | x <- xs, x >= s]

-- 11

-- BubbleSort
bolha2::(Ord a)=>[a]->([a],Int)
bolha2 [] = ([],0)
bolha2 lista = bolhaOrd2 lista (length lista) 0

bolhaOrd2::(Ord a)=>[a]->Int->Int->([a],Int)
bolhaOrd2 lista 0 _ = (lista,0)
bolhaOrd2 lista n c = (fst (bolhaOrd2 (fst (troca2 lista c)) (n-1) (c + snd (troca2 lista c))), 
 c+snd (bolhaOrd2 (fst (troca2 lista c)) (n-1) (c + snd (troca2 lista c))))

troca2::(Ord a)=>[a]->Int->([a],Int)
troca2 [x] c = ([x],c)
troca2 (x:y:zs) c
 | x > y = (y: fst (troca2 (x:zs) c), 1 + snd (troca2 (x:zs) c)) 
 | otherwise = (x: fst (troca2 (y:zs) c), c)

-- SelectionSort
selecao2::(Ord a)=>[a]->([a],Int)
selecao2 [] = ([],0)
selecao2 xs = ([x] ++ fst (selecao2 (remove2 x xs)), c + snd (minimo2 xs c)) 
 where 
 x = fst (minimo2 xs 0)
 c = snd (minimo2 xs 0)

remove2::(Ord a)=>a->[a]->[a]
remove2 a [] = []
remove2 a (x:xs)
 | a==x = xs
 | otherwise = x: (remove2 a xs)

minimo2::(Ord a)=>[a]->Int->(a,Int)
minimo2 [] _ = undefined
minimo2 [x] _ = (x,0)
minimo2 (x:xs) c
 | x <= (fst (minimo2 xs c)) = (x,1 + snd (minimo2 xs c))
 | otherwise = minimo2 xs c



-- 12
-- BubbleSort
bolha3::(Ord a)=>[a]->([a],Int)
bolha3 [] = ([],0)
bolha3 lista = bolhaOrd3 lista (length lista) 0

bolhaOrd3::(Ord a)=>[a]->Int->Int->([a],Int)
bolhaOrd3 lista 0 _ = (lista,0)
bolhaOrd3 lista n c = (fst (bolhaOrd3 (fst (troca3 lista c)) (n-1) (c + snd (troca3 lista c))), 
 c+snd (bolhaOrd3 (fst (troca3 lista c)) (n-1) (c + snd (troca3 lista c))))

troca3::(Ord a)=>[a]->Int->([a],Int)
troca3 [x] c = ([x],c)
troca3 (x:y:zs) c
 | x < y = (y: fst (troca3 (x:zs) c), 1 + snd (troca3 (x:zs) c)) 
 | otherwise = (x: fst (troca3 (y:zs) c), c)

-- SelectionSort

selecao3::(Ord a)=>[a]->([a],Int)
selecao3 [] = ([],0)
selecao3 xs = ([x] ++ fst (selecao3 (remove3 x xs)), c + snd (minimo3 xs c)) 
 where 
 x = fst (minimo3 xs 0)
 c = snd (minimo3 xs 0)

remove3::(Ord a)=>a->[a]->[a]
remove3 a [] = []
remove3 a (x:xs)
 | a==x = xs
 | otherwise = x: (remove3 a xs)

minimo3::(Ord a)=>[a]->Int->(a,Int)
minimo3 [] _ = undefined
minimo3 [x] _ = (x,0)
minimo3 (x:xs) c
 | x >= (fst (minimo3 xs c)) = (x,1 + snd (minimo3 xs c))
 | otherwise = minimo3 xs c








