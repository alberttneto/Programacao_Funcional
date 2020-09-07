--1
checkTri::Float->Float->Float->Bool
checkTri x y z | x + y + z == 180 = True
 | otherwise = False

triangulo::Float->Float->Float->String
triangulo  x y z | (x == y) && (y == z) && checkTri x y z = "equilatero"
 | ((x == 90) || (y == 90) || (z == 90)) && checkTri x y z = "retangulo"
 | ((x > 90) || (y > 90) || (z > 90)) && checkTri x y z = "obtuso"
 | checkTri x y z = "simples"
 | otherwise = "nao_triangulo"


 --2
fstgrau::Float->Float->Float
fstgrau a b = -b/a

delta::Float->Float->Float->Float
delta a b c = (b ** 2) - 4 * a * c

sndgrau::Float->Float->Float->(Float,Float)
sndgrau a b c | b /= 0 = ((-b + sqrt (delta a b c))/(2*a),(-b - sqrt (delta a b c))/(2*a))
 | otherwise = (sqrt (-c/a),-sqrt (-c/a))

equacao::Float->Float->Float->(Float,Float)
equacao a b c | a == 0 = (fstgrau b c, a)
 | otherwise = sndgrau a b c


--3
type Data = (Int,Int,Int)

idade::Data->Data->Int
idade (da,ma,aa) (dn,mn,an) 
 | ma < mn || (ma == mn && da < dn) = aa - an - 1
 | otherwise = aa - an

valorPassagem::Float->Data->Data->Float
valorPassagem valor dAtual dNasc
 | (idade dAtual dNasc) < 2 = valor*15/100
 | (idade dAtual dNasc) >= 2 && (idade dAtual dNasc) <= 10 = valor*40/100
 | (idade dAtual dNasc) >= 70 = valor*50/100
 | otherwise = valor


--4 
type Lista = [Int]

--a) 
gera1::Lista
gera1 = [n^2 | n <- [1..15], n > 3, n < 15, mod n 2 /= 0] 

--b)
gera2::[(Int,Int)]
gera2 = [(x,y) | x <- [1..15], x < 5, y <- [x..x+x] ]

--c)
gera3::Lista
gera3 = [n |x <- [1..15], x > 9, n <- [1..x]]

--d)
gera4::[(Int,Int)]
gera4 = [(n,n+1) | n <- [1..15], odd n]

--e)
gera5::Lista
gera5 = [fst(n) + snd(n) | n <- gera4]


--5
--a)
contaNegM2::Lista->Int
contaNegM2 x = sum [1 | n <- x, n < 0, even n]

--b)
listaNegM2::Lista->Lista
listaNegM2 x = [n | n <- x, n < 0, even n]


--6
distancias::[(Float,Float)]->[Float]
distancias lista = [sqrt (fst (n) ^ 2 + snd (n) ^ 2) | n <- lista]


--7
fatores::Int->Lista
fatores x = [n | n <- [1..x], mod x n == 0]

primos::Int->Int->Lista
primos x y = [n | n <- [x..y], fatores n == [1,n]]


--8
checkMmc::Int->Int->Int->Int->Int
checkMmc x y z n 
 | mod n x == 0 && mod n y == 0 && mod n z == 0 = n
 | otherwise = checkMmc x y z (n+1)

mmc::Int->Int->Int->Int
mmc x y z = checkMmc x y z 1


--9
calculaSerie::Float->Int->Float
calculaSerie x n = sum [if even n then x/fromIntegral(n) else fromIntegral(n)/x | n <- [1..n]]


--10
checkFizzbuzz::Int->String
checkFizzbuzz x 
 | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
 | mod x 3 == 0 = "Fizz"
 | mod x 5 == 0 = "Buzz"
 | otherwise = "No" 

fizzbuzz::Int->[String]
fizzbuzz n = [checkFizzbuzz a | a <- [1..n]]


--11 
contador::Int->Int->Lista->(Int,Int)
contador x y lista 
 | null lista = (0,0)
 | x == head lista = (1 + fst (contador x y (tail lista)), snd (contador x y (tail lista)))
 | y == head lista = (fst (contador x y (tail lista)), 1 + snd (contador x y (tail lista)))
 | otherwise = contador x y (tail lista)


--12
unica_ocorrencia::Int->Lista->Bool
unica_ocorrencia x lista = 
 if length [n | n <- lista, n == x] == 1 
 then True 
 else False


--13 
intercala_list::Lista->Lista->Lista
intercala_list [] [] = []
intercala_list lista [] = lista
intercala_list [] lista = lista
intercala_list (a:b) (a1:b1) = a: a1 : intercala_list b b1


--14
type Contato = (String, String, String, String)
c1, c2, c3, c4, c5::Contato
c1 = ("Alberto", "Rua 2, ap 1, Uberlandia-MG", "34 99994455", "beto@email.com")
c2 = ("João", "Rua 45, 451, Uberlandia-MG", "34 985452555", "jj@email.com")
c3 = ("Maria", "Rua Norte, ap 220, Uberaba-MG", "34 984788521", "mari@email.com")
c4 = ("Pedro", "Rua Joaquim, 2200, Rio de Janeiro-RJ", "92 945658545", "pedrinho@email.com")
c5 = ("Joana", "Rua Naves, ap 102, Maraba-PA", "94 975898652", "jonan12@email.com")

contatos::[Contato]
contatos = [c1,c2,c3,c4,c5]

nome_email::Contato->(String,String)
nome_email (n,_,_,e) = (n,e)

recuperaContato::String->String
recuperaContato email = head [fst (nome_email n) | n <- contatos, snd (nome_email n) == email] 


--15
type Pessoa = (String,Float,Int,Char)
pessoas::[Pessoa]
pessoas = 
 [("Rosa", 1.66, 27,'F'),
 ("Joao", 1.85, 26, 'C'),
 ("Maria", 1.55, 62, 'S'),
 ("Jose", 1.78, 42, 'C'),
 ("Paulo", 1.93, 25, 'S'),
 ("Clara", 1.70, 33, 'C'),
 ("Bob", 1.45, 21, 'C'),
 ("Rosana", 1.58,39, 'S'),
 ("Daniel", 1.74, 72, 'S'),
 ("Jocileide", 1.69, 18, 'S')]

--------
altura::Pessoa->Float
altura (_,alt,_,_) = alt

altura_media::Float
altura_media = sum [altura n | n <- pessoas] / fromIntegral(length pessoas)

--------
valor_id::Pessoa->Int
valor_id (_,_,id,_) = id

idade_mais_novo::Int
idade_mais_novo = minimum [valor_id n | n <- pessoas]

--------
nome_estadoCivil::Pessoa->(String,Char)
nome_estadoCivil (n,_,_,ec) = (n,ec)

mais_velho_nec::(String,Char)
mais_velho_nec = head [nome_estadoCivil n | n <- pessoas, (maximum [valor_id n2 | n2 <- pessoas]) == valor_id n]

--------
dados_50Anos::[Pessoa]
dados_50Anos = [n | n <- pessoas, valor_id n >= 50]

--------
pessoas_por_idade::Int->Int
pessoas_por_idade i = sum [ 1 | n <- [n | n <- pessoas, snd (nome_estadoCivil n) == 'C'], valor_id n > i]


--16
insere_ord::(Ord t) => t->[t]->[t]
insere_ord x [] = [x]
insere_ord x (a:b) = if (x <= a) then x:a:b else a: insere_ord x b


--17
reverte::[t]->[t]
reverte [a] = [a]
reverte (a:b) = reverte b ++ [a]


--18
sem_repetidos::(Ord t)=>[t]->[t]
sem_repetidos [] = []
sem_repetidos (a:b) = if elem (head [a]) b then sem_repetidos b else a: sem_repetidos b


--19
disponiveis::[Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco::Int->[[Int]]
notasTroco 0 = [[]]
notasTroco x = [n1:n2 | n1 <- disponiveis, n1 <= x, n2 <- notasTroco (x-n1)]



--20

  

















