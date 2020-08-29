--1
dobro::Int->Int
dobro n = n * 2
quad::Int->Int
quad n = 2 * (dobro n)
hipotenusa::Float->Float->Float
hipotenusa x y = sqrt ((x ** 2) + (y ** 2))
distAB::Float->Float->Float->Float->Float
distAB xa ya xb yb = (hipotenusa (xb - xa) (yb - ya))

--2
{-
ghci> fst (2,5)
2 //Obs Recebe um par e retorna o primeiro componente
ghci> snd (5, "Bom dia")
Bom dia //Obs Recebe um par e retorna o segundo componente
ghci> (1,1) == (1,1)
True //Obs Compara se duas tuplas são iguals
ghci> (1,1) /= (1,1)
False //Obs Compara se duas tuplas são diferentes
ghci> (1,1) < (1,2)
True //Obs Verifica se os elementos da primeira tupla são menores que o da segunda
ghci> (2,1) < (1,2)
False //Obs Verifica se os elementos da primeira tupla são menores que o da segunda
ghci> (1,2,3) < (1,2)
Erro //Obs Tuplas de tamanhos diferentes
ghci> "azul" < "verde"
True //Obs Verifica se o valor do primeiro caractere da primeira string é menor que o primeiro caractere da segunda string
ghci> "azul" < "amarelo"
False //Obs Verifica se o valor do segundo caractere da primeira string é menor que o segundo caractere da segunda string
ghci> (1,2,3) == (,,) 1 2 3
True //Obs Comparando se as Tuplas são iguais, são iguais pois no segundo caso é como o compilador interpreta o código
-}

-- 3
type Moeda = (Float,Float,Float)
conversao::Float->Moeda
conversao x = (x, x*3.96, x*4.45)

-- 4
bissexto::Int->Bool
bissexto x | ((mod x 400) == 0) = True
 | ((mod x 100) /= 0) && ((mod x 4) == 0) = True
 | otherwise = False

-- 5
type Data = (Int,Int,Int)
bissexto2::Data->Bool
bissexto2 (d,m,a) = bissexto a

-- 6
valida::Data->Bool
valida (d,m,a) | mod m 2 /= 0 && d > 0 && d <= 31 && m > 0 && m <= 12 = True
 | mod m 2 == 0 && m /= 2 && d > 0 && d <= 30 && m > 0 && m <= 12 = True
 | (bissexto a) && m == 2 && d > 0 && d <= 29 && m > 0 && m <= 12 = True
 | not (bissexto a) && m == 2 && d > 0 && d <= 28 && m > 0 && m <= 12 = True
 | otherwise = False

-- 7
precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) | a1 < a2 && (valida (d1,m1,a1)) && (valida (d2,m2,a2)) = True
 | a1 == a2 && m1 < m2 && (valida (d1,m1,a1)) && (valida (d2,m2,a2)) = True
 | a1 == a2 && m1 == m2 && d1 < d2 && (valida (d1,m1,a1)) && (valida (d2,m2,a2)) = True
 | otherwise = False

 -- 8
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)
type ListaLivros = [Livro]
type ListaAluno = [Aluno]
type ListaEmprestimo = [Emprestimo]

alunos::ListaAluno
alunos = [("BCC63","Alberto","beto@email.com", "34 95050-2012"),
 ("BSI50","Pedro","pd@email.com", "34 92030-9999"),
 ("BCC62","Maria","mari@email.com", "34 91010-5050")]

livros::ListaLivros
livros = [("1","Programação Funcional","Joao L.","Pife",1989),
 ("2","Programação Orientada a Objeto","Pedro A.","Pife",1990),
 ("3","Programação Logica","Silvio L.","Teff",1980)]

e1::Emprestimo
e2::Emprestimo
e3::Emprestimo

-- 9
e1 = ("1","BCC63",(01,05,2020),(01,07,2020),"encerrado")
e2 = ("2","BCC62",(01,08,2020),(28,08,2020),"aberto")
e3 = ("3","BSI50",(05,08,2020),(20,08,2020),"aberto")


dataDev(_,_,_,dt,_) = dt
situacao(_,_,_,_,sit) = sit
checkEmprestimo::Emprestimo->Data->Bool
checkEmprestimo emp (d,m,a) | not (precede (dataDev emp) (d,m,a)) && head(situacao emp) == 'a' = False
 | otherwise = True





