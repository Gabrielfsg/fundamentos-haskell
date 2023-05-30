import Data.Char (digitToInt)


media y x = (y + x / 2)
hipotenusa a b = sqrt( ( (a * a) + (b*b) ) )

verificaSeTaNaLista :: Eq a => a -> [a] -> Bool
verificaSeTaNaLista _ [] = False
verificaSeTaNaLista x (y:ys)
    | x == y    = True
    | otherwise = verificaSeTaNaLista x ys


listaAte200 :: Integer -> [Integer]
listaAte200 0 = [0]
listaAte200 x = x : listaAte200 (x-1)

inverso :: [Int] -> [Int]
inverso [] = []
inverso (h:t) = inverso (t) ++ [h]

nPrimeirosDigitos :: Int -> [Int] -> [Int]
nPrimeirosDigitos 0 (h:t) = []
nPrimeirosDigitos x (h:t) = h: nPrimeirosDigitos (x-1) t

removeNPrimeirosDigitos :: Int -> [Int] -> [Int]
removeNPrimeirosDigitos 1 (h:t) = t
removeNPrimeirosDigitos x (h:t) = removeNPrimeirosDigitos (x-1) t

ultimoElementoLista :: [Int] -> [Int]
ultimoElementoLista (h:[]) = [h]
ultimoElementoLista (h:t) = ultimoElementoLista t

removeUltimosElementos :: Int -> [a] -> [a]
removeUltimosElementos n lista = take (length lista - n) lista

removeEnesimo :: Int -> [Int] -> [Int]
removeEnesimo _ [] = []
removeEnesimo x (h:t)
	| x == h = removeEnesimo x t
	| otherwise = h: removeEnesimo x t

removePares :: [Int] -> [Int]
removePares [] = []
removePares (h:t)
	| h `mod` 2 == 0 = removePares t
	| otherwise = h: removePares t

iguais :: Bool
iguais = let a = 2
             b = 2
             c = 2
         in a == b && b == c && c == a

areaTri :: Double
areaTri = let a = 1.0
              b = 8.0
              c = 9.0
              s = calculoS 1.0 2.0 3.0
          in sqrt( s * (s - a) * (s - b) * (s - c) )

calculoS :: Double -> Double -> Double -> Double
calculoS a b c = (a+b+c)/2

fatorial :: Integer -> Integer
fatorial x =
    if x == 0
      then 1
      else x * fatorial (x-1)


mediaC :: Double -> Double -> Double
mediaC num1 num2 = (num1+num2)/2

potencia :: Integer -> Integer
potencia x = if x > 0
              then x * potencia (x-1)
    	      else 1

resultadoSomaSeq :: Integer -> Int
resultadoSomaSeq x = let lista = intToIntegers x
			 soma = somaDaSequencia lista
		     in soma 

intToIntegers :: Integer -> [Int]
intToIntegers x = map digitToInt (show x)

somaDaSequencia :: [Int] -> Int
somaDaSequencia (h:[]) = h
somaDaSequencia (h:t) =  h + somaDaSequencia t 

par x = if x `mod` 2 == 0
         then True
         else False

somatorio :: Integer -> Integer
somatorio x = if x > 0
               then x + somatorio (x-1)
               else 0

-- comentario

doisPontos :: (Float,Float) -> (Float,Float) -> Float
doisPontos (x1,y1) (x2,y2) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

bisexto x = if x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0
	       then True
	       else False

dataValidar :: Integer -> Integer -> Integer -> Bool
dataValidar x y z = if (x >= 1 && x <= 31) && (y >= 1 && y <= 12) && (z >= 0)
		then True
		else False 

conceito :: Float -> String
conceito x | x < 4 = "Conceito E"
	   | x >= 4 && x <= 5.99 = "Conceito D"
	   | x >= 6 && x <= 7.49 = "Conceito C"
	   | x >= 7.5 && x <= 8.99 = "Conceito B"
           | otherwise = "Conceito A"

type Seq = String
type Nomes = (Seq, Seq, Seq, Seq)

--funções constantes

f_nomes_estacoes :: Nomes
f_nomes_estacoes = ("Inverno","Outono","Primavera","Verao")

primeiro :: Nomes -> Seq
primeiro (x, _, _, _) = x

segundo :: Nomes -> Seq
segundo (_, x, _, _) = x

type Nome = String
type Idade = Integer
type Peso = Float
type Esporte = String
type Pessoa = (Nome,Idade,Peso,Esporte)

bancoDeDados :: Integer -> Pessoa
bancoDeDados id | id == 1 = ("Alice", 25, 60.5, "Tenis")
		| id == 2 = ("Bob", 30, 75.2, "Futebol")
		| id == 3 = ("Carol", 28, 63.8, "Natacao")
		| id == 4 = ("David", 35, 80.1, "Basquete")
		| id == 5 = ("Eve", 22, 55.9, "Corrida")
		| otherwise = ("Desconhecido", 0, 0.0, "Desconhecido")

getNome :: Integer -> String
getNome x = let (n,_,_,_) = bancoDeDados x in n

getIdade :: Integer -> Integer
getIdade x = let (_,i,_,_) = bancoDeDados x in i

getPeso :: Integer -> Float
getPeso x = let (_,_,p,_) = bancoDeDados x in p

getEsporte :: Integer -> String
getEsporte x = let (_,_,_,e) = bancoDeDados x in e

comparaIdades :: Integer -> Integer -> String
comparaIdades x y = let (n1,i1,_,_) = bancoDeDados x
			(n2,i2,_,_) = bancoDeDados y
		    in if (i1 > i2)
		          then n1
		       else n2
