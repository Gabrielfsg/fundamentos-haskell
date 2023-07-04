import Data.Char (digitToInt)

media y x = (y + x / 2)
hipotenusa a b = sqrt( ( (a * a) + (b*b) ) )

verificaSeTaNaLista :: Eq a => a -> [a] -> Bool
verificaSeTaNaLista _ [] = False
verificaSeTaNaLista x (y:ys)
    | x == y    = True
    | otherwise = verificaSeTaNaLista x ys

removePrimosLista :: [Int] -> [Int]
removePrimosLista [] = []
removePrimosLista (c:cs)
	| primoVer c == True =  removePrimosLista cs
	| otherwise = c: removePrimosLista cs

primoVer :: Int -> Bool
primoVer x = numPrimo x x 0

numPrimo :: Int -> Int -> Int -> Bool
numPrimo 0 _ _ = False
numPrimo 1 _ _ = False
numPrimo num cop cont
	| cop == 0 && cont > 2 = False
	| cop == 0 && cont == 2 = True
	| num `mod` cop == 0 = numPrimo num (cop-1) (cont+1)
	| otherwise = numPrimo num (cop-1) cont

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
funcaoPertenceX2 = [x^2 | x<-[1,2,3,4,5,6,7,8]]

somaTupla (x,y) = x + y

somaListaTuplas :: [(Int, Int)] -> [Int]
somaListaTuplas [] = [] 
somaListaTuplas (h:t) = somaTupla h: somaListaTuplas t

alfabeto = ['a'..'z']

verificaLista :: [Int] -> Int -> Bool
verificaLista l x | x == 1 = listaImpar l
		  | x == 2 = listaPar l
		  | otherwise = False

listaImpar :: [Int] -> Bool
listaImpar [] = True
listaImpar (x:y) | x `mod` 2 == 0 = False
		 | otherwise = listaImpar y

listaPar :: [Int] -> Bool
listaPar [] = True
listaPar (x:y) | x `mod` 2 /= 0 = False
	       | otherwise = listaPar y


primo :: Int -> Int -> Int -> Bool
primo _ 0 z 
	| z == 2 = True
	|otherwise = False
primo x y z 
	| x `mod` y == 0 = primo x (y-1) (z+1)
	| otherwise = primo x (y-1) z
	
	
ePrimo :: Int -> Bool
ePrimo x = primo x x 0


tamanhoLista :: [Int] -> Int
tamanhoLista x = verificaTamanhoLista x 0

verificaTamanhoLista :: [Int] -> Int -> Int
verificaTamanhoLista [] z  = z
verificaTamanhoLista (x:y) z = verificaTamanhoLista y (z+1)


prefixo :: [Int] -> [Int] -> Bool
prefixo [] _ = True
prefixo (a:b) (c:d)
		| a == c = prefixo b d
		| otherwise = False
		
		
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:y) = inverte y ++ [x]

sufixo :: [Int] -> [Int] -> Bool
sufixo x y = prefixo (inverte x) (inverte y) 

verificaSeTaNaListaDef [] _ = False 
verificaSeTaNaListaDef :: [Int] -> Int -> Bool
verificaSeTaNaListaDef (x:y) z 
	| x == z = True
	|otherwise = verificaSeTaNaListaDef y z
	
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:y) = 1 + somaLista y 

menor :: [Int] -> Int
menor (x:y) = menorLista y x

menorLista :: [Int] -> Int -> Int
menorLista [] z = z
menorLista (x:y) z
	| x < z = menorLista y x
	| otherwise = menorLista y z

adicionaInitLista :: [Int] -> Int -> [Int]
adicionaInitLista x y = (y:x)

adicionaFinalLista :: [Int] -> Int -> [Int]
adicionaFinalLista [] y = [y]
adicionaFinalLista (x:z) y = [x] ++ adicionaFinalLista z y 


removeDuplicados :: [Int] -> [Int] -> [Int]
removeDuplicados [] x = x
removeDuplicados (a:b) x = if verificaSeTaNaListaDef x a
                               then removeDuplicados b x
                               else removeDuplicados b (x ++ [a])

deletaNum :: [Int] -> Int -> [Int]
deletaNum [] _ = []
deletaNum (h:t) x 
	| h == x = deletaNum t x
	| otherwise = [h] ++ deletaNum t x

deletaIndice :: [Int] -> Int -> [Int]
deletaIndice (h:t) 0 = t
deletaIndice (h:t) x = [h] ++ deletaIndice t (x-1)


type Vertice = Int
type Aresta = (Vertice,Vertice)
type Grafo = [(Vertice, Vertice)]

bancoGrafo :: Int -> Grafo
bancoGrafo c 
	| c == 1 = [(1,2),(2,3),(3,4),(2,5)]
	| otherwise = []

adicionarAresta :: Grafo -> Aresta -> Grafo
adicionarAresta grafo aresta = aresta : grafo

existeAresta :: Grafo -> Aresta -> Bool
existeAresta grafo aresta = aresta `elem` grafo

vizinho :: Grafo -> Int -> Int -> Bool
vizinho [] _ _ =  False
vizinho (h:t) x y
	| h == (x,y) || h == (y,x) = True
	| otherwise = vizinho t x y

conectados :: Grafo -> Int -> Int -> [Int] -> Bool
conectados _ 0 _ _ = False
conectados grafo v1 v2 lista
    | vizinho grafo v1 v2 == True = True
    | otherwise = let y = retornaVizinho grafo v1 lista in conectados grafo y v2 (v1 : lista)

										
retornaVizinho :: Grafo -> Int -> [Int] -> Int
retornaVizinho [] _ _  =  0
retornaVizinho ((y,z):t) x lista
	| x == y && verTaLista lista z == False = z
	| x == z && verTaLista lista y == False = y
	| otherwise = retornaVizinho t x lista


verificaValorListaIgualX :: [Int] -> Int -> Bool
verificaValorListaIgualX [] _ = True
verificaValorListaIgualX (c:cs) x
		| c == x = verificaValorListaIgualX cs x
		| otherwise = False

contemAoMenosUm :: [Int] -> Int -> Bool
contemAoMenosUm [] _ = False
contemAoMenosUm (c:cs) x
		| c == x = True
		| otherwise = contemAoMenosUm cs x

somentePositivos :: [Int] -> [Int]
somentePositivos [] = []
somentePositivos (c:cs)
		|  c >= 0 = [c] ++ somentePositivos cs
		| otherwise = somentePositivos cs

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (c:cs) =  c * produtoLista cs

stringsCincoCaracteres :: [String] -> [String]
stringsCincoCaracteres [] = []
stringsCincoCaracteres (c:cs)
		| validaStringCinco c 0 = [c] ++ stringsCincoCaracteres cs
		| otherwise = stringsCincoCaracteres cs

validaStringCinco :: String -> Int -> Bool
validaStringCinco [] num
		| num >= 5 = True
		| otherwise = False
validaStringCinco (c:cs) num = validaStringCinco cs (num + 1)

retornaPares :: [Int] -> [Int]
retornaPares [] = []
retornaPares (h:t)
		| h `mod` 2 == 0 = [h] ++ retornaPares t
		| otherwise = retornaPares t

tamanhoStrings :: [String] -> [(String,Int)]
tamanhoStrings [] = []
tamanhoStrings (h:t) = let tam = contaString h in [(h,tam)] ++ tamanhoStrings t

contaString :: String -> Int
contaString [] = 0
contaString (h:t) = 1 + contaString t

mediaLista :: [Int] -> Float
mediaLista lista = fazOperacaoMediaLista lista 0 0

fazOperacaoMediaLista :: [Int] -> Int -> Int -> Float
fazOperacaoMediaLista [] soma cont = fromIntegral soma / fromIntegral cont
fazOperacaoMediaLista (h:t) soma cont = fazOperacaoMediaLista t (soma + h) (cont + 1)


listasEmComum :: [Int] -> [Int] -> [Int]
listasEmComum [] _ = []
listasEmComum (h:t) lista
		| verTaLista lista h == True = h : listasEmComum t lista
		| otherwise = listasEmComum t lista

verTaLista :: [Int] -> Int -> Bool
verTaLista [] _ = False
verTaLista (h:t) x
		| h == x = True
		| otherwise = verTaLista t x

removeRepetidos :: [Int] -> [Int]
removeRepetidos [] = []
removeRepetidos (h:t)
		| verTaLista t h == True = removeRepetidos t
		| otherwise = h : removeRepetidos t


listaStrPre :: [String] -> String -> [String]
listaStrPre [] _ = []
listaStrPre (h:t) x
		| prefixoStr h x == True = [h] ++ listaStrPre t x
		| otherwise = listaStrPre t x

prefixoStr :: String -> String -> Bool
prefixoStr _ [] = True
prefixoStr (h:t) (c:s) 
		| h == c = prefixoStr t s
		| otherwise = False

listaInversa :: [Int] -> [Int]
listaInversa [] = []
listaInversa (h:t) = listaInversa t ++ [h]

geraMatriz :: Int -> Int -> [[Int]]
geraMatriz 0 _ = []
geraMatriz l c = [geraColunas c] ++  geraMatriz (l-1) c

geraColunas :: Int -> [Int]
geraColunas 0 = []
geraColunas c = [0] ++ geraColunas (c-1)

type Matriz = [[Int]]

matrizExemplo :: Matriz
matrizExemplo = [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]

alteraMatriz :: [[Int]] -> Int -> Int -> [[Int]]
alteraMatriz [] _ _ = []
alteraMatriz (h:t) 1 c = [alteraColunaMat h c] ++ t
alteraMatriz (h:t) l c = [h] ++ alteraMatriz t (l-1) c


alteraColunaMat :: [Int] -> Int -> [Int]
alteraColunaMat [] _ = []
alteraColunaMat (h:t) 1 = [1] ++ t
alteraColunaMat (h:t) c = [h] ++ alteraColunaMat t (c-1)

menuSoma :: IO ()
menuSoma = do
    putStr "Entre com o primeiro numero: "
    x <- getLine
    putStr "Entre com o segundo numero: "
    y <- getLine
    let n1 = read x :: Int
        n2 = read y :: Int
    putStr "A soma e': "
    print (n1 + n2)

split :: String -> Char -> String -> [String]
split [] _ lista = [lista]  
split (h:t) x lista
    | h == x = lista : split t x [] 
    | otherwise = split t x (lista ++ [h]) 


percorreString :: String -> String
percorreString [] = ""
percorreString (h:t) = [h] ++ percorreString t

estaOrdenada :: [Int] -> Bool
estaOrdenada lista = verOrdem lista []

verOrdem :: [Int] -> [Int] -> Bool
verOrdem [] _ = True
verOrdem (h:t) [] = verOrdem t [h]
verOrdem (h:t) (x:z)
		| h > x = verOrdem t ( [h] ++ (x:z))
		| otherwise = False


palindromo :: String -> Bool 
palindromo lista = verPalindromo lista listaInversa
	where listaInversa = inversoStr lista

verPalindromo :: String -> String -> Bool 
verPalindromo [] [] = True
verPalindromo (h1:t1) (h2:t2)
		| h1 /= h2 = False
		| otherwise = verPalindromo t1 t2

inversoStr :: String -> String
inversoStr [] = ""
inversoStr (h:t) = inversoStr t ++ [h]

ordena :: [Int] -> [Int]
ordena lista = quicksort1 lista

quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
  let smallerSorted = quicksort1 [a | a <- xs, a <= x]
      biggerSorted = quicksort1 [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

lengthStr :: String -> Int
lengthStr [] = 0
lengthStr (h:t) = 1 + lengthStr t

strTam :: [String] -> [(String,Int)]
strTam [] = []
strTam (h:t) = [(h, lengthStr h)] ++ strTam t

concatenarLista :: [Int] -> [Int] -> [Int]
concatenarLista x y = x ++ y

henrique :: [Int] -> [(Int,Int)]
henrique lista = pedro lista 0

pedro :: [Int] -> Int -> [(Int,Int)]
pedro [] _ = []
pedro (c:cs) x = [(x,c)] ++ pedro cs (x+1)


-- Função auxiliar para verificar se um caractere é um operador
ehOperador :: Char -> Bool
ehOperador c = c `elem` ['+', '-', '*', '/']

-- Função auxiliar para executar uma operação aritmética
executarOperacao :: Char -> Float -> Float -> Float
executarOperacao '+' x y = x + y
executarOperacao '-' x y = x - y
executarOperacao '*' x y = x * y
executarOperacao '/' x y = x / y

-- Função principal que avalia a expressão
avalia :: String -> Float
avalia expressao = avaliarOperacoes expressaoSemEspacos [] []
  where
    -- Remove espaços em branco desnecessários
    expressaoSemEspacos = trace ("expressaoSemEspacos: " ++ filter (/= ' ') expressao) $ filter (/= ' ') expressao

    -- Função auxiliar para extrair um número da expressão
    extrairNumero :: String -> (Float, String)
    extrairNumero "" = (0.0, "")
    extrairNumero cs =
      let (numStr, resto) = span isDigit cs
       in (read numStr, resto)

    -- Função auxiliar para extrair uma subexpressão entre parênteses
    extrairSubexpressao :: String -> (String, String)
    extrairSubexpressao = extrairSubexpressaoAux 0
      where
        extrairSubexpressaoAux :: Int -> String -> (String, String)
        extrairSubexpressaoAux _ "" = ("", "")
        extrairSubexpressaoAux contador (c:cs)
          | c == '(' =
            let (subexp, resto) = extrairSubexpressaoAux (contador + 1) cs
             in (c : subexp, resto)
          | c == ')' =
            if contador == 0
              then ("", cs)
              else let (subexp, resto) = extrairSubexpressaoAux (contador - 1) cs
                    in (c : subexp, resto)
          | otherwise =
            let (subexp, resto) = extrairSubexpressaoAux contador cs
             in (c : subexp, resto)

    -- Função auxiliar para avaliar as operações
    avaliarOperacoes :: String -> [Float] -> [Char] -> Float
    avaliarOperacoes "" operandos [] = head operandos
    avaliarOperacoes "" operandos (op:ops) = avaliarOperacoes "" (executarOperacao op (head (tail operandos)) (head operandos) : drop 2 operandos) ops
    avaliarOperacoes (c:cs) operandos operadores
      | isDigit c =
        let (numero, resto) = extrairNumero (c:cs)
         in avaliarOperacoes resto (numero : operandos) operadores
      | c == '(' =
        let (subexp, resto) = extrairSubexpressao cs
            resultadoSubexp = avaliarOperacoes subexp [] []
         in avaliarOperacoes resto (resultadoSubexp : operandos) operadores
      | ehOperador c =
        if null operadores || (precedencia c > precedencia (head operadores))
          then avaliarOperacoes cs operandos (c:operadores)
          else let novoOperando = executarOperacao (head operadores) (head (tail operandos)) (head operandos)
                in avaliarOperacoes (c:cs) (novoOperando : drop 2 operandos) (tail operadores)
      | otherwise = error "Caractere inválido na expressão"
      where
        precedencia :: Char -> Int
        precedencia '+' = 1
        precedencia '-' = 1
        precedencia '*' = 2
        precedencia '/' = 2
        precedencia _ = 0
