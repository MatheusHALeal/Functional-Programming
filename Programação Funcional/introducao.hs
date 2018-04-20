{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}



xor a b = undefined
impl a b = undefined
equiv a b = undefined

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow :: Int -> Int -> Int
pow x 1 = x
pow x y = x * pow x (y-1)


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}

fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}

isPrime :: Int -> Bool
isPrime x = do
 let list = [a| a <- [2..x], x`mod`a == 0]
 (head list) == (last list)


{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc :: Int -> Int -> Int
mdc x 0 = x
mdc 0 y = y
mdc x y
 |x > y = mdc y (x`mod`y) 
 |otherwise = mdc x (y`mod`x)
{-
- Calcula um MMC de dois numeros. 
-}
mmc :: Int -> Int -> Int
mmc 0 y = 0
mmc x 0 = 0
mmc x y = div (x * y) (mdc x y)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y
 | mdc x y == 1 = True
 | otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = undefined


main:: IO()
main = do
 a <- getLine
 b <- getLine
 let result = coprimo (read a) (read b)
 print result

