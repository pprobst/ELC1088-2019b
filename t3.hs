-------------------------------------------------------------
-- T3 - Streams e Expressões Lambda
-- Pedro Probst Minini | 201710013
-- ELC1088 - Implementação de Linguagens de Programação
-------------------------------------------------------------
--
-- ------------------------------
-- Motivação de fazer em Haskell:
-- ------------------------------
-- Haskell é uma linguagem puramente funcional e a API
-- Streams do Java apenas implementa várias funcionalidades
-- já presentes em linguagens funcionais como Haskell.
-- Como o autor deste trabalho gostaria de relembrar um pouco
-- de Haskell, a escolha foi natural...
--
-- -------------------------------
-- Como testar as funções criadas:
-- -------------------------------
-- O modo mais direto é pelo ghci:
-- $ ghci
-- $ :load t3
-- Executar função desejada, por exemplo:
-- $ calcS
--
-------------------------------------------------------------

import Data.List
import Control.Monad(replicateM)

-- 1
-- Faça um programa que calcule e escreva o resultado de S:
-- s = 1/1 + 3/2 + 5/3 + 7/4 + ... + 99/50
calcS = sum [x/y | (x,y) <- zip [1,3..99] $ [1..50]]

-- 2 
-- Faça um programa que solicite ao usuário para digitar
-- valores numéricos inteiros positivos. Encerre a entrada
-- de dados quando for digitado um número negativo ou 0.
-- Calcula a média dos números positivos digitados.
doWhile :: Monad m => (a -> Bool) -> m a -> m [a]
doWhile p a = do
    x <- a
    if p x
        then (x:) <$> doWhile p a
        else return []

average xs = realToFrac (sum xs) / genericLength xs
calcMedia = doWhile (> 0) readLn >>= print . average

-- 3
-- Faça um programa que leia 10 números inteiros e, ao final,
-- mostre a quantidade de números ímpares e pares lidos.
-- Calcule também a soma dos números pares e a média dos
-- números ímpares.
sumEvenMeanOdd = do
    putStrLn "Insira 10 números:"
    numbers <- replicateM 10 readLn
    print (sum (filter even numbers), average (filter odd numbers))

-- 4
-- Uma empresa armazena para cada funcionário (10 no total) uma
-- ficha contendo o CÓDIGO, o Nº DE HORAS TRABALHADAS e o seu
-- Nº DE DEPENDENTES. Empresa paga R$12,00 por hora, e R$40,00
-- por dependente. Desconta 8,5% para INSS e 5% para IR.
-- Após ler os 10 funcionários, mostre:
-- > o código
-- > os valores descontados para cada imposta
-- > o salário líquido de cada funcionário
--
-- OBS.: por questões de simplificação, foram considerados
-- apenas 2 funcionários...
chunksOf n xs = takeWhile (not.null) $ unfoldr (Just . splitAt n) xs
discount n xs = map (\p -> p*(n/100)) xs

infoFunc = do
    putStrLn "Insira na ordem CÓDIGO, HORAS, DEPENDENTES:"
    info <- replicateM 6 readLn
    let lst = chunksOf 3 info
    let salaries = map (\p -> p!!1*12 + p!!2*40) lst
    let discounts = [(x, y) | (x, y) <- zip (discount 8.5 salaries) $ (discount 5 salaries)]
    let discounted_salaries = zip discounts $ (map (\p -> fst p - sum(snd p)) (zip salaries $ discounts))
    let res = [(x, y) | (x, y) <- zip (map(\p -> p!!0) lst) $ discounted_salaries]
    putStrLn "[(cód, (inss,ir), salario)), (cód, (inss, ir), salario)), ...]"
    print res


-- Por infortúnio, o autor deste trabalho também relembrou o quão infernal é I/O em Haskell...
