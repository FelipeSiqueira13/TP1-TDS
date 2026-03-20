module Bank where

import Data.List
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Function

data Movimento = Credito Float
                | Debito Float
                deriving Show

data Data = D Int Int Int
            deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
                deriving Show

data Extractos = Extractos [Extracto]
                deriving Show


-- 1. Construa a função extValor :: Extracto -> Float -> [Movimento] que produz uma lista de todos os movimentos (créditos ou débitos) superiores a um determinado valor.

getValor :: Movimento -> Float
getValor (Credito v) = v
getValor (Debito v) = v

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext f []) f1 = []
extValor (Ext f ((d, s, m) : h)) f1 | getValor m > f1 = [m] ++ extValor (Ext f h) f1
                                    | otherwise = extValor (Ext f h) f1


-- 2. Defina a função filtro :: Extracto -> [String] -> [(Data,Movimento)] que retorna informação relativa apenas aos movimentos cuja descrição esteja incluída na lista fornecida no segundo parâmetro.

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext f []) _ = []
filtro (Ext f _) [] = []
filtro (Ext f ((d, s, m) : h)) sl | elem s sl = [(d,m)] ++ filtro (Ext f h) sl
                                  | otherwise = filtro (Ext f h) sl

 
-- 3. Defina a função creDeb :: Extracto -> (Float,Float), que retorna o total de créditos e de débitos de um extracto no primeiro e segundo elementos de um par, respectivamente.
isCred :: Movimento -> Bool
isCred (Credito _) = True
isCred (Debito _) = False

sumFF :: (Float,Float) -> (Float,Float) -> (Float,Float)
sumFF (a1, a2) (b1, b2) = (a1+b1,a2+b2)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext f []) = (0,0)
creDeb (Ext f ((d,s,m) : h)) | isCred m = sumFF (creDeb (Ext f h)) (getValor m,0)
                             | otherwise = sumFF (creDeb (Ext f h)) (0,getValor m)

-- 4. Defina a função saldo :: Extracto -> Float que devolve o saldo final que resulta da execução de todos os movimentos no extracto sobre o saldo inicial.
saldo :: Extracto -> Float
saldo (Ext f []) = f
saldo (Ext f ((d,s,m) : h)) | isCred m = saldo (Ext f h) + getValor m
                            | otherwise = saldo (Ext f h) - getValor m 


{- 
Exercício ------------------------------------------------------------------------------------
Utilizando a biblioteca QuickCheck em Haskell ou Hypothesis em Python, defina propriedades que os extratos bancário devem obdecer. 
Para definir propriedades interessantes poderá ter necessidade de definir algumas funções sobre extratos bancários semelhantes às que
são pedidas na UC de programação funcional.
Defina ainda geradores de modo a produzir extratos o mais semelhantes possível com extratos bancários reais. Por exemplo, num extrato típico há mais movimentos que corrrespondem a débitos do que a créditos (que tipicamente pode ser apenas um: o salário do titular da conta bancária).
Tenha ainda em conta que a lista de extratos Extratos tem noção de ordem, isto é, o valor inicial do extrato seguinte deve corresponder ao resultado da função saldo do extrato anterior.
-}