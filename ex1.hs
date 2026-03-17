import Data.List
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen (genFloat)
import Foreign (free)
import Data.Ratio 

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
Exerc´ıcio
Utilizando a biblioteca QuickCheck em Haskell ou Hypothesis em Python, defina propriedades que os extratos banc´ario devem obdecer. Para definir propriedades interessantes
poder´a ter necessidade de definir algumas fun¸c˜oes sobre extratos banc´arios semelhantes `as que
s˜ao pedidas na UC de programa¸c˜ao funcional.
Defina ainda geradores de modo a produzir extratos o mais semelhantes poss´ıvel com extratos banc´arios reais. Por exemplo, num extrato t´ıpico h´a mais movimentos que corrrespondem
a d´ebitos do que a cr´editos (que tipicamente pode ser apenas um: o sal´ario do titular da conta
banc´aria).
Tenha ainda em conta que a lista de extratos Extratos tem no¸c˜ao de ordem, isto ´e, o valor
inicial do extrato seguinte deve corresponder ao resultado da fun¸c˜ao saldo do extrato anterior.
-}


genMovimento :: Gen Movimento
genMovimento = do
                tipo <- frequency[(50, return Debito),(50, return Credito)]
                valor <- choose(1,50000)
                return $ tipo (fromRational (valor % 100))

genData :: Gen Data
genData = do
        dia <- elements[1..31]
        mes <- elements[1..12]
        ano <- elements[2000..2026]
        return $ D dia mes ano

genDinheiro :: Gen Float
genDinheiro = do
    v <- choose (1, 50000) :: Gen Integer
    return (fromRational (v % 100))


genOperacoes :: Int -> Gen [(Data, String, Movimento)]
genOperacoes s = do
    datah <- genData
    str   <- vectorOf 5 (elements ['A'..'Z'])
    mov   <- genMovimento
    rest  <- frequency[(1, return []),(s, genOperacoes (s - 1))]
    return ((datah, str, mov) : rest)

instance Arbitrary Extracto where
    arbitrary = sized genExtracto
genExtracto :: Int -> Gen Extracto
genExtracto s = do
            dinIni <- genDinheiro
            lista <- genOperacoes s
            return (Ext dinIni lista)
            
instance Arbitrary Extractos where
    arbitrary = sized genExtractos

genExtractos :: Int -> Gen Extractos
genExtractos s = do
    xs <- genExtractos' (s-1)
    return (Extractos xs)

genExtractos' :: Int -> Gen [Extracto]
genExtractos' s = do
    atual <- genExtracto 3
    resto <- frequency [(1, return []), (s, genExtractos' (s - 1))]
    return (atual : resto)
            
            