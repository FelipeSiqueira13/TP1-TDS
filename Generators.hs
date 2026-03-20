module Generators where

import Bank
import Data.Ratio ((%))
import Test.QuickCheck
----------------------------------------  GERADORES ----------------------------------------

genMovimento :: Gen Movimento
genMovimento = do
                tipo <- frequency[(75, return Debito),(25, return Credito)]
                valor <- choose(1,50000)
                return $ tipo (fromRational (valor % 100))

genData :: Gen Data
genData = do
        mes <- elements[1..12]
        ano <- elements[2000..2026]
        let maxDia = if mes == 2 && (ano `mod` 4) == 0 then 29
                   else if mes == 2 && (ano `mod` 4) /= 0 then 28
                   else if elem mes [1,3,5,7,8,10,12] then 31
                   else 30
        dia <- elements[1..maxDia]
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
            
            
