module Generators where

import Bank
import Data.Ratio ((%))
import Test.QuickCheck
----------------------------------------  GERADORES ----------------------------------------

genDinheiro :: Gen Float
genDinheiro = do
    v <- choose (1, 50000) :: Gen Integer
    return (duasCasas (fromRational (v % 100)))

genDinheiro' :: Float -> Gen Float
genDinheiro' limite
    | limite <= 0 = return 0
    | otherwise = do
        let maxCentavos = max 1 (floor (limite * 100))
        v <- choose (1, maxCentavos) :: Gen Integer
        return (duasCasas (fromRational (v % 100)))


genMovimento :: Gen Movimento
genMovimento = do
                tipo <- frequency[(75, return Debito),(25, return Credito)]
                valor <- genDinheiro
                return $ tipo valor

genMovimento' :: Float -> Gen Movimento
genMovimento' dinheiro
    | dinheiro < 10 = do
        valor <- genDinheiro
        return (Credito valor)
    | otherwise = do
        isDebito <- frequency [(75, return True), (25, return False)]
        if isDebito
            then do
                valor <- genDinheiro' (dinheiro - 0.1)
                return (Debito valor)
            else do
                valor <- genDinheiro
                return (Credito valor)

saldoAposMovimento :: Float -> Movimento -> Float
saldoAposMovimento dinheiro (Credito v) = duasCasas (dinheiro + v)
saldoAposMovimento dinheiro (Debito v) = duasCasas (max 0 (dinheiro - v))

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

genDia :: Int -> Int -> Gen Data
genDia mes ano = do
        let maxDia = if mes == 2 && (ano `mod` 4) == 0 then 29
                   else if mes == 2 && (ano `mod` 4) /= 0 then 28
                   else if elem mes [1,3,5,7,8,10,12] then 31
                   else 30
        dia <- elements[1..maxDia]
        return $ D dia mes ano



genOperacoes :: Int -> Int -> Int -> Float -> Gen [(Data, String, Movimento)]
genOperacoes s mes ano dinheiro
    | s <= 0 = return []
    | otherwise = do
        datah <- genDia mes ano
        str <- vectorOf 5 (elements ['A'..'Z'])
        mov <- genMovimento' dinheiro
        let novoDinheiro = saldoAposMovimento dinheiro mov
        rest <- frequency [(1, return []), (s, genOperacoes (s - 1) mes ano novoDinheiro)]
        return ((datah, str, mov) : rest)

instance Arbitrary Extracto where
    arbitrary = sized genExtracto
genExtracto :: Int -> Gen Extracto
genExtracto s = do
            dinIni <- genDinheiro
            mes <- elements[1..12]
            ano <- elements[2000..2026]
            lista <- genOperacoes s mes ano dinIni
            return (Ext dinIni lista)

genExtracto' :: Int -> Int -> Int -> Float -> Gen Extracto
genExtracto' s mes ano dinIni = do
            lista <- genOperacoes s mes ano dinIni
            return (Ext dinIni lista)
            
instance Arbitrary Extractos where
    arbitrary = sized genExtractos

genExtractos :: Int -> Gen Extractos
genExtractos s = do
    mes <- elements[1..12]
    ano <- elements[2000..2026]
    dinIni <- genDinheiro
    xs <- genExtractos' s mes ano dinIni
    return (Extractos xs)

genExtractos' :: Int -> Int -> Int -> Float -> Gen [Extracto]
genExtractos' 0 mes ano dinIni = return []
genExtractos' s mes ano dinIni = do
    atual <- genExtracto' 3 mes ano dinIni
    let mesatual = if 12 == mes then 1 else mes + 1
    let anoatual = if 12 == mes then ano + 1 else ano
    let novoSaldo = duasCasas (saldo atual)
    resto <- genExtractos' (s - 1) mesatual anoatual novoSaldo
    return (atual : resto)