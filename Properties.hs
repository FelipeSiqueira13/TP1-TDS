import Bank
import Generators
import Test.QuickCheck
import Data.Time.Clock
import Data.Time.Calendar (toGregorian)

---------------------------------------- PROPRIEDADES ----------------------------------

-- Os floats devem ter no máximo duas casas décimais

-- prop_float :: Float -> Bool
-- prop_float x = 

------- movimento -------------------------------------------------------------------------------

-- Os movimentos não podem ser negativos nem 0

prop_movPos :: Movimento -> Bool 
prop_movPos (Credito x) = x > 0
prop_movPos (Debito x) = x > 0

-- um extrato não pode ter movimntos negativos. isto é preciso sendo que já temos a prop_movPos ???????
prop_creDebPos :: Extracto -> Bool
prop_creDebPos e = (fst $ creDeb e) >= 0 && (snd $ creDeb e) >= 0

------- data -------------------------------------------------------------------------------

-- Máximos de dias num mês

prop_maxDate :: Data -> Bool
prop_maxDate (D d m a) | m == 2 && (a `mod` 4) == 0 = d<=29
                       | m == 2 && (a `mod` 4) /= 0 = d<=28
                       | elem m [1,3,5,7,8,10,12] = d<=31
                       | otherwise = d<=30

-- Mínimo de dias num mês

prop_minDate :: Data -> Bool
prop_minDate (D d m a) = d>=1

{-
PARA USAR O GETCURRENT TIME É PRECISO USAR IOS,PARA NÃO OS USAR TERIA QUE DAR COMO PARAMENTRO DO PROP_MOVDATE A DATA ATUAL
FAZER ISTO OU IGNORAR???????

-- Uma data não pode ser no futuro

formatDate :: UTCTime -> Data
formatDate time = D (fromIntegral dia) mes (fromIntegral ano)
    where
        (ano, mes, dia) = toGregorian (utctDay time)

prop_movDate :: Extracto -> IO Bool
prop_movDate (Ext _ ((date, _, _): t)) =
            let (D td tm ta) = formatDate getCurrentTime 
                (D d m a) = date
            in if a <= ta then m <= tm && d <= td else False
-}

------- extrato -------------------------------------------------------------------------------

-- há mais movimentos que corrrespondem a débitos do que a créditos

prop_MoreDebOrCred :: Extracto -> Bool -- está a falhar
prop_MoreDebOrCred (Ext _ ops) = debitos >= creditos
                    where
                        movimentos = [m | (_, _, m) <- ops]
                        creditos = length [m | m <- movimentos, isCred m]
                        debitos = length movimentos - creditos

-- todos os movimentos num extrato têm de ser feitos no mesmo mês e ano

prop_extractDate :: Extracto -> Bool
prop_extractDate (Ext _ (((D d m a), _, _) : [])) = True
prop_extractDate (Ext _ (((D d m a), _, _): t)) = and (map (==m) months)
                    where
                        months = [m | ((D d m a), _, _) <- t]
------- extratos -------------------------------------------------------------------------------

-- o valor inicial do extrato seguinte deve corresponder ao resultado da função saldo do extrato anterior

prop_extractValues :: Extractos -> Bool
prop_extractValues (Extractos (h:[])) = True
prop_extractValues (Extractos (e:(Ext s _):t)) = duasCasas (saldo e) == duasCasas s