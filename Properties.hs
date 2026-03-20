import Bank
import Generators
import Test.QuickCheck
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

-- Uma data não pode ser no futuro

--formatDate :: -> Date
--formatDate = getCurrentTime 

--pop_movDate :: Extracto -> Bool
--prop_movDate (Ext _ ((d, _, _): t)) | formatDate getCurrentTime
