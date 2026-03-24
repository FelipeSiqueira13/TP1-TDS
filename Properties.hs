import Bank
import Generators
import Test.QuickCheck
import Data.List

---------------------------------------- PROPRIEDADES ----------------------------------
  
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

-- Meses válidos
prop_validMonth :: Data -> Bool
prop_validMonth (D _ m _) = m>=1 && m<=12

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
prop_extractDate (Ext _ (((D _ m _), _, _) : [])) = True
prop_extractDate (Ext _ (((D _ m _), _, _): t)) = and (map (==m) months)
                    where
                        months = [m | ((D _ m _), _, _) <- t]

-- os movimentos devem estar organizados por ordem cronológica

prop_extractDateOrd :: Extracto -> Bool 
prop_extractDateOrd (Ext _ (((D d _ _), _, _) : [])) = True 
prop_extractDateOrd (Ext _ l) = and (zipWith (>=) ds (tail ds))
                                where ds = [d | (d, _, _) <- l]

------- extratos -------------------------------------------------------------------------------

-- o valor inicial do extrato seguinte deve corresponder ao resultado da função saldo do extrato anterior
prop_extractValues :: Extractos -> Bool
prop_extractValues (Extractos []) = True
prop_extractValues (Extractos [h]) = True
prop_extractValues (Extractos (e:(Ext s l):t)) = duasCasas (saldo e) == duasCasas s && prop_extractValues (Extractos ((Ext s l):t))

------- funções bank.hs -------------------------------------------------------------------------------

-- extValor

-- o resultado de extValor é uma lista vazia quando o valor for maior que todos os movimentos do extrato
prop_extValor :: Extracto -> Bool
prop_extValor (Ext f []) = True
prop_extValor (Ext f l) = length(extValor (Ext f l) (maxMovimento)) == 0
                    where 
                        maxMovimento = maximum (map (\(_, _, m) -> getValor m) l)

-- o comprimento da lista resultante de extValor não ultrapassa o total de movimentos do extrato
prop_extValorTotal :: Extracto -> Float -> Bool
prop_extValorTotal (Ext f []) x = True 
prop_extValorTotal (Ext f l) x = length (extValor (Ext f l) x) <= length l


-- filtro

-- se a lista for vazia, o resultado deve ser uma lista vazia
prop_filtroEmpty :: Extracto -> Bool 
prop_filtroEmpty e = length(filtro e []) == 0

-- a ordem não importa 
prop_filtroOrder :: Extracto -> [String] -> Bool
prop_filtroOrder e sl = length (filtro e sl) == length (filtro e (reverse sl))


-- creDeb

-- a soma do par (cred,deb) deve ser a soma de todos os valores de um extrato
prop_creDebSum :: Extracto -> Bool
prop_creDebSum (Ext f l) = duasCasas(fst (cd) + snd (cd)) == duasCasas(sum (map (getValor . movs) l))
                        where
                            cd = creDeb (Ext f l)
                            movs = \(_, _, m) -> m


-- saldo

-- se a lista de movimentos for vazia, o saldo deve ser o saldo original
prop_saldoEmpty :: Extracto -> Bool
prop_saldoEmpty (Ext f l) | length(l) == 0 = saldo ((Ext f l)) == f 
                          | otherwise = True

-- o resultado da função saldo deve ser o saldo inicial após os movimentos do extracto
prop_saldo :: Extracto -> Bool 
prop_saldo (Ext f l) = duasCasas(saldo((Ext f l))) == duasCasas(f + c - d)
                    where (c,d) = creDeb((Ext f l))

