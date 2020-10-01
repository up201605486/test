{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Interpreter
import Test.QuickCheck
import Data.List (sort, nub)

-- algums programas sequênciais para testes

-- a = 5+3; b = a-2;
example1 =
  CompoundStm
  (AssignStm "a"
    (OpExp (NumExp 5) Plus (NumExp 3))
  )
  (AssignStm "b"
    (OpExp (IdExp "a") Minus (NumExp 2))
  )

  
-- a = 5+3; b = (a++, a*2);
example2 =
  CompoundStm
  (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
  (AssignStm "b"
    (SeqExp
      (IncrStm "a")
      (OpExp (IdExp "a") Times (NumExp 2))
    )
  )

-- a = 2; b = (a++, a+1); c = b+1;
example3 =
  CompoundStm
  (AssignStm "a" (NumExp 2))
  (CompoundStm
    (AssignStm "b" (SeqExp
                    (IncrStm "a")
                    (OpExp (IdExp "a") Plus (NumExp 1))))
    (AssignStm "c" (OpExp (IdExp "b") Plus (NumExp 1))))


--- testes sobre a função que lista identificadores;
--- usamos `nub' para eliminar possíveis repetidos
prop_idents_example1 =
  nub (idsStm example1) === ["a", "b"]

prop_idents_example2 =
  nub (idsStm example2) === ["a", "b"]

prop_idents_example3 =
  nub (idsStm example3) === ["a", "b", "c"]


--- testes do interpretador usando os exemplos 1-3;
--- usamos `sort` para ignorar a ordem de resultados
prop_interp_example1 =
  sort (interpStm example1 []) === [("a",8), ("b", 6)]

prop_interp_example2 =
  sort (interpStm example2 []) === [("a",9), ("b", 18)]
      
prop_interp_example3 =
  sort (interpStm example3 []) === [("a", 3), ("b", 4), ("c", 5)]


---------------------------------------------------------------------------  

return []
runTests = $(quickCheckAll)
