{-
  Exercícios para a Aula Laboratorial 1

  Escrever um interpretador em Haskell da sintaxe abstrata de
  programas sequenciais.

  Pedro Vasconcelos, 2020.

  Baseado num exercício do livro "Modern Compiler Implementation in
  ML", A. Appel.
-}

module Interpreter where

--
-- sintaxe abstrata de programas sequenciais
--
type Ident = String  -- identificadores (nomes de variaveis)

data BinOp = Plus | Minus | Times | Div -- operações binárias
           deriving (Eq, Show)

data Stm = AssignStm Ident Exp   -- ident = exp
         | IncrStm Ident         -- ident++
         | CompoundStm Stm Stm   -- stm1; stm2
         deriving (Eq, Show)

data Exp = IdExp Ident           -- x, y, z ..
         | NumExp Int            -- 123
         | OpExp Exp BinOp Exp   -- e1+e2, e1*e2, ...
         | SeqExp Stm Exp        -- (stm, e)
         deriving (Eq, Show)


{- Exercício 1.

Escrever duas funções recursivas para colecionar todos os identificadores
de comandos e expressões.

NOTA: escreva uma equação para cada construtor da sintaxe abstrata
acima. As duas funções devem chamar-se mutuamente porque os comandos
contêm sub-expressões mas as expressões também podem conter
sub-comandos.
-}

idsStm :: Stm -> [Ident]
idsStm AssignStm a b = a ++ "=" ++ (idsExp b)
idsStm IncrStm a = a++ "++"  -- ou só "[a]"
idsStm CompoundStm a b = (idsStm a) ++ (idsStm b)
idsStm _
  = error "completar esta definição"

idsExp :: Exp -> [Ident]
idsExp IdExp a = [a]
idsExp NumExp a = (Show a)
idsExp OpExp a b c = (idsExp a) ++ [b] ++ (idsExp c)
idsExp SeqExp a b = (idsExp a) ++ (idsExp b)
idsExp _
  = error "completar esta definição"

-- NB: o que acontece se um identificador ocorrer mais do que uma vez?


{- Exercício 2: um interpretador funcional

Escreva duas funções mutuamente recursivas para interpretar comandos
e expressões.

Represente tabelas associações de valores (inteiros) aos
identificadores como listas de pares.
Por exemplo, a lista [("x", 2), ("y", 0)] associa x -> 2, y -> 0.

Sugestões: use a função do prelúdio

lookup :: Eq a => a -> [(a,b)] -> Maybe b

para procurar o valor (se existir) associado a um identificador.
-}

type Table = [(Ident, Int)]


interpStm :: Stm -> Table -> Table
interpStm _ _
  = error "completar esta definição"


interpExp :: Exp -> Table -> (Int, Table)
interpExp _ _
  = error "completar esta definição"



{- Exercício 3 (extra):

Modifique a definição do tipo Table and das funções do interpretador
de forma usar to uma estrutura de dados para associações mais eficiente;
por exemplo, Data.Map (package containers)
ou Data.HashMap (package unordered-containers).

Para evitar conflitos de nomes, deve importar o módulo Data.Map
com nomes qualificados:

import qualified Data.Map as Map

Depois use nomes como as Map.Map para o tipo e Map.empty, Map.lookup,
Map.insert, etc. para as operações.

Documentação:

http://hackage.haskell.org/package/containers
http://hackage.haskell.org/package/unordered-containers
-}
