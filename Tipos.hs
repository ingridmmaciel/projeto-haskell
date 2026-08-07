module Tipos where

data Categoria =
  Alimentacao |
  Transporte  |
  Saude       |
  Lazer       |
  Educacao    |
  Moradia     |
  Vestuario   |
  Outros      deriving (Show, Eq, Enum, Bounded)

data TipoTransacao =
  Receita |
  Despesa deriving (Show, Eq)

data Transacao = Transacao {
  categoria :: Categoria,
  tipo      :: TipoTransacao,
  valor     :: Double
} deriving (Show, Eq)

data ResumoFinanceiro = ResumoFinanceiro {
  totalReceitas :: Double,
  totalDespesas :: Double,
  saldo         :: Double,
  mediaDespesas :: Double
} deriving (Show, Eq)