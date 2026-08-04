module Main
  ( main
  , Categoria(..)
  , TipoTransacao(..)
  , Transacao(..)
  , ResumoFinanceiro(..)
  , todasCategorias
  , transacaoParaResumo
  , calcularResumo
  , formatarTransacoes
  , removerPorIndice
  ) where

import Text.Read (readMaybe)
import System.IO (hSetEncoding, stdin, stdout, utf8)

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
  saldo         :: Double
} deriving (Show, Eq)

-- Instanciação de Monoid (bônus): combinar dois resumos parciais soma os campos
instance Semigroup ResumoFinanceiro where
  (ResumoFinanceiro r1 d1 s1) <> (ResumoFinanceiro r2 d2 s2) =
    ResumoFinanceiro (r1 + r2) (d1 + d2) (s1 + s2)

instance Monoid ResumoFinanceiro where
  mempty = ResumoFinanceiro 0 0 0

-- Todas as categorias possíveis (Enum/Bounded)
todasCategorias :: [Categoria]
todasCategorias = [minBound .. maxBound]

-- Converte uma transação individual em um "resumo parcial"
transacaoParaResumo :: Transacao -> ResumoFinanceiro
transacaoParaResumo t = case tipo t of
  Receita -> ResumoFinanceiro (valor t) 0 (valor t)
  Despesa -> ResumoFinanceiro 0 (valor t) (negate (valor t))

-- Calcula o resumo financeiro combinando todas as transações via Monoid (foldMap)
calcularResumo :: [Transacao] -> ResumoFinanceiro
calcularResumo = foldMap transacaoParaResumo

-- Formata as transações com índice (1-based) para exibição -- list comprehension
formatarTransacoes :: [Transacao] -> [String]
formatarTransacoes transacoes =
  [ show i ++ " - " ++ show (tipo t) ++ " | " ++ show (categoria t) ++
    " | R$ " ++ show (valor t)
  | (i, t) <- zip [1 :: Int ..] transacoes ]

-- Remove o elemento de índice (1-based) via recursão explícita (tail recursion)
removerPorIndice :: Int -> [Transacao] -> [Transacao]
removerPorIndice _ [] = []
removerPorIndice indice (t:ts)
  | indice == 1 = ts
  | indice < 1  = t : ts
  | otherwise   = t : removerPorIndice (indice - 1) ts

-- Imprime cada linha da lista, uma por vez, percorrendo recursivamente
imprimirLinhas :: [String] -> IO ()
imprimirLinhas [] = return ()
imprimirLinhas (linha:resto) = do
  putStrLn linha
  imprimirLinhas resto

listarTransacoes :: [Transacao] -> IO ()
listarTransacoes [] = putStrLn "Nenhuma transação cadastrada."
listarTransacoes transacoes = imprimirLinhas (formatarTransacoes transacoes)

-- Exibe cada categoria numerada, percorrendo a lista recursivamente
exibirCategoriasNumeradas :: [(Int, Categoria)] -> IO ()
exibirCategoriasNumeradas [] = return ()
exibirCategoriasNumeradas ((indice, cat):resto) = do
  putStrLn (show indice ++ " - " ++ show cat)
  exibirCategoriasNumeradas resto

lerCategoria :: IO Categoria
lerCategoria = do
  putStrLn "Categorias disponíveis:"
  exibirCategoriasNumeradas (zip [1 :: Int ..] todasCategorias)
  putStr "Escolha o número da categoria: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Int of
    Just n | n >= 1 && n <= length todasCategorias -> return (todasCategorias !! (n - 1))
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      lerCategoria

lerTipoTransacao :: IO TipoTransacao
lerTipoTransacao = do
  putStrLn "1 - Receita"
  putStrLn "2 - Despesa"
  putStr "Escolha o tipo: "
  entrada <- getLine
  case entrada of
    "1" -> return Receita
    "2" -> return Despesa
    _   -> do
      putStrLn "Opção inválida, tente novamente."
      lerTipoTransacao

lerValor :: IO Double
lerValor = do
  putStr "Informe o valor: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Double of
    Just v | v > 0 -> return v
    _ -> do
      putStrLn "Valor inválido, informe um número positivo."
      lerValor

adicionarTransacao :: [Transacao] -> IO [Transacao]
adicionarTransacao transacoes = do
  cat <- lerCategoria
  tp  <- lerTipoTransacao
  vl  <- lerValor
  let novaTransacao = Transacao { categoria = cat, tipo = tp, valor = vl }
  putStrLn "Transação adicionada com sucesso!"
  return (novaTransacao : transacoes)

removerTransacao :: [Transacao] -> IO [Transacao]
removerTransacao [] = do
  putStrLn "Não há transações para remover."
  return []
removerTransacao transacoes = do
  listarTransacoes transacoes
  putStr "Informe o número da transação a remover: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Int of
    Just n | n >= 1 && n <= length transacoes -> do
      putStrLn "Transação removida com sucesso!"
      return (removerPorIndice n transacoes)
    _ -> do
      putStrLn "Índice inválido."
      return transacoes

exibirResumo :: [Transacao] -> IO ()
exibirResumo transacoes = do
  let resumo = calcularResumo transacoes
  putStrLn "------ Resumo Financeiro ------"
  putStrLn ("Total de receitas: R$ " ++ show (totalReceitas resumo))
  putStrLn ("Total de despesas: R$ " ++ show (totalDespesas resumo))
  putStrLn ("Saldo: R$ " ++ show (saldo resumo))

displayInstrucoes :: IO ()
displayInstrucoes = do
  putStrLn "------------------------------"
  putStrLn "1 - Listar transações"
  putStrLn "2 - Adicionar transação"
  putStrLn "3 - Remover transação"
  putStrLn "4 - Gerar resumo financeiro"
  putStrLn "5 - Sair"
  putStrLn "------------------------------"
  putStr "Escolha uma opção: "

loopSistema :: [Transacao] -> IO ()
loopSistema transacoes = do
  displayInstrucoes
  opcao <- getLine
  case opcao of
    "1" -> do
      listarTransacoes transacoes
      loopSistema transacoes
    "2" -> do
      novasTransacoes <- adicionarTransacao transacoes
      loopSistema novasTransacoes
    "3" -> do
      novasTransacoes <- removerTransacao transacoes
      loopSistema novasTransacoes
    "4" -> do
      exibirResumo transacoes
      loopSistema transacoes
    "5" -> putStrLn "Saindo do sistema. Até logo!"
    _   -> do
      putStrLn "Opção inválida. Tente novamente."
      loopSistema transacoes

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  putStrLn "Sistema de Controle Financeiro"
  loopSistema []
