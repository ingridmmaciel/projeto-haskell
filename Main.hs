module Main
  ( main
  , todasCategorias
  , transacaoParaResumo
  , calcularResumo
  , formatarTransacoes
  , removerPorIndice
  ) where

import Text.Read (readMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Tipos
import Estatisticas

-- Instanciacao de Monoid (bonus): combinar dois resumos parciais soma os campos
instance Semigroup ResumoFinanceiro where
  (ResumoFinanceiro r1 d1 s1) <> (ResumoFinanceiro r2 d2 s2) =
    ResumoFinanceiro (r1 + r2) (d1 + d2) (s1 + s2)

instance Monoid ResumoFinanceiro where
  mempty = ResumoFinanceiro 0 0 0

-- Todas as categorias possiveis (Enum/Bounded)
todasCategorias :: [Categoria]
todasCategorias = [minBound .. maxBound]

-- Converte uma transacao individual em um "resumo parcial"
transacaoParaResumo :: Transacao -> ResumoFinanceiro
transacaoParaResumo t = case tipo t of
  Receita -> ResumoFinanceiro (valor t) 0 (valor t)
  Despesa -> ResumoFinanceiro 0 (valor t) (negate (valor t)) 

-- Calcula o resumo financeiro combinando todas as transacoes via Monoid (foldMap)
calcularResumo :: [Transacao] -> ResumoFinanceiro
calcularResumo = foldMap transacaoParaResumo

-- Formata as transacoes com indice (1-based) para exibicao -- list comprehension
formatarTransacoes :: [Transacao] -> [String]
formatarTransacoes transacoes =
  [ show i ++ " - " ++ show (tipo t) ++ " | " ++ show (categoria t) ++
    " | R$ " ++ show (valor t)
  | (i, t) <- zip [1 :: Int ..] transacoes ]

-- Remove o elemento de indice (1-based) via recursao explicita (tail recursion)
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
listarTransacoes [] = putStrLn "Nenhuma transacao cadastrada."
listarTransacoes transacoes = imprimirLinhas (formatarTransacoes transacoes)

-- Exibe cada categoria numerada, percorrendo a lista recursivamente
exibirCategoriasNumeradas :: [(Int, Categoria)] -> IO ()
exibirCategoriasNumeradas [] = return ()
exibirCategoriasNumeradas ((indice, cat):resto) = do
  putStrLn (show indice ++ " - " ++ show cat)
  exibirCategoriasNumeradas resto

lerCategoria :: IO Categoria
lerCategoria = do
  putStrLn "Categorias disponiveis:"
  exibirCategoriasNumeradas (zip [1 :: Int ..] todasCategorias)
  putStr "Escolha o numero da categoria: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Int of
    Just n | n >= 1 && n <= length todasCategorias -> return (todasCategorias !! (n - 1))
    _ -> do
      putStrLn "Opcao invalida, tente novamente."
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
      putStrLn "Opcao invalida, tente novamente."
      lerTipoTransacao

lerValor :: IO Double
lerValor = do
  putStr "Informe o valor: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Double of
    Just v | v > 0 -> return v
    _ -> do
      putStrLn "Valor invalido, informe um numero positivo."
      lerValor

adicionarTransacao :: [Transacao] -> IO [Transacao]
adicionarTransacao transacoes = do
  cat <- lerCategoria
  tp  <- lerTipoTransacao
  vl  <- lerValor
  let novaTransacao = Transacao { categoria = cat, tipo = tp, valor = vl }
  putStrLn "Transacao adicionada com sucesso!"
  return (novaTransacao : transacoes)

removerTransacao :: [Transacao] -> IO [Transacao]
removerTransacao [] = do
  putStrLn "Nao ha transacoes para remover."
  return []
removerTransacao transacoes = do
  listarTransacoes transacoes
  putStr "Informe o numero da transacao a remover: "
  entrada <- getLine
  case readMaybe entrada :: Maybe Int of
    Just n | n >= 1 && n <= length transacoes -> do
      putStrLn "Transacao removida com sucesso!"
      return (removerPorIndice n transacoes)
    _ -> do
      putStrLn "Indice invalido."
      return transacoes

exibirResumo :: [Transacao] -> IO ()
exibirResumo transacoes = do
  let resumo = calcularResumo transacoes
  putStrLn "------ Resumo Financeiro ------"
  putStrLn ("Total de receitas: R$ " ++ show (totalReceitas resumo))
  putStrLn ("Total de despesas: R$ " ++ show (totalDespesas resumo))
  putStrLn ("Saldo: R$ " ++ show (saldo resumo))
  putStrLn ("")
  putStrLn "------ Estatísticas ------"
  putStrLn ("Quantidade de receitas: " ++ show (qtdReceitas transacoes))
  putStrLn ("Quantidade de despesas: " ++ show (qtdDespesas transacoes))
  putStrLn ("Maior receita: " ++ show (fst(maiorReceita transacoes)) ++ " | R$ " ++ show (snd(maiorReceita transacoes)))
  putStrLn ("Maior despesa: " ++ show (fst(maiorDespesa transacoes)) ++ " | R$ " ++ show (snd(maiorDespesa transacoes)))
  putStrLn ("Média das despesas: R$ " ++ show (calcularMediaDespesas transacoes))

displayInstrucoes :: IO ()
displayInstrucoes = do
  putStrLn "------------------------------"
  putStrLn "1 - Listar transacoes"
  putStrLn "2 - Adicionar transacao"
  putStrLn "3 - Remover transacao"
  putStrLn "4 - Gerar resumo financeiro"
  putStrLn "5 - Sair"
  putStrLn "------------------------------"
  putStr "Escolha uma opcao: "

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
    "5" -> putStrLn "Saindo do sistema. Ate logo!"
    _   -> do
      putStrLn "Opcao invalida. Tente novamente."
      loopSistema transacoes

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Sistema de Controle Financeiro"
  loopSistema []