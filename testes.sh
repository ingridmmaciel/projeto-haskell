module Testes (main) where

import Main
  ( Categoria(..)
  , TipoTransacao(..)
  , Transacao(..)
  , ResumoFinanceiro(..)
  , todasCategorias
  , transacaoParaResumo
  , calcularResumo
  , formatarTransacoes
  , removerPorIndice
  )
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetEncoding, stdout, utf8)

-- Estrutura simples de teste: nome do caso + resultado esperado vs obtido
data Teste = Teste { nomeTeste :: String, passou :: Bool, detalhe :: String }

testar :: (Eq a, Show a) => String -> a -> a -> Teste
testar nome esperado obtido =
  Teste nome (esperado == obtido)
    ("esperado: " ++ show esperado ++ " | obtido: " ++ show obtido)

-- Dados de exemplo usados em vários testes
receita1 :: Transacao
receita1 = Transacao { categoria = Alimentacao, tipo = Receita, valor = 1500 }

despesa1 :: Transacao
despesa1 = Transacao { categoria = Transporte, tipo = Despesa, valor = 300 }

despesa2 :: Transacao
despesa2 = Transacao { categoria = Lazer, tipo = Despesa, valor = 100 }

-- ===== Testes de transacaoParaResumo =====

testeTransacaoParaResumoReceita :: Teste
testeTransacaoParaResumoReceita =
  testar "transacaoParaResumo (Receita)"
    (ResumoFinanceiro 1500 0 1500)
    (transacaoParaResumo receita1)

testeTransacaoParaResumoDespesa :: Teste
testeTransacaoParaResumoDespesa =
  testar "transacaoParaResumo (Despesa)"
    (ResumoFinanceiro 0 300 (-300))
    (transacaoParaResumo despesa1)

-- ===== Testes de calcularResumo (usa a instância de Monoid) =====

testeCalcularResumoListaVazia :: Teste
testeCalcularResumoListaVazia =
  testar "calcularResumo (lista vazia == mempty)"
    (ResumoFinanceiro 0 0 0)
    (calcularResumo [])

testeCalcularResumoMisto :: Teste
testeCalcularResumoMisto =
  testar "calcularResumo (receitas e despesas combinadas)"
    (ResumoFinanceiro 1500 400 1100)
    (calcularResumo [receita1, despesa1, despesa2])

testeCalcularResumoApenasDespesas :: Teste
testeCalcularResumoApenasDespesas =
  testar "calcularResumo (somente despesas -> saldo negativo)"
    (ResumoFinanceiro 0 400 (-400))
    (calcularResumo [despesa1, despesa2])

-- Propriedade de Monoid: mempty é elemento neutro de (<>)
testeMonoidIdentidadeEsquerda :: Teste
testeMonoidIdentidadeEsquerda =
  testar "Monoid: mempty <> r == r"
    (calcularResumo [receita1])
    (mempty <> calcularResumo [receita1])

testeMonoidIdentidadeDireita :: Teste
testeMonoidIdentidadeDireita =
  testar "Monoid: r <> mempty == r"
    (calcularResumo [receita1])
    (calcularResumo [receita1] <> mempty)

-- Propriedade: dividir a lista em duas partes e combinar os resumos
-- deve dar o mesmo resultado que calcular o resumo da lista inteira
testeMonoidAssociatividade :: Teste
testeMonoidAssociatividade =
  testar "Monoid: resumo(parte1) <> resumo(parte2) == resumo(tudo)"
    (calcularResumo [receita1, despesa1, despesa2])
    (calcularResumo [receita1] <> calcularResumo [despesa1, despesa2])

-- ===== Testes de removerPorIndice (recursão) =====

testeRemoverPrimeiro :: Teste
testeRemoverPrimeiro =
  testar "removerPorIndice 1 (remove o primeiro)"
    [despesa1, despesa2]
    (removerPorIndice 1 [receita1, despesa1, despesa2])

testeRemoverMeio :: Teste
testeRemoverMeio =
  testar "removerPorIndice 2 (remove o do meio)"
    [receita1, despesa2]
    (removerPorIndice 2 [receita1, despesa1, despesa2])

testeRemoverUltimo :: Teste
testeRemoverUltimo =
  testar "removerPorIndice 3 (remove o último)"
    [receita1, despesa1]
    (removerPorIndice 3 [receita1, despesa1, despesa2])

testeRemoverIndiceInvalidoZero :: Teste
testeRemoverIndiceInvalidoZero =
  testar "removerPorIndice 0 (índice inválido, lista inalterada)"
    [receita1, despesa1]
    (removerPorIndice 0 [receita1, despesa1])

testeRemoverIndiceForaDoIntervalo :: Teste
testeRemoverIndiceForaDoIntervalo =
  testar "removerPorIndice 10 (índice além do tamanho, lista inalterada)"
    [receita1, despesa1]
    (removerPorIndice 10 [receita1, despesa1])

testeRemoverListaVazia :: Teste
testeRemoverListaVazia =
  testar "removerPorIndice em lista vazia"
    ([] :: [Transacao])
    (removerPorIndice 1 [])

-- ===== Testes de formatarTransacoes (list comprehension) =====

testeFormatarTransacoesVazia :: Teste
testeFormatarTransacoesVazia =
  testar "formatarTransacoes (lista vazia)"
    ([] :: [String])
    (formatarTransacoes [])

testeFormatarTransacoesConteudo :: Teste
testeFormatarTransacoesConteudo =
  testar "formatarTransacoes (contém categoria e tipo formatados)"
    True
    (let linhas = formatarTransacoes [receita1]
     in length linhas == 1
        && "Receita" `elemStr` head linhas
        && "Alimentacao" `elemStr` head linhas)
  where
    elemStr sub str = sub `isInfixOfSimples` str
    isInfixOfSimples sub str = any (sub `prefixDe`) (caudas str)
    caudas [] = [[]]
    caudas s@(_:xs) = s : caudas xs
    prefixDe [] _ = True
    prefixDe _ [] = False
    prefixDe (a:as) (b:bs) = a == b && prefixDe as bs

-- ===== Testes de todasCategorias =====

testeTodasCategoriasTamanho :: Teste
testeTodasCategoriasTamanho =
  testar "todasCategorias tem as 8 categorias definidas"
    8
    (length todasCategorias)

testeTodasCategoriasContemOutros :: Teste
testeTodasCategoriasContemOutros =
  testar "todasCategorias contém Outros"
    True
    (Outros `elem` todasCategorias)

-- ===== Execução dos testes =====

todosOsTestes :: [Teste]
todosOsTestes =
  [ testeTransacaoParaResumoReceita
  , testeTransacaoParaResumoDespesa
  , testeCalcularResumoListaVazia
  , testeCalcularResumoMisto
  , testeCalcularResumoApenasDespesas
  , testeMonoidIdentidadeEsquerda
  , testeMonoidIdentidadeDireita
  , testeMonoidAssociatividade
  , testeRemoverPrimeiro
  , testeRemoverMeio
  , testeRemoverUltimo
  , testeRemoverIndiceInvalidoZero
  , testeRemoverIndiceForaDoIntervalo
  , testeRemoverListaVazia
  , testeFormatarTransacoesVazia
  , testeFormatarTransacoesConteudo
  , testeTodasCategoriasTamanho
  , testeTodasCategoriasContemOutros
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "===== Executando testes unitários ====="
  resultados <- mapM executarTeste todosOsTestes
  let totalTestes = length resultados
      totalFalhas = length (filter not resultados)
  putStrLn "========================================"
  putStrLn (show (totalTestes - totalFalhas) ++ "/" ++ show totalTestes ++ " testes passaram.")
  if totalFalhas == 0
    then exitSuccess
    else exitFailure
  where
    executarTeste :: Teste -> IO Bool
    executarTeste t = do
      if passou t
        then putStrLn ("[OK]   " ++ nomeTeste t)
        else putStrLn ("[FALHOU] " ++ nomeTeste t ++ " (" ++ detalhe t ++ ")")
      return (passou t)
