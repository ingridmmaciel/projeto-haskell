module Estatisticas where

import Tipos
    ( Categoria(..)
    , TipoTransacao(..)
    , Transacao(..)
    , ResumoFinanceiro(..)
    )

-- Conta a quantidade de transações do tipo Receita
qtdReceitas :: [Transacao] -> Int
qtdReceitas [] = 0
qtdReceitas (t:ts) =
    if tipo t == Receita
        then 1 + qtdReceitas ts
        else qtdReceitas ts

-- Conta a quantidade de transações do tipo Despesa
qtdDespesas :: [Transacao] -> Int
qtdDespesas [] = 0
qtdDespesas (t:ts) =
    if tipo t == Despesa
        then 1 + qtdDespesas ts
        else qtdDespesas ts

-- Retorna a receita de maior valor utilizando Tail Recursion
maiorReceita :: [Transacao] -> (Categoria, Double)
maiorReceita transacoes = aux transacoes (Outros, 0)
    where
        aux [] maiorAtual = maiorAtual
        aux (t:ts) maiorAtual =
            if tipo t == Receita
                then
                if valor t > snd maiorAtual
                        then aux ts (categoria t, valor t)
                        else aux ts maiorAtual
                else
                    aux ts maiorAtual

-- Retorna a despesa de maior valor utilizando Tail Recursion
maiorDespesa :: [Transacao] -> (Categoria, Double)
maiorDespesa transacoes = aux transacoes (Outros, 0)
    where
        aux [] maiorAtual = maiorAtual
        aux (t:ts) maiorAtual =
            if tipo t == Despesa
                then
                if valor t > snd maiorAtual
                        then aux ts (categoria t, valor t)
                        else aux ts maiorAtual
                else
                    aux ts maiorAtual

--Calcula a média das despesas
calcularMediaDespesas :: [Transacao] -> Double
calcularMediaDespesas transacoes = aux transacoes 0 0
    where
        aux [] _ 0 = 0
        aux [] soma qtd = fromIntegral (round((soma/qtd)*10))/10
        aux (t:ts) soma qtd =
            if tipo t == Despesa
                then aux ts (soma + valor t) (qtd+1)
                else aux ts soma qtd