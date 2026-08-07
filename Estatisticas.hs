module Estatisticas where

import Tipos
    ( Categoria(..)
    , TipoTransacao(..)
    , Transacao(..)
    , ResumoFinanceiro(..)
    )

-- Calcula a soma de todos os valores das transacoes classificadas como Receita
calcularTotalReceitas :: [Transacao] -> Double
calcularTotalReceitas [] = 0
calcularTotalReceitas (t:ts) =
    if tipo t == Receita
        then valor t + calcularTotalReceitas ts
        else calcularTotalReceitas ts
        
-- Calcula a soma de todos os valores das transacoes classificadas como Despesa
calcularTotalDespesas :: [Transacao] -> Double
calcularTotalDespesas [] = 0
calcularTotalDespesas (t:ts) =
    if tipo t == Despesa
        then valor t + calcularTotalDespesas ts
        else calcularTotalDespesas ts

-- Calcula o saldo financeiro 
calcularSaldo :: [Transacao] -> Double
calcularSaldo transacoes = calcularTotalReceitas transacoes - calcularTotalDespesas transacoes

--Calcula a média das despesas
calcularMediaDespesas :: [Transacao] -> Double
calcularMediaDespesas transacoes = aux transacoes 0 0
    where
        aux [] _ 0 = 0
        aux [] soma qtd = soma / qtd
        aux (t:ts) soma qtd =
            if tipo t == Despesa
                then aux ts (soma + valor t) (qtd+1)
                else aux ts soma qtd

-- Retorna a despesa de maior valor utilizando Tail Recursion
maiorDespesa :: [Transacao] -> Double
maiorDespesa transacoes = aux transacoes 0
    where
        aux [] maiorAtual = maiorAtual
        aux (t:ts) maiorAtual =
            if tipo t == Despesa
                then
                if valor t > maiorAtual
                        then aux ts (valor t)
                        else aux ts maiorAtual
                else
                    aux ts maiorAtual

-- Retorna a receita de maior valor utilizando Tail Recursion
maiorReceita :: [Transacao] -> Double
maiorReceita transacoes = aux transacoes 0
    where
        aux [] maiorAtual = maiorAtual
        aux (t:ts) maiorAtual =
            if tipo t == Receita
                then
                if valor t > maiorAtual
                        then aux ts (valor t)
                        else aux ts maiorAtual
                else
                    aux ts maiorAtual

-- Conta a quantidade de transações do tipo Receita
qtdReceitas :: [Transacao] -> Double
qtdReceitas [] = 0
qtdReceitas (t:ts) =
    if tipo t == Receita
        then 1 + qtdReceitas ts
        else qtdReceitas ts

-- Conta a quantidade de transações do tipo Depesa
qtdDespesas :: [Transacao] -> Double
qtdDespesas [] = 0
qtdDespesas (t:ts) =
    if tipo t == Receita
        then 1 + qtdReceitas ts
        else qtdReceitas ts

-- Gera o resumo financeiro
gerarResumo :: [Transacao] -> ResumoFinanceiro
gerarResumo transacoes = ResumoFinanceiro {
    totalReceitas = calcularTotalReceitas transacoes,
    totalDespesas = calcularTotalDespesas transacoes,
    saldo = calcularSaldo transacoes,
    mediaDespesas = calcularMediaDespesas transacoes
}

