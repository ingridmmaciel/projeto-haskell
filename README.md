## Como compilar e executar o programa
 
```bash
ghc -o sistema main.hs
./sistema
```
 
Isso gera o executável `sistema` e um menu interativo com as opções:
 
```
1 - Listar transações
2 - Adicionar transação
3 - Remover transação
4 - Gerar resumo financeiro
5 - Sair
```
 
## Como compilar e rodar os testes unitários
 
Os testes ficam em `Testes.hs` e importam as funções puras direto do `main.hs`
(por isso os dois arquivos são compilados juntos, com `-main-is Testes`
indicando qual módulo tem a função `main` a ser executada):
 
```bash
ghc -main-is Testes -o testes Testes.hs main.hs
./testes
```
 
Saída esperada: `18/18 testes passaram.` (código de saída `0`). Se algum teste
falhar, o executável imprime `[FALHOU]` na linha correspondente com o valor
esperado e o obtido, e termina com código de saída `1`.
