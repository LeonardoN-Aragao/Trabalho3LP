-- Gabriel Santos Fortunato: 201665074AC
-- Leonardo Nunes Aragão: 201665565C

# Trabalho3LP

## Como jogar

Para rodar o projeto precisa baixar e instalar:

- Raco
- DrRacket

link para download: https://racket-lang.org/download/

Após instalar adicione nas variaveis de ambientes do sistema para utilizar os comandos de instalação do package no terminal ou pelo programa DrRacket -> File -> Install Package

:exclamation:OBS: Caso necessário baixe a pasta util de https://github.com/lvsreis/dcc019

---------------------------------------------------

## Decisões de projeto

A estratégia para validação da resposta do usuário é a seguinte: Inicialmente, encontra-se todos os acertos completos. Após isso, a partir das listas remanescentes (sem considerar os acertos), é feita uma verificação em cada elemento das duas listas de forma que a cada "match" os elementos são removidos de ambas as listas (evitando encontrar múltiplos acertos parciais para uma só entrada).

Para validar a entrada de cada tentativa do usuário, limpamos inicialmente a entrada (removendo os espaços existentes) e validamos conforme as regras do jogo (de forma que a entrada só seja aceita caso possua tamanho 4 e não possua dígitos de valor fora do alcance entre 1 e 6)