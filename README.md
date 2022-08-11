-- Gabriel Santos Fortunato: 201665074AC
-- Leonardo Nunes Aragão: 201665565C

# Trabalho3LP

## Como executar

Para rodar o projeto precisa baixar e instalar:

- Raco
- DrRacket

link para download: https://racket-lang.org/download/

Após instalar adicione nas variaveis de ambientes do sistema para utilizar os comandos de instalação do package no terminal ou pelo programa DrRacket -> File -> Install Package

OBS: Caso necessário baixe a pasta util de https://github.com/lvsreis/dcc019

---------------------------------------------------

## Decisões de projeto

Foi utilizado o struct objeto que é uma instância de uma classe.  

$ (struct object (class-name fields)) 

the-class-env é uma variável global que é iniciada vazia '()
initialize-class-env! somente inicia o objeto vazio em the-class-env.

$ "object" (ast:decl  #f #f  '() '()))

initializa-class-decl! inclui a declaração de cada classe do programa no the-class-env.


Algumas das funções utilizadas foram baseadas em funções encontradas no livro EOPL.

Algumas funções básicas foram encontradas e retiradas de documentação do Racket:

- https://docs.racket-lang.org/guide/
- https://docs.racket-lang.org/reference

