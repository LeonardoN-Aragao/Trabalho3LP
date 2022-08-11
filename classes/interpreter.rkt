#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

(provide value-of-program)

; Representação de procedimentos para escopo estático

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ) ; call by value
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; Criação de ambiente estendido com procedimento recursivo
(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

; value-of :: Exp -> ExpVal

(define (value-of exp Δ)
  (match exp
    [(ast:int n) n]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:proc (ast:var v) e) (proc-val v e Δ)]
    [(ast:call e1 e2) (apply-proc (value-of e1 Δ) (value-of e2 Δ))] ; call by value
    [(ast:letrec (ast:var f) (ast:var v) e1 e2) (value-of e2 (extend-env-rec f v e1 Δ))]
    [(ast:begin es) (foldl (lambda (e v) (value-of e Δ)) (value-of (first es) Δ) (rest es))]
    [(ast:assign (ast:var x) e) (begin
                                  (setref! (apply-env Δ x) (value-of e Δ)) ;set the value in the store
                                  42)] ; return the 42 value
    ;-------------------------------------------------------------------------
    [(ast:self) (apply-env Δ '%self)] ; apply the environment in the '%self class
    [(ast:send obj-exp method-name args)
      (let* ([args (values-of-exps args Δ)] ; send retira os argumentos e o objeto
             [obj (value-of obj-exp Δ)]
             [class (find-class (object-class-name obj))])
        (apply-method (find-method (object-class-name obj) (ast:var-name method-name))
                      obj
                      args
                      (ast:decl-fields class)
        )
      )
    ]
    [(ast:super name args) 
      (let* ([args (values-of-exps args Δ)] ; super retira os argumentos e o objeto self
             [obj (apply-env Δ '%self)]
             [class (find-class (object-class-name obj))])
        (apply-method (find-method (apply-env Δ '%super) (ast:var-name name)); e acha o método ( exp) do objeto super
                      obj                                      ; com os argumentos args e o objeto obj
                      args
                      (ast:decl-fields class)
        )
      )
    ]
    [(ast:new (ast:var class-name) args) 
      (let ([args (values-of-exps args Δ)]
            [obj (new-object class-name)]
            [class (find-class class-name)])        ; usando new-object
        (let  ([this-meth (find-method (object-class-name obj) "initialize")])   ; acha o método initialize do objeto obj 
              (apply-method this-meth obj args (ast:decl-fields class))
        )
        obj ; retornando obj
      )
    ]     
    ;-------------------------------------------------------------------------
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; value-of-program :: Program -> ExpVal
(define value-of-program ; Função principal, chamada para avaliar o valor de um programa.
  (lambda (prog)
    (match-define  
      (ast:prog decls exp) prog) ; separa as declarações das expressões
    (empty-store)        ; incializa o store
    (initialize-class-env) ; inicializa o ambiente com object no the-class-env
    (map 
      (lambda decl
        (match (car decl) 
          [(ast:decl (ast:var name) (ast:var super) fields methods)
            (initialize-class-decl! name super fields methods)
          ]
        )
      ) 
      decls
    ) ; para cada estrutura dentro das declarações que corresponder com a estrutura definida em "ast:decl", inicializa a mesma (inclui no ambiente de classes)
    (value-of exp the-class-env) ; chama "value-of" pra avaliar as expressões dado o ambiente de classes
  )
)

; Comportamento de uma lista de expressões, podendo ser vazia.
(define values-of-exps
  (lambda (exps env)
    (map 
      (lambda (exp)
        (value-of exp env)
      )
      exps
    ) ; para cada expressão em "exps", calcula a expressao dado o ambiente "env", através de "value-of"
  )
)

; Funções baseadas/retiradas do Capítulo 9 do livro "Essentials of Programming Languages"

; Struct de object, que é uma instância de uma classe.
(struct object (class-name fields)) 


(define new-object      ; Cria um objeto da classe class-name
  (lambda (class-name)
    (object class-name
      (map
        (lambda (field-name) 
          (newref field-name) ; cria uma nova referência na memória com o nome dos campos
        )
        (ast:decl-fields (find-class class-name)) ; Pega os campos da classe com o nome class-name
      )
    )
  )
) 

(define (apply-method m self args f) ; Função que aplica o método m do objeto self com os argumentos args, e retorna o valor
  (if (ast:method? m) ; Checa se m é método
    (
      let ([vars (map (lambda (x) 
            (ast:var-name x)) (ast:method-params m))]                  ; Obtém as informações relevantes do método m
          [body (ast:method-body m)]
          [super-name (ast:method-name m)]
          [fields (map (lambda (x) 
            (ast:var-name x)) f)])
      (value-of body 
        (multiple-extend-env vars (map newref args) ; Obtém o valor do corpo do método no novo ambiente, criado com extend env, com a variável vars, e as referências na memória dos argumentos
          (extend-env-with-self-and-super ; Estende o ambiente com o objeto atual, e com a classe super-name que possui o método
            self super-name
            (multiple-extend-env fields (object-fields self) empty-env)
          )
        )
      )
    )
      (display " não é método\n")
  )
)

(define (multiple-extend-env vars values env)
  (foldl (lambda(var-value env)
    (extend-env (first var-value) (second var-value) env)) env (append vars values)
  )
)

;------------------------------------
; Função extend-env-with-self-and-super, retirada do livro Essentials of Programming Languages
(define (extend-env-with-self-and-super self super-name saved-env) ; função faz a ligação de %self e %super com um objeto e um nome de classe, respectivamente
  (lambda (svar)
    (case svar
            ((%self) self)
            ((%super) super-name)
            (else (apply-env saved-env svar)))))

; ClassEnv = Listof(List(ClassName, Class))
;the-class :: ClassEnv
(define the-class-env '()) ;object object null)) ; Ambiente de classe vazio, que será inicializado pela função initialize-class-env! com as classes declaradas no programa

; initialize-class-env! :: () -> ???
(define initialize-class-env ; inicializa o ambiente de classes the-class com os objetos correspondentes às declarações de classe do programa
  (lambda ()
    (set! the-class-env
      (list
        (list "object" (ast:decl  #f #f  '() '())))) ; Inicializa o ambiente de classes com um objeto
  )
)

; my initialize-class-decl!
(define initialize-class-decl! ; Inicializa o ambiente the-class com cada uma das classes declaradas no programa, passadas pela função initialize-class-env!
  (lambda (class-name super-name field-names m-decls)
    (let ([field-names 
            (append-field-names ; Obtém os nomes dos campos resultantes da concatenação dos campos da classe atual com os da superior
              (ast:decl-fields (find-class super-name))
              field-names)])
          (add-to-class-env! ; Adiciona a classe com nome class-name e dados obtidos acima no ambiente the-class-env
            class-name
            (ast:decl class-name super-name field-names 
              (merge-method-envs ; Faz um merge no ambiente de métodos da classe atual com a superior
                (ast:decl-methods (find-class super-name)) ; Obtém o ambiente de métodoss da classe superior
                (method-decls->method-env ; Obtém o ambiente de métodos declarado nesta declaração de classe
                m-decls super-name)
              )
            )
          )
    )
  )
)

;Concatena as listas em uma lista só
(define (append l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (append  (cdr l1) (cdr l2))))
)

; add-to-class-env! :: ClassName x Class -> ???
(define add-to-class-env!     ; Adiciona a classe class de nome class-name no ambiente the-class
  (lambda (class-name class)
    (set! the-class-env       ; usando a função set! O ambiente the-class vai possuir listas com dois elementos, o primeiro sendo o
          (cons               ; nome da classe, o segundo a classe em si
           (list class-name class)
           the-class-env))))

; find-class :: ClassName -> Class
(define find-class    ; Procura e retorna (se existir) a classe de nome no ambiente the-class
  (lambda (name)
    (let ((maybe-pair 
            (assf 
              (lambda (x) 
                (string=? name x))
            the-class-env)))
      (if maybe-pair 
        (cadr maybe-pair)
        (display "\nClasse desconhecida\n"))
    )
  )
)

; fresh-identifier           
(define fresh-identifier ; Função para criar novo identificador ao "mergear" os campos de classes diferentes com métodos com mesmo nome
  (let ((sn 0))          ; Retirada do livro EOPL
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; % não pode aparecer em um identificador de input
        (number->string sn))))))

; append-field-names :: Listof(FieldName) x Listof(FieldName) -> Listof(FieldName)
(define append-field-names ; Concatena a lista de nomes de campos da classe superior com a atual
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields) ; Se não houver campos na classe superior, retorna simplesmente os nomes de campos novos
      (else                             ; senão
       (cons
        (if (memq (car super-fields) new-fields) ; verifica se o primeiro campo da classe superior tem nome igual a um campo da classe atual
            (fresh-identifier (car super-fields)) ; Se sim cria um novo identificador para o primeiro da classe superior
            (car super-fields)) ; Se não, simplesmente o primeiro campo da classe superior
        (append-field-names    ; chama a função recursivamente com o resto (2:n) dos campos da classe superior com os novos campos
         (cdr super-fields) new-fields))))))

; MethodEnv = Listof(List(MethodName, Method))
; find-method :: Sym x Sym -> Method
(define find-method                ; encontra um método de nome name em uma classe c-name
  (lambda (c-name name)
    (let ([this-class (find-class c-name)])   ; primeiro procura a classe
      (if (void? this-class) (display "Classe não encontrada\n")
          (let ([m-env (ast:decl-methods this-class)])
                (let ([maybe-pair (assf 
                  (lambda (x)
                   (match-define
                    (ast:var m-name) x)
                   (string=? name m-name)) m-env)
                ])
                (if (pair? maybe-pair) (cadr maybe-pair)   ; Se encontrou, retorna somente o método.
                  (display "Método não encontrado\n")
                ) 
                  )
                ) 
              )
)))

; method-decls->method-env :: Listof(MethodDecl) x ClassName -> MethodEnv
(define method-decls->method-env ; retorna o ambiente de métodos correspondente às declarações de métodos da classe que está sendo declarada no programa
  (lambda (m-decls super-name)  ; m-decls: declaração de métodos, super-name: nome de class, field-names: nome dos campos da classe
    (map
      (lambda (m-decl)  ; para cada declaração de método, pega as informações relevantes do método (method-name, vars e body)
        (match-define 
          (ast:method method-name params body) m-decl)
        (list method-name (ast:method super-name params body))
      ) ; cria uma lista de dois elementos: o nome do método, e o método em si, como foi feito para as classes
     m-decls)
  )
) ; aplica a função em cada uma das declarações de método atuais

; merge-method-envs :: MethodEnv x MethodEnv -> MethodEnv     
(define merge-method-envs ; Função para juntar ambientes de métodos de duas classes
  (lambda (super-m-env new-m-env) ; Simplesmente concatena (append) o novo ambiente com o antigo (da classe superior). Dessa maneira, os métodos da classe nova, que estende a superior
    (zip new-m-env super-m-env))) ; ficam na frente. Quando a função find-method procura um método, vai pegar usando a função assq o primeiro da lista, ou seja, o da classe mais
                                     ; baixa na hierarquia (o mais novo). Dessa forma lida-se com duas classes com métodos de mesmo nome, uma das quais estende a outra.

(define (zip l1 l2)
  (cond
    ((null? l1) l2)
    (else (cons (car l1) (zip (cdr l1) l2)))))                                