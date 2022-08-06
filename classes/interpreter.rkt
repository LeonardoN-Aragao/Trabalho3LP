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
    ;[(ast:super (ast:var x) e) (apply-env x e %super)]
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
              
    [(ast:send obj-exp method-name rands) 
                          (let ([args (values-of-exps (cadddr exp) Δ)] ; send retira os argumentos e o objeto
                                [obj (value-of (cadr exp) Δ)])
                            (apply-method                              ; e em seguida aplica o método (caddr exp) no objeto obj com os argumentos args
                            (find-method (object-class-name obj) (caddr exp))
                            obj
                            args))]
    [(ast:super name args) 
                            (let ([args (values-of-exps (caddr exp) Δ)] ; super retira os argumentos e o objeto self
                                [obj (apply-env Δ '%self)])
                            (apply-method                             ; e acha o método (cadr exp) do objeto super
                              (find-method (apply-env Δ '%super) (cadr exp))
                              obj                                      ; com os argumentos args e o objeto obj
                              args))]
    [(ast:new class-name rands)  
                            (printf "oi")
                            (display class-name rands)
                            (let ([args (values-of-exps (caddr exp) Δ)]   ; new cria um novo objeto
                                  [obj (cadr exp)])
                                (display args)
                                (display obj)          ; usando new-object
                              (let ([this-meth (find-method (object-class-name obj) 'initialize)])   ; acha o método initialize do objeto obj
                                ;((display (method? this-meth)) (display this-meth) (display "\n")
                                (apply-method                                                        ; e aplica o método
                                  this-meth
                                  obj
                                  args))
                              obj)] ; retornando obj
    ;-------------------------------------------------------------------------
    [e (raise-user-error "unimplemented-construction: " e)]
    )
    (if (number? exp) exp                          ; Se Expr -> Number            
          (if (equal? exp 'self) (apply-env Δ '%self) ; se for self
              (apply-env Δ exp))))                  ; senão, é identificador

;;; (define value-of-program prog)
;;;   (display prog)
;;;   (empty-store)
;;;   ; you must collect all the classes declared and building its respectively environment
;;;   ; execute the prog expression in the correct environment
;;;   (value-of prog init-env))

  ; value-of-program :: Program -> ExpVal
(define value-of-program ; Função principal, chamada para avaliar o valor de um programa.
  (lambda (prog)
    (display prog)
    (printf "\n")
    (empty-store)        ; incializa o store
    (let ([class-decls (car prog)]   ; retira do programa a primeira parte, que são as declarações de classe
          [body (cadr prog)])     ; segunda parte, que é o corpo do programa
    (for-each initialize-class-env! class-decls) ; inicializa o ambiente de classes the-class-env com as classes declaradas
    (value-of body the-class-env)
  )))


; Comportamento de uma lista de expressões, podendo ser vazia. Função auxiliar
(define values-of-exps
  (lambda (exps env)
    (map
     (lambda (exp) (value-of exp env))
     exps)))

;;; ; Função extend-env modificada para CLASSES
;;; (define (extend-env var value env)
;;;   (lambda (svar)
;;;     (if (list? var) ; Se a variável for uma lista, como quando tem vários fields (i j) em um objeto
;;;         (if (memq svar var) (list-ref value (index-of var svar)) ; Se svar está na lista var, retira o valor da lista value na posição que o elemento svar está em var
;;;             (apply-env env svar)) ; senão, aplica o ambiente-pai em svar para encontrar a variável no ambiente anterior
;;;         (if (equal? svar var) (if (list? value) (car value) value) ; Se value é uma lista, como no caso em que um objeto tem vários fields (i j) e você quer encontrar somente i
;;;             (apply-env env svar))))) ; senão, aplica o ambiente-pai em svar para encontrar a variável no ambiente anterior

(define (apply-env env var)
  (env var))

;------------------------------------
; Função extend-env-with-self-and-super, retirada do livro Essentials of Programming Languages
(define (extend-env-with-self-and-super self super-name saved-env) ; função faz a ligação de %self e %super com um objeto e um nome de classe, respectivamente
  (lambda (svar)
    (case svar
            ((%self) self)
            ((%super) super-name)
            (else (apply-env saved-env svar)))))

;------------------------------------

; call-by-value
; proc-val :: Var x Expr x Env -> Proc
#;(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
#;(define (apply-proc proc val)
  (proc val))


; call-by-reference
;;; (define (proc-val var exp Δ)
;;;   (lambda (val flag)
;;;     (if flag (value-of exp (extend-env var (newref val) Δ))
;;;         (value-of exp (extend-env var val Δ)))))

;;; (define (apply-proc proc val)
;;;   (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))

(struct thunk (env exp))

; -------------------------------- Implementação das estruturas e funções da linguagem CLASSES --------------------------------------
; Seguindo estratégia apresentada no Capítulo 9 do livro "Essentials of Programming Languages", 3ra edição.

; Tipo de dados class, que representa uma classe com um nome, um nome da classe superior, campos de dados, e um ambiente de métodos
;(struct class (class-name super-name fields method-env)) 

; Tipo de dados object, que representa um objeto, que é uma instância de uma classe. Possui nome da classe e campos.
(struct object (class-name fields)) 

; Tipo de dados method, que representa um método. Possui variáveis, um corpo a ser executado quando o método é chamado,
; um nome da classe que possui o médoto, e campos de dados
;(struct method (vars body super-name fields))

; new-object :: ClassName -> Obj
(define new-object      ; Cria um objeto da classe class-name
  (lambda (class-name)
    (object             ; Cria um objeto
      class-name        ; nome da classe
      (map
       (lambda (field-name) ; Campos de dados. cria uma nova referência na memória com o nome dos campos
         (newref field-name));(list 'uninitialized-field field-name)))
       (ast:decl-fields (lookup-class class-name)))))) ; Pega os campos da classe com o nome class-name, encontrada com lookup-class

; apply-method :: Method x Obj x ListOf(ExpVal) -> ExpVal
(define (apply-method m self args) ; Função que aplica o método m do objeto self com os argumentos args, e retorna o valor
  ;(lambda (m self args)
  (if (ast:method? m) ; Checa se m é método
        (let ([vars (ast:method-params m)]                  ; Obtém as informações relevantes do método m
              [body (ast:method-body m)]
              [super-name (ast:method-name m)])
          (value-of body (extend-env vars (map newref args) ; Obtém o valor do corpo do método no novo ambiente, criado com extend env, com a variável vars, e as referências na memória dos argumentos
                                        (extend-env-with-self-and-super ; Estende o ambiente com o objeto atual, e com a classe super-name que possui o método
                                         self super-name
                                         (extend-env (object-fields self) ; Estende o ambiente vazio com os campos de dados do objeto atual
                                                     empty-env)))))(display "m não é método\n")))



; ClassEnv = Listof(List(ClassName, Class))
;the-class-env :: ClassEnv
(define the-class-env '()) ;object object null)) ; Ambiente de classe vazio, que será inicializado pela função initialize-class-env! com as classes declaradas no programa

; initialize-class-env! :: Listof(ClassDecl) -> ???
(define initialize-class-env! ; inicializa o ambiente de classes the-class-env com os objetos correspondentes às declarações de classe do programa
  (lambda (c-decls)
    (printf "\n")
    (display c-decls)
    (set! the-class-env
      (list
        (list 'object (ast:decl '() '() '() '())))) ; Inicializa o ambiente de classes com um objeto
    (initialize-class-decl! c-decls))) ; para cada classe declarada no programa

; my initialize-class-decl!
(define initialize-class-decl! ; Inicializa o ambiente the-class-env com cada uma das classes declaradas no programa, passadas pela função initialize-class-env!
  (lambda (c-decl)
    (let ([class-name (cadr c-decl)] ; Obtém as informações relevantes da declaração de classes
          [super-name (car (caddr c-decl))]
          [field-names (cadr (caddr c-decl))]
          [m-decls (caddr (caddr c-decl))])
       (let ([field-names (append-field-names ; Obtém os nomes dos campos resultantes da concatenação dos campos da classe atual com os da superior
                            (ast:decl-fields (lookup-class super-name))
                            field-names)])
                      (add-to-class-env! ; Adiciona a classe com nome class-name e dados obtidos acima no ambiente the-class-env
                       class-name
                       (ast:decl class-name super-name field-names
                                (merge-method-envs ; Faz um merge no ambiente de métodos da classe atual com a superior
                                 (ast:decl-methods (lookup-class super-name)) ; Obtém o ambiente de métodoss da classe superior
                                 (method-decls->method-env ; Obtém o ambiente de métodos declarado nesta declaração de classe
                                  m-decls super-name))))
                                  ))))

; add-to-class-env! :: ClassName x Class -> ???
(define add-to-class-env!     ; Adiciona a classe class de nome class-name no ambiente the-class-env
  (lambda (class-name class)
    (set! the-class-env       ; usando a função set! O ambiente the-class-env vai possuir listas com dois elementos, o primeiro sendo o
          (cons               ; nome da classe, o segundo a classe em si
           (list class-name class)
           the-class-env))))

; lookup-class :: ClassName -> Class
(define lookup-class    ; Procura e retorna (se existir) a classe de nome name no ambiente the-class-env
  (lambda (name)
    (printf "\n name \n")
    (display name)
    (printf "\n")
    (let ((maybe-pair (assq name the-class-env)))
    (printf "maybe-pair\n")
    (display maybe-pair)
    (if maybe-pair (cadr maybe-pair)
        (display "\nClasse desconhecida\n"))
)))

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
    (let ([this-class (lookup-class c-name)])   ; primeiro procura a classe
      (if (void? this-class) (display "Classe não encontrada\n")
          (let ([m-env (ast:decl-methods this-class)])    ; se encontrou a clase, pega o ambiente de métodos da classe
             (let ([maybe-pair (assq name m-env)])        ; encontra o par (nome_do_método, método) no ambiente de métodos da m-env
               (if (pair? maybe-pair) (cadr maybe-pair)   ; Se encontrou, retorna somente o método.
                   (display "Método não encontrado\n"))))))))

; method-decls->method-env :: Listof(MethodDecl) x ClassName x Listof(FieldName) -> MethodEnv
(define method-decls->method-env ; retorna o ambiente de métodos correspondente às declarações de métodos da classe que está sendo declarada no programa
  (lambda (m-decls super-name)  ; m-decls: declaração de métodos, super-name: nome de class, field-names: nome dos campos da classe
    (map
     (lambda (m-decl)  ; para cada declaração de método, pega as informações relevantes do método (method-name, vars e body)
       (let ([method-name (cadr m-decl)] 
             [vars (caddr m-decl)]
             [body (cadddr m-decl)])
         (list method-name (ast:method super-name vars body)))) ; cria uma lista de dois elementos: o nome do método, e o método em si, como foi feito para as classes
     m-decls))) ; aplica a função em cada uma das declarações de método atuais

; merge-method-envs :: MethodEnv x MethodEnv -> MethodEnv     
(define merge-method-envs ; Função para juntar ambientes de métodos de duas classes
  (lambda (super-m-env new-m-env) ; Simplesmente concatena (append) o novo ambiente com o antigo (da classe superior). Dessa maneira, os métodos da classe nova, que estende a superior
    (append new-m-env super-m-env))) ; ficam na frente. Quando a função find-method procura um método, vai pegar usando a função assq o primeiro da lista, ou seja, o da classe mais
                                     ; baixa na hierarquia (o mais novo). Dessa forma lida-se com duas classes com métodos de mesmo nome, uma das quais estende a outra.
; maybe :: 
(define maybe  ; Função auxiliar maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v)))))

; value-of-program :: Program -> ExpVal
;;; (define value-of-program ; Função principal, chamada para avaliar o valor de um programa.
;;;   (lambda (prog)
;;;     (empty-store)        ; incializa o store
;;;     (let ([class-decls (car prog)]   ; retira do programa a primeira parte, que são as declarações de classe
;;;           [body (cadr prog)])        ; segunda parte, que é o corpo do programa
;;;       (initialize-class-env! class-decls)   ; inicializa o ambiente de classes the-class-env com as classes declaradas
;;;         (value-of body the-class-env))))  ; pega o valor do corpo do programa no ambiente de classes recém inicializado

;------------------------------------------------------------------------------------------------------------------------------------------------
; Exemplo 1: resultado = 33
(define t1 '((
              (class c1 (object () ((method initialize () 1)
                                   (method m1 () (send self m2()))
                                   (method m2 () 13))))
             (class c2 (c1 () ((method m1 () 22)
                              (method m2 () 23)
                              (method m3 () (super m1()))
                              )))
            )
            (let o = new c1()
             (display o))
))
            ;;;  (class c3 (c2 () ((method m1 () 32)
            ;;;                   (method m2 () 33)))))
            ;;;  (new c1 ()) ))
            ;(send (var o3) m3 ()))))

; Valor dos exemplos
(value-of-program t1)