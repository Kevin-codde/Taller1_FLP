#lang eopl
#|

   Autores: Kevin Andres Bejarano - 2067678
            Juan David Gutierrez Florez - 2060104
            Johan Sebastián Laverde pineda - 2266278
            Johan Sebastian Acosta Restrepo 2380393
   
|#



;Datatypes

;<chip prim> := prim_or
;              chip-or()
;           := prim and
;               chip-and()
;           := prim_not
;              chip-not()
;            := prim_xor
;              chip-xor()
;           := prim_nand
;              chip-xor()


(define-datatype chip-prim chip-prim?
  (chip_or)
  (chip_and)
  (chip_not)
  (chip_xor)
  (chip_nand)
 )




;-------------------------------------------------------------------------------------------------

;<circuito> := circ_simple ({cable }∗) ({cable}∗)
;             <chip>
;              simple-circuit(in out chip)
;          := circcomp <circuito> {<circuito>}+
;              input{cable}∗
;              output{cable}∗
;              complex-circuit(circ lcircs in out)


(define-datatype  circuito circuit?

    (simple-circuit(in (list-of symbol?))
                   (out (list-of symbol?))
                   (ch chip?)
    )

    (complex-circuit (circ circuit?)
                     (lcircs (list-of circuit?))
                     (in (list-of symbol?))
                     (out (list-of symbol?))
)
)

;-------------------------------------------------------------------------------------------------

 ;  <chip> := <chip prim>
 ;           prim-chip (chip-prim)
 ;         := chip ( --> {(port)}* )
 ;           ( <-- {(port)}* )
 ;           <circuito>
 ;            comp-chip (in, out, circ)


(define-datatype chip chip?
   (prim-chip(ent chip-prim?))
   (comp-chip (in (list-of symbol?))
                  (out (list-of symbol?))
                  (circ circuit?)
))



;---------------------------------------------------------------------------------------------------
;Area del programador


(define cir2


 (complex-circuit
 (simple-circuit
  '(m n o p)
  '(e f)
  (comp-chip
   '(INA INB INC IND)
   '(OUTE OUTF)
   (complex-circuit
    (simple-circuit '(a b) '(e) (prim-chip (chip_and)))
    (list
     (simple-circuit '(c d) '(f) (prim-chip (chip_and))))
    '(a b c d)
    '(e f))

   ))
 
 (list
  (simple-circuit
   '(e f)
   '(z)
   (comp-chip
    '(INE INF)
    '(OUTA)
    (simple-circuit '(e f) '(g) (prim-chip (chip_or)))

    )

   ))
 
 '(m n o p)
 '(z)))

(define parse-chip
  (lambda (exp)
    (cond
      [(eqv? (car exp) 'prim-chip)
       (prim-chip (cadr exp))]  ;; Extrae el tipo de chip primitivo
      [(eqv? (car exp) 'comp-chip)
       (comp-chip
         (cadr exp)              ;; Lista de entradas
         (caddr exp)             ;; Lista de salidas
         (parse (cadddr exp)))])))  ;; Parsea el circuito interno

(define parse
  (lambda (exp)
    (cond
      ;; Si 'exp' es un circuito simple
      [(eqv? (car exp) 'simple-circuit)
       (simple-circuit
         (cadr exp)                          ;; Lista de entradas
         (caddr exp)                         ;; Lista de salidas
         (parse-chip (cadddr exp)))]         ;; Parsea el chip asociado

      ;; Si 'exp' es un circuito complejo
      [(eqv? (car exp) 'complex-circuit)
       (complex-circuit
         (parse (cadr exp))                  ;; Parsea el circuito interno
         (map parse (caddr exp))             ;; Parsea cada circuito en la lista
         (cadddr exp)                         ;; Lista de entradas
         (cadddr (cdr exp)))])))              ;; Lista de salidas


(define unparse-chip
  (lambda (chip-datatype)
    (cond
      ['(prim-chip? chip-datatype)
       (list 'prim-chip '(unparse-chip-prim '(prim-chip->ent chip-datatype)))]  ; Usa el unparser de chip-prim
      ['(comp-chip? chip-datatype)
       (list 'comp-chip 
             '(comp-chip->in chip-datatype) 
             '(comp-chip->out chip-datatype) 
             '(unparse-circuit (comp-chip->circ chip-datatype)))])))  ; Cambia a unparse-circuit

(define unparse
  (lambda (circuito-datatype)
    (cond
      ['(simple-circuit? circuito-datatype)
       (list 'simple-circuit 
             '(simple-cir->in circuito-datatype) 
             '(simple-cir->out circuito-datatype) 
             (unparse-chip '(simple-cir->chip circuito-datatype)))]
      ['(complex-circuit? circuito-datatype)
       (list 'complex-circuit 
             (unparse '(complex-cir->circ circuito-datatype)) 
             (map unparse '(complex-cir->lcircs circuito-datatype)) 
             '(complex-cir->in circuito-datatype) 
             '(complex-cir->out circuito-datatype))]
      [else ('error "Tipo no reconocido")])))



;; ejem3: Un circuito que utiliza XOR y NAND para combinar tres entradas
(define cir3
  (simple-circuit
    '(A B C)         ; Entradas del circuito
    '(OUT)           ; Salida del circuito
    (comp-chip       
      '(A B C)       
      '(OUT)         
      (complex-circuit
        (simple-circuit '(A B) '(w1) (prim-chip (chip_xor))) ;
        (list
          (simple-circuit '(w1 C) '(w2) (prim-chip (chip_nand))) ;
          (simple-circuit '(w2) '(OUT) (prim-chip (chip_or))))  ; 
        '(A B C)      
        '(OUT)))))    ; Salida del circuito complejo

;; ejem4: Circuito de un sumador de medio usando XOR y AND
(define cir4
  (simple-circuit
    '(A B)           ; Entradas del circuito (dos bits)
    '(SUM CARRY)     ; Salidas del circuito (suma y acarreo)
    (comp-chip       
      '(A B)         ; Entradas del chip
      '(SUM CARRY)   ; Salidas del chip
      (complex-circuit
        (simple-circuit '(A B) '(SUM) (prim-chip (chip_xor)))   ; XOR
        (list
          (simple-circuit '(A B) '(CARRY) (prim-chip (chip_and)))) ; AND
        '(A B)        
        '(SUM CARRY)))))


(define cir5
  (comp-chip
    '(INA INB INC IND)   
    '(OUT)               
    (complex-circuit
      ;; Parte 1: AND entre INA y INB
      (simple-circuit '(INA INB) '(w1) (prim-chip (chip_and)))  ; w1 = INA AND INB

      ;; Parte 2: AND entre INC e IND
      (list
        (simple-circuit '(INC IND) '(w2) (prim-chip (chip_and))) ; w2 = INC AND IND

        ;; Parte 3: OR entre los dos resultados anteriores
        (simple-circuit '(w1 w2) '(OUT) (prim-chip (chip_or)))   ; OUT = w1 OR w2
      )
      '(INA INB INC IND)  
      '(OUT)              
    )
  )
)