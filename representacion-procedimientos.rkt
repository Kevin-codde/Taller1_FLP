#lang eopl
#|

   Autores: Kevin Andres Bejarano - 2067678
            Juan David Gutierrez Florez - 2060104
            Johan Sebastián Laverde pineda - 2266278
            Johan Sebastian Acosta Restrepo 2380393
   
|#



;Constructor1 chip prim
;<chip prim> := prim_or
;              chip−or()
;           := prim and
;               chip−and()
;           := prim_not
;              chip−not()
;            := prim_xor
;              chip−xor()
;           := prim_nand
;              chip−xor()


(define chip_or
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_or]
        [else (eopl:error "Señal no reconocida en prim_or")]
      )
    )
  )
)

(define chip_and
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_and]
        [else (eopl:error "Señal no reconocida en prim_and")]
      )
    )
  )
)

(define chip_not
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_not]
        [else (eopl:error "Señal no reconocida en prim_not")]
      )
    )
  )
)

(define chip_xor
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_xor]
        [else (eopl:error "Señal no reconocida en prim_not")]
      )
    )
  )
)

(define chip_nand
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_nand]
        [else (eopl:error "Señal no reconocida en prim_not")]
      )
    )
  )
)

(define prim_nor
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_nor]
        [else (eopl:error "Señal no reconocida en prim_not")]
      )
    )
  )
)

(define chip_xnor
  (lambda ()
    (lambda (señal)
      (cond
        [(= señal 0) 'chip_xnor]
        [else (eopl:error "Señal no reconocida en prim_not")]
      )
    )
  )
)



;;Constructor2 chip
 ;  <chip> := <chip prim>
 ;           prim-chip (chip-prim)
 ;         := chip ( --> {(port)}* )
 ;           ( <-- {(port)}* )
 ;           <circuito>
 ;            comp-chip (in, out, circ)

(define prim-chip
  (lambda (chip_pr)
    (lambda (señal)
      (cond
        [(= señal 0) 'prim-chip]
        [(= señal 1) chip_pr]
        [else (eopl:error "Señal no reconocida en prim-chip")]
      )
    )
  )
)


;comp-chip

(define comp-chip
  (lambda (in out circ)
    (lambda (señal)
      (cond
        [(= señal 0) 'comp-chip]
        [(= señal 1) in]
        [(= señal 2) out]
        [(= señal 3) circ]
        [else (eopl:error "Señal no reconocida en comp-chip")]
      )
    )
  )
)


;<circuito> := circsimple ({cable }∗) ({cable}∗)
;             <chip>
;              simple−circuit(in out chip)
;          := circcomp <circuito> {<circuito>}+
;              input{cable}∗
;              output{cable}∗
;              complex−circuit(circ lcircs in out)



;; constructores
(define simple-circuit
  (lambda (in out chip_integrado)
    (lambda (señal)
      (cond
        [(= señal 0) 'simple-circuit]
        [(= señal 1) in]
        [(= señal 2) out]
        [(= señal 3) chip_integrado]
        [else (eopl:error "Señal no reconocida en simple-circuit")]
      )
    )
  )
)

;;complex-circuit

(define complex-circuit
  (lambda (circ lcircs in out)
    (lambda (señal)
      (cond
        [(= señal 0) 'complex-circuit]
        [(= señal 1) circ]
        [(= señal 2) lcircs]
        [(= señal 3) in]
        [(= señal 4) out]
        [else (eopl:error "Señal no reconocida en complex-circuit")]
      )
    )
  )
)

;-----------------------------------------------------------------------------


;;
;; predicados chips
;;

(define prim-chip?
  (lambda (chip_proc)
    (equal? (chip_proc 0) 'prim-chip)
  )
)

(define comp-chip?
  (lambda (chip_proc)
    (equal? (chip_proc 0) 'comp-chip)
  )
)


;; predicados circuitos

(define simple-circuit?
  (lambda (circ_proc)
    (equal? (circ_proc 0) 'simple-circuit)
  )
)

(define complex-circuit?
  (lambda (circ_proc)
    (equal? (circ_proc 0) 'complex-circuit)
  )
)

;;
;;  extractores
;;



;get->type:  devuelve el tipo de circuito o chip con el que se identifica
;ejemplo: (get->type (simple-circuit '(e f) '(g) (prim-chip (prim_or)))) -> 'simple-circuit


(define get->type
  (lambda (c)
    (c 0)
  )
)


(define prim-chip->chip_pr
  (lambda (prim_chip_proc)
    (prim_chip_proc 1)
  )
)

(define comp-chip->in
  (lambda (chip_comp_proc)
    (chip_comp_proc 1)
  )
)

(define comp-chip->out
  (lambda (chip_comp_proc)
    (chip_comp_proc 2)
  )
)

(define comp-chip->cir
  (lambda (chip_comp_proc)
    (chip_comp_proc 3)
  )
)





;;extractores
(define simple-circuit->in
  (lambda (circ_simple_proc)
    (circ_simple_proc 1)
  )
)

(define simple-circuit->out
  (lambda (circ_simple_proc)
    (circ_simple_proc 2)
  )
)

(define simple-circuit->chip
  (lambda (circ_simple_proc)
    (circ_simple_proc 3)
  )
)

(define complex-circuit->circ
  (lambda (circ_comp_proc)
    (circ_comp_proc 1)
  )
)

(define complex-circuit->lcircs
  (lambda (circ_comp_proc)
    (circ_comp_proc 2)
  )
)

(define complex-circuit->in
  (lambda (circ_comp_proc)
    (circ_comp_proc 3)
  )
)

(define complex-circuit->out
  (lambda (circ_comp_proc)
    (circ_comp_proc 4)
  )
)





;;ejem1

(define cir1 (comp-chip
 
  '(INA INB INC IND)
  
  '(OUTA)
  
  (complex-circuit
   
    (simple-circuit '(a b) '(e)
      (prim-chip (chip_and)))
    
    (list
      (simple-circuit '(c d) '(f)
        (prim-chip (chip_and))

       )
        
      (simple-circuit '(e f) '(g)
        (prim-chip (chip_or))
       )
      
      )
    
    '(a b c d)
    
    '(g))

  ))



;;ejem2

(define cir2


 (complex-circuit
 (simple-circuit
  ' (m n o p)
  ' (e f)
  (comp-chip
   '(INA INB INC IND)
   '(OUTE OUTF)
   (complex-circuit
    (simple-circuit ' (a b) ' (e) (prim-chip (chip_and)))
    (list
     (simple-circuit ' (c d) ' (f) (prim-chip (chip_and))))
    ' (a b c d)
    ' (e f))

   ))
 
 (list
  (simple-circuit
   ' (e f)
   ' (z)
   (comp-chip
    '(INE INF)
    '(OUTA)
    (simple-circuit ' (e f) ' (g) (prim-chip (chip_or)))

    )

   ))
 
 ' (m n o p)
 ' (z)))


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



;;Ejemplo5
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