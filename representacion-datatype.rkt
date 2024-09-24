#lang eopl

#|

   Autores: Kevin Andres Bejarano - 2067678
            Juan David Gutierrez Florez - 2060104
            Johan Sebastián Laverde pineda - 2266278
            Johan Sebastian Acosta Restrepo 2380393
   
|#

;;


;Datatypes

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
;              simple−circuit(in out chip)
;          := circcomp <circuito> {<circuito>}+
;              input{cable}∗
;              output{cable}∗
;              complex−circuit(circ lcircs in out)


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



;;ejem5

(define chipP (chip_and))