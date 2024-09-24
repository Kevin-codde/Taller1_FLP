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

