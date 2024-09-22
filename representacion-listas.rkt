#lang racket
#|

   Autores: Kevin Andres Bejarano - 2067678
            Juan David Gutierrez Florez - 2060104
            Johan Sebastián Laverde pineda - 2266278
            Johan Sebastian Acosta Restrepo 2380393
   
|#





;;Constructores

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
       '(chip_or())
    )
)

(define chip_and
  (lambda ()
       '(chip_and())
    )
)

(define chip_not
  (lambda ()
       '(chip_not())
    )
)

(define chip_xor
  (lambda ()
       '(chip_xor())
    )
)

(define chip_nand
  (lambda ()
       '(chip-nand())
    )
)

(define chip_nor
  (lambda ()
       '(chip-nor())
    )
)

(define chip_xnor
  (lambda ()
       '(chip-xnor())
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
  (lambda (chip-prim)
        (list 'chip-prim chip-prim
    )
))





;comp-chip

(define comp-chip

  (lambda (in out circ)

    (list 'comp-chip in out circ)

  )
 )




;constructor4 circuitos

;<circuito> := circsimple ({cable }∗) ({cable}∗)
;             <chip>
;              simple−circuit(in out chip)
;          := circcomp <circuito> {<circuito>}+
;              input{cable}∗
;              output{cable}∗
;              complex−circuit(circ lcircs in out)



;;simple-circuit

(define simple-circuit

  (lambda (in out chip)

      (list 'simple-circuit in out chip)

   )
)

;;complex-circuit

(define complex-circuit
  (lambda (circ lcircs in out)

     (list 'complex-circuit circ lcircs in out)

  )
)


;-----------------------------------------------------------------------------


;;Observadores


;;Predicados

(define chip-prim?
  (lambda (n)
    (equal? (car n) 'chip-prim)))

(define prim-chip?
  (lambda (n)
    (equal? (car n) 'prim-chip)))

(define comp-chip?
  (lambda (n)
    (equal? (car n) 'comp-chip)))

(define simple-circuit?
  (lambda (n)
    (equal? (car n) 'simple-circuit)))

(define complex-circuit?
  (lambda (n)
    (equal? (car n) 'complex-circuit)))


;;Extractores

;get->type:  devuelve el tipo de circuito o chip con el que se identifica
;ejemplo: (get->type (simple-circuit '(e f) '(g) (prim-chip (chip_or)))) -> 'simple-circuit

(define get->type
  (lambda (c)
    (car c)
  )
 )

;simple-cir->in: Devuelve el/los puertos de entrada de un circuito simple
(define simple-cir->in
  (lambda(c)
      (cadr c)
  )
)

;simple-cir->out: Devuelve el/los puertos de salida de un circuito simple
(define simple-cir->out
  (lambda(c)
      (caddr c)
  )
)

;simple-cir->chip: Devuelve el chip de de un circuito simple
(define simple-cir->chip
  (lambda(c)
      (cadddr c)
  )
)

;complex-cir->circ: Devuelve el circuito de entrada de un circuito complejo
(define complex-cir->circ
  (lambda(c)
      (cadr c)
  )
)

;complex-cir->lcircs: Devuelve una lista de circuitos de entrada de un circuito complejo
(define complex-cir->lcircs
  (lambda(c)
      (caddr c)
  )
)

;complex-cir->in: Devuelve la entrada de un circuito complejo
(define complex-cir->in
  (lambda(c)
      (cadddr c)
))

;complex-cir->out: Devuelve la salida circuito de un circuito complejo
(define complex-cir->out
  (lambda(c)
      (car(cddddr c))
))

;--------------------------------------------------

;;Area Programador


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




