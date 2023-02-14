;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exersizes_lambda) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

λa.(a λb.(b a)) 
func
 bound var - a
 body - (a λb.(b a)) 
 app
  func exp - name - a
  arg exp - λb.(b a)
   func
    bound var - b
    body - (b a)
      app
       func exp name - b
       arg exp name - a

λx.λy.λz((z x) (z y))

func
 bound var - x
 body - λy.λz((z x) (z y))
   func
    bound var - y
    body - λz((z x) (z y))
     func
      bound var - z
      body ((z x) (z y))
        app
         func exp - (z x)
          app
           func exp - name - z
           arg exp - name - x
         func exp - (z y)
          app
            func exp - name - z
            arg exp - name - y
          
     
(λf.λg.(λh.(g h) f) λp.λq.p)

app
 func exp - λf.λg.(λh.(g h) f)
  func 
   bound var  f
   body - λg.(λh.(g h) f)
     func
      bound var g
      body - (λh.(g h) f)
        app 
         func exp - λh.(g h)
          func
           bound var h
            body (g h)
              app
               func exp - name - g
               arg exp - name - h
          arg exp- name - f 
       
arg exp  - λp.λq.p
 func
  bound var p
   body - λq.p
    func
     bound var q
      body - name - p



λfee.λfi.λfo.λfum(fum (fo  (fi fee)))

func
 bound var fee
 body - λfi.λfo.λfum(fum (fo  (fi fee)))
  func
   bound var fi
   body - λfo.λfum(fum (fo  (fi fee)))
    func
     bound var fo
     body - λfum(fum (fo  (fi fee)))
      func
       bound var fum
       body - (fum (fo (fi fee)))
        app
         func exp - name - fum
         arg exp -(fo (fi fee))
          app
           func exp - name fi
           arg exp - name fee
         
    
(λp.(λq.p λx.(x p)) λi.λj.(j i)) λa.λb.(a (a b))
app
func exp - (λp.(λq.p λx.(x p)) λi.λj.(j i))
app
 func exp - λp.(λq.p λx.(x p))
  func
   bound var p
   body -(λq.p λx.(x p))
    app
     func exp - λq.p
      func
       bound var q
       body - name - p
     arg exp - λx.(x p)
      func
       bound var x
       body (x p)
        app
         func exp - name - x
         arg exp - name - p
 arg exp - λi.λj.(j i)
  func
   bound var  i
   body λj.(j i)
    func
     bound var j
     body (j i)
      app
       func exp - name - j
       arg exp -  name - i
   
 arg exp - λa.λb.(a (a b))
  func
   bound var a
   body λb.(a (a b))
    func
     bound var b
     body (a (a b))
      app
       func exp - name - a
       arg exp (a b)
        app
         func exp - name - a
         arg exp - name - b
       
       
(λa.λb.(a b) (λa.λb.(a b) λx.x)))           


;;make pair
λfirst.λsecond.λfun.((fun first) second)

;;select second
λfirst.λsecond.second
;;select first
λfirst.λsecond.first


;;make triplet
λfirst.λsecond.λthird.λfun.(((fun first) second) third)

;;select first
λfirst.λsecond.λthird.first

;;select second
λfirst.λsecond.λthird.second

;;select third
λfirst.λsecond.λthird.third