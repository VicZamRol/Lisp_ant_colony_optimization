;;;---------------------------------------------------------------------------
;;;---- ANT COLONY OPTIMIZATION : Knapsack Problem implementation
;;;---------------------------------------------------------------------------

;;;----------------------------
;;;------ IMPLEMENTATION ------
;;;----------------------------

;List with the object which you can put into the backpack

(defvar *objetos* nil)

;Object Structure

(defstruct (objeto (:constructor crea-objeto)
		   (:conc-name)
		   (:print-function escribe-objeto))
  numero     ;Indice del objeto
  peso
  utilidad
  ti         ;pheromone_map.Small value in the begining
)

;Modify object funtion

(defun escribe-objeto (objeto &optional (canal t) profundidad)
  (format canal "~& Objeto: ~a ~& Peso: ~a ~& Utilidad: ~a ~& Concentración feromonas: ~a"
	  (numero objeto) (peso objeto) (utilidad objeto) (ti objeto)))


;Read Folder Funtions

(defun parse-string-to-float (line)
  (with-input-from-string (s line)
    (loop
       :for num := (read s nil nil)
       :while num
       :collect num)))


(defun lista-fichero (fichero)
  (let ((in (open fichero :if-does-not-exist nil))
	dev)
    (when in
      (setf dev (loop for line = (read-line in nil)
	 while line collect (parse-string-to-float line)))
      (close in))
    dev))

;Extra Funtions

;Return object, given an index parameter

(defun busca-objeto (x)
  (loop for i in *objetos* do
       (if (= x (numero i))
	   (return i))))

;Read capacity backpack file and object file and save in a global variable

(defun inicializa (fichero)
  (let ((parametros (lista-fichero fichero))
	(capacidad))
    (setf capacidad (first (first parametros)))
    (setf *objetos* (loop for x in (rest parametros) collect
			 (crea-objeto :numero (first x) :peso (second x) :utilidad (third x) :ti 1)))
    capacidad))

;Choose a candidate by probability

(defun probabilidad (l)
  (let ((alea (random 100))
	(acum 0))
    (loop for x in l do
	 (if (<  alea (+ acum (* 100 (second x))))
	     (return (first x))
	     (setf acum (+ acum (* 100 (second x))))))))

;Return the candidate.

(defun elige-candidato (candidatos)
  (let ((l '())       ;lista de pares en la que almacenamos (obj fi) donde obj es el índice del objeto y fi la variable fi
	(l2 '())      ;Lista de pares en la que almacenamos (obj prob) donde obj es el índice del objeto y prob es la probabilidad de ser elegido
	(fi 0)        ;Variable local en la que almacenamos la probabilidad relativa del objeto
	(obj)
	(ti)
	(w)
	(v)
	(alfa 1)
	(beta 1)
	(gamma 1) 
	(sumatorio 0) ;Variable local en la que almacenamos la frecuencia acumulada
	(res))
    (cond ((= (length candidatos) 1)         ;Si tiene un único candidato se devuelve ese
	   (setf res (first candidatos)))
	  (t (loop for x in candidatos do    ;Si no, calculamos fi de cada objeto y el acumulado
		(setf obj (busca-objeto x))
		(setf ti (ti obj))
		(setf w (peso obj))
		(setf v (utilidad obj))
		(setf fi (* (expt ti alfa) (* (expt (/ 1.0 w) beta) (expt (- 1.0 (/ 1.0 v)) gamma))))
		(setf l (append l (list (list x fi))))
		(setf sumatorio (+ sumatorio fi)))
	   (loop for x in l do               ;Creamos la lista con la probabilidad de cada objeto
		(setf l2 (append l2 (list (list (first x) (/ (second x) sumatorio))))))
	   (setf res (probabilidad l2))))
    res))

;Delete element from the list

(defun elimina (x l)
  (cond ((endp l) l)
	((equal x (first l)) (rest l))
	(t (cons (first l) (elimina x (rest l))))))

;Return list of final candidates objects.

(defun dame-candidatos (obj pes cap)
  (let ((l '()))
    (loop for x in *objetos* do
	 (if (not (= obj (numero x)))
	     (if (>= cap (+ pes (peso x)))
		 (setf l (append l (list (numero x)))))))l))

;Return a solution (v l), where v is the solution generated and l is a list whit the object index

(defun construye-solucion (i capacidad)
  (let ((aux)
	(p)
	(u)
	(s '())          ;Lista con los objetos que pertenecen a la solución
	(candidatos)
	(aux2)
	(sobrepasa 0))   ;Variable para saber cuando se sobrepasa la capacidad de la mochila
    (setf aux (nth (- i 1) *objetos*))
    (setf s (append s (list i)))
    (setf p (peso aux))
    (setf u (utilidad aux))
    (setf candidatos (dame-candidatos (numero aux) p capacidad))     ;Lista con los candidatos que caben en la mochila
    (loop while (and (> (length candidatos) 0) (= sobrepasa 0)) do   ;Mientras candidatos tenga elementos y no se sobrepase la capacidad
	 (setf aux2 (elige-candidato candidatos))                    ;Elegimos un candidato
	 (setf aux (busca-objeto aux2))	 
	 (cond ((and (> (+ p (peso aux)) capacidad) (= (length candidatos) 1))   ;Si queda un elemento y se sobrepasa la capacidad, acabamos
		(setf sobrepasa 1))
	       ((and (> (+ p (peso aux)) capacidad) (> (length candidatos) 1))   ;Si sobrepasa la capacidad, lo eliminamos
		(setf candidatos (elimina aux2 candidatos)))
	       (t (setf s (append s (list aux2)))                                ;Añadimos el objeto a la solución
		  (setf p (+ p (peso aux)))
		  (setf u (+ u (utilidad aux)))
		  (setf candidatos (elimina aux2 candidatos))))) (list u s)))

;Retunr TRUE if the element is in the solution list

(defun pertenece (e c)
  (cond ((endp c) nil)
	((equalp (first c) e) t)
	(t (pertenece e (rest c)))))

;pheromone_map update funtion

(defun actualiza-rastro-feromonas (l)
  (loop for x in *objetos* do
       (cond ((pertenece (numero x) l)
	      (setf (ti x) (* (ti x) 1.5)))
	     (t (setf (ti x) (* (ti x) 0.9))))))

;Return best solution

(defun mejor-solucion (soluciones)
  (let ((max 0)
	(sol))
    (loop for x in soluciones do
	 (cond ((> (first x) max)
		(setf max (first x))
		(setf sol x)))) sol))
 
;Return optimize solution

(defun colonia-hormigas (fichero)
  (let ((aux)
	(aux2)
	(capacidad)
	(mejor 0)
	(res))
    (setf capacidad (inicializa fichero))
    (loop for x from 1 to 30 do
	 (setf aux (loop for i from 1 to (length *objetos*)       ;Construimos la solución de cada hormiga
			collect (construye-solucion i capacidad)))
	 (setf aux2 (mejor-solucion aux))     ;Elegimos la mejor solución de esta iteración
	 (cond ((> (first aux2) mejor)        ;Comprobamos si esa solución es mejor que las de las iteraciónes anteriores
		(setf mejor (first aux2))
		(setf res aux2)))
	 (actualiza-rastro-feromonas (second aux2))) res))

;Return weight of the objects

(defun peso-lista (l)
  (loop for x in l 
     summing (peso (busca-objeto x))))


(defun utilidad-lista (l)
  (loop for x in l
       summing (utilidad (busca-objeto x))))


(defun subconjunto (c1 c2)
  (cond ((endp c1) t)
        ((pertenece (first c1) c2) (subconjunto (rest c1) c2))
        (t nil)))


(defun iguales (c1 c2)
  (and (subconjunto c1 c2)
       (subconjunto c2 c1)))

;try to find a new solution changes some objects in the current solution

(defun busqueda-local-hormiga (solucion capacidad)
  (let ((modificado 0)                 ;Variable usada para salir de los bucles cuando se haya encontrado un "vecion" mejor
	(objeto)
	(utilidad (first solucion))    ;Utilidad de la solución de entrada
	(aux)
	(aux2)
	(aux3))
    (loop for x in (second solucion) when (= modificado 0) do    ;Iteramos los objetos de la solución
	 (setf objeto (busca-objeto x))
	 (loop for obj in *objetos* when (= modificado 0) do     ;Iteramos los objetos
	      (cond ((and (not (= x (numero obj)))               ;En el caso de que se haya encontrado un "vecion" mejor
			  (not (pertenece (numero obj) (second solucion)))
			  (> capacidad (+ (peso-lista (elimina x (second solucion))) (peso obj)))
			  (> (+ (utilidad-lista (elimina x (second solucion))) (utilidad obj)) utilidad))
		     ;Modificamos la solución con el nuevo objeto y la devolvemos
		     (setf aux (- utilidad (utilidad objeto)))
		     (setf solucion (list aux (elimina x (second solucion))))
		     (setf aux2 (+ (first solucion) (utilidad obj)))
		     (setf aux3 (append (second solucion) (list x)))
		     (setf solucion (list aux2 aux3))
		     (setf modificado 1))))) solucion))

;Main funtion to modify the local iterations

(defun colonia-hormigas-iterativa (fichero)
  (let ((aux)
	(aux2 '())
	(aux3)
	(capacidad)
	(mejor 0)
	(res)
	(bl)
	(modificado 0))
    (setf capacidad (inicializa fichero))
    (loop for x from 1 to 30 do
	 (loop for i from 1 to (length *objetos*) do
	      (setf aux (construye-solucion i capacidad))
	      (loop while (= modificado 0) do                ;En este bucle realizamos la modificación de la busqueda local
		   (setf bl (busqueda-local-hormiga aux capacidad))
		   (cond ((iguales (second bl) (second aux)) ;Si no se ha modificado la solución salimos del bucle
			  (setf modificado 1)) 
			 (t (setf aux bl))))           ;Si se ha modificado volvemos a comprobar si hay un "vecino" mejor para esta solución
	      (setf aux2 (append aux2 (list aux))))
	 (setf aux3 (mejor-solucion aux2))             ;Elegimos la mejor iteración de esta solución
	 (cond ((> (first aux3) mejor)                 ;Comprobamos si esa solución es mejor que las de las iteraciónes anteriores
		(setf mejor (first aux3))
		(setf res aux3)))
	 (actualiza-rastro-feromonas (second aux3))) res))

