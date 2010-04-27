(defn ph-eval* [expr scope input]
  (loop [scope* scope]
	(cond
	 (empty? expr) input
	 (contains? scope* (first expr))
	 ((scope* (first expr)) (rest expr) scope input scope*)
	 :else (recur (scope* 'outer)))))

(defn ph-valid? [val]
  (cond
   (not (seq? val)) false
   (empty? val) true
   (not (seq? (first val))) false
   (empty? (first val)) (recur (rest val))
   :else (recur (cons (first (first val))
		      (cons (rest (first val)) (rest val))))))

(defn ph-fn [body]
  (fn [expr scope input def-scope]
      #(ph-eval* body def-scope expr)))

(defn ph-quote [expr scope input def-scope]
  expr)

(defn ph-let [expr scope input def-scope]
  (if (empty? expr)
      ()
    (loop [bindings (first expr) scope* {'outer scope}]
	  (cond
	   (empty? bindings) #(ph-eval* (rest expr) scope* input)
	   (empty? (first bindings)) (recur (rest bindings) scope*)
	   :else (recur (rest bindings)
			(assoc scope*
			       (first (first bindings))
			       (ph-fn (rest (first bindings)))))))))

(defn ph-car [expr scope input def-scope]
  (let [expr* (trampoline ph-eval* expr scope input)]
       (if (empty? expr*) () (first expr*))))

(defn ph-cdr [expr scope input def-scope]
  (let [expr* (trampoline ph-eval* expr scope input)]
       (if (empty? expr*) () (rest expr*))))

(defn ph-cons [expr scope input def-scope]
  (if (empty? expr)
      ()
    (let [head (trampoline ph-eval* (first expr) scope input)
	 tail (trampoline ph-eval* (rest expr) scope input)]
	 (cons head tail))))

(defn ph-if [expr scope input def-scope]
  (cond
   (or (empty? expr) (empty? (rest expr))) ()
   (not (empty? (trampoline ph-eval* (first expr) scope input)))
     #(ph-eval* (first (rest expr)) scope input)
   :else
     #(ph-eval* (rest (rest expr)) scope input)))

(defn ph-eval [expr scope input def-scope]
  (let [expr* (trampoline ph-eval* expr scope input)]
       (if (not (ph-valid? expr))
	   (throw (IllegalArgumentException. "Not valid Parenthesis Hell")))
       #(ph-eval* expr* scope input)))

(defn ph-eval
  "Evaluate a Parenthesis Hell expression with optional input."
  ([expr] (ph-eval expr ()))
  ([expr input]
     (if (or (not (ph-valid? expr))
	     (not (seq? input)))
	 (throw (IllegalArgumentException. "Not valid Parenthesis Hell")))
     (trampoline ph-eval* expr
		          {'() ph-quote
			   '(()) ph-let
			   '((())) ph-car
			   '(()()) ph-cdr
			   '((())()) ph-cons
			   '(()()()) ph-if
			   '(((()))) ph-eval}
			  input)))
