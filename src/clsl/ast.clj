(ns clsl.ast)

(defmacro def-ast-type
  [name components]
  `(do 
     (defrecord ~(symbol (str name "-rec")) ~components)
     (def ~name ~(symbol (str "->" name "-rec")))))

(def-ast-type root [defns])
(def-ast-type decl [id type maybe-initialized-to])
(def-ast-type fun-def [id type params body])
(def-ast-type param-decl [type maybe-id])
(def-ast-type empty-expression [])
(def-ast-type expression-stmt [expr])
(def-ast-type jump [kind maybe-value])
(def-ast-type empty-stmt [])
(def-ast-type compound-stmt [stmts])
(def-ast-type while-loop [condition body])
(def-ast-type for-loop [initializer condition maybe-update body])
(def-ast-type expression-list [expressions])
(def-ast-type assignment [oper to value])
(def-ast-type ite [cond then else])
(def-ast-type void [])
(def-ast-type condition [type id initialized-to])
(def-ast-type bin-op [oper left right])
(def-ast-type unary-op [oper side sub-expr])
(def-ast-type array-index [expr index])
(def-ast-type field-get [expr field])
(def-ast-type unary-op [oper side sub-expr])
(def-ast-type identifier [id])
(def-ast-type fun-call [func maybe-args])
(def-ast-type type-decl [type maybe-array-spec])
(def-ast-type full-type [name maybe-qualifiers])
