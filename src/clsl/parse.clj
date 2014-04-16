(ns clsl.parse
  (:require (instaparse [core :as insta]))
  (:use (clojure.core [match :only (match)])))

(def whitespace (insta/parser "whitepace := #'\\s+'"))

(def str->raw
  (insta/parser
   (slurp "grammars/glsl.bnf")
   :auto-whitespace whitespace
   :partial true
   :total true))

(declare
 raw->ast--decl
 raw->ast--stmt
 raw->ast--expr
 raw->ast--type
 raw->ast--initializer
 assn-oper-type
 oper->node-type)

(defn raw->ast [t]
  (match 
   t
   [:S [:translation_unit & r]]
   {:node-type :root :defns (vec (map raw->ast--decl r))}))

(defn raw->ast--decl [t]
  (match 
   t
   [:external_declaration decl] (raw->ast--decl decl)

   [:declaration [:init_declarator_list decl & rest] _]
   (if (empty? rest) (raw->ast--decl decl) "UNSUPPORTED MULTI-DECL")

   [:single_declaration type [:IDENTIFIER id]]
   {:node-type :decl :id id :type (raw->ast--type type)}
   
   [:single_declaration type [:IDENTIFIER id] [:EQUAL _] initializer]
   {:node-type :decl :id id :type (raw->ast--type type)
    :initialized-to (raw->ast--initializer initializer)}

   [:function_definition
    [:function_prototype
     [:function_declarator 
      [:function_header_with_parameters
       [:function_header type [:IDENTIFIER id] _]
       & params]] _]
    body]
   {:node-type :fun-def 
    :id id
    :type (raw->ast--type type)
    :params (vec (map raw->ast--decl (filter #(not (= :COMMA (first %))) params)))
    :body (raw->ast--stmt body)}

   [:parameter_declaration [:parameter_declarator type [:IDENTIFIER id]]]
   {:node-type :param-decl :id id :type (raw->ast--type type) }

   ;; TODO: is this only used for (void) types?
   [:parameter_declaration [:parameter_type_specifier type]]
   {:node-type :param-decl :type (raw->ast--type type)}

   :else (str "UNSUPPORTED DECL" " " t)))

(defn raw->ast--stmt [t]
  (match 
   t
   [:statement stmt] (raw->ast--stmt stmt)
   [:simple_statement stmt] (raw->ast--stmt stmt)

   [:declaration_statement d] (raw->ast--decl d)

   [:expression_statement [:SEMICOLON _]] 
   {:node-type :empty-expression}
   [:expression_statement expr _] 
   {:node-type :expression-stmt :expr (raw->ast--expr expr)}

   [:selection_statement & r] "selection_statement"
   [:switch_statement & r] "switch_statement"
   [:case_label & r] "case_label"
   [:iteration_statement & r] (raw->ast--stmt r)

   [:jump_statement [:CONTINUE _] _]
   {:node-type :jump :kind :continue}
   [:jump_statement [:BREAK _] _]
   {:node-type :jump :kind :break}
   [:jump_statement [:RETURN _] _]
   {:node-type :jump :kind :return}
   [:jump_statement [:RETURN _] expr _]
   {:node-type :jump :kind :return :expr (raw->ast--expr expr)}
   [:jump_statement [:DISCARD _] _]
   {:node-type :jump :kind :discard}

   [[:LEFT_BRACE _] [:RIGHT_BRACE _]]
   {:node-type :empty-stmt}
   
   [[:LEFT_BRACE _] [:statement_list & stmts] [:RIGHT_BRACE _]]
   {:node-type :compound-stmt :stmts (vec (map raw->ast--stmt stmts))}

   [:compound_statement_no_new_scope _ [:statement_list & body] _]
   {:node-type :compound-stmt :stmts (vec (map raw->ast--stmt body))}

   [[:WHILE _] _ [:condition condition] _ body]
   {:node-type :while-loop 
    :condition (raw->ast--expr condition)
    :body (raw->ast--stmt body)}

   [[:FOR _]
    _
    [:for_init_statement s-or-d]
    [:for_rest_statement cond _]
    _
    [:statement_no_new_scope body]]
   {:node-type :for-loop
    :initializer 
    (match 
     s-or-d
     [:expression-stmt e] (raw->ast--expr e)
     [:declaration_statement d] (raw->ast--decl d))
    :condition (raw->ast--expr cond)
    :body (raw->ast--stmt body)}

   [[:FOR _] 
    _
    [:for_init_statement s-or-d]
    [:for_rest_statement cond _ update]
    _
    [:statement_no_new_scope body]]
   {:node-type :for-loop
    :initializer 
    (match 
     s-or-d
     [:expression-stmt e] (raw->ast--expr e)
     [:declaration_statement d] (raw->ast--decl d))
    :condition (raw->ast--expr cond)
    :update (raw->ast--expr update)
    :body (raw->ast--stmt body)}
   
   :else (str "UNSUPPORTED STMT" " " t)))

(defn raw->ast--expr [t]
  (match 
   t

   [:expression & r] 
   {:node-type :expression-list :expressions (vec (map raw->ast--expr r))}

   [:assignment_expression conditional]
   (raw->ast--expr conditional)

   [:assignment_expression unary_expression [:assignment_operator oper] expr]
   {:node-type :assignment 
    :oper (assn-oper-type oper)
    :to (raw->ast--expr unary_expression)
    :value (raw->ast--expr expr)}

   [:conditional_expression cond] (raw->ast--expr cond)
   [:conditional_expression cond _ then _ else] 
   {:node-type :ite :cond (raw->ast--expr cond) 
    :then (raw->ast--expr then)
    :else (raw->ast--expr else)}

   [:conditionopt] {:node-type :void}
   [:conditionopt condition] (raw->ast--expr condition)
   
   [:condition expr] (raw->ast--expr expr)
   [:condition type [:IDENTIFIER id] _ initializer]
   {:node-type :condition :type (raw->ast--type type) :id id
    :initialized-to (raw->ast--initializer initializer)}

   ;;;; Logic Operations

   [:and_expression expr] (raw->ast--expr expr)
   [:and_expression l_expr [:AMPERSAND _] r_expr] 
   {:node-type :bin-op :oper '&
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:exclusive_or_expression expr] (raw->ast--expr expr)
   [:exclusive_or_expression l_expr [:CARET _] r_expr] 
   {:node-type :bin-op :oper '^
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:inclusive_or_expression expr] (raw->ast--expr expr)
   [:inclusive_or_expression l_expr [:VERTICAL_BAR _] r_expr] 
   {:node-type :bin-op :oper '|
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:logical_and_expression expr] (raw->ast--expr expr)
   [:logical_and_expression l_expr [:AND_OP _] r_expr] 
   {:node-type :bin-op :oper '&
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:logical_xor_expression expr] (raw->ast--expr expr)
   [:logical_xor_expression l_expr [:XOR_OP _] r_expr] 
   {:node-type :bin-op :oper 'log-xor
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:logical_or_expression expr] (raw->ast--expr expr)
   [:logical_or_expression l_expr [:OR_OP _] r_expr] 
   {:node-type :bin-op :oper '||
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   ;;;; Math Operations

   [:multiplicative_expression expr] (raw->ast--expr expr)
   [:multiplicative_expression l_expr oper r_expr] 
   {:node-type :bin-op :oper (oper->node-type oper)
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:additive_expression expr] (raw->ast--expr expr)
   [:additive_expression l_expr oper r_expr] 
   {:node-type :bin-op :oper (oper->node-type oper) 
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:shift_expression expr] (raw->ast--expr expr)
   [:shift_expression l_expr oper r_expr] 
   {:node-type :bin-op :oper (oper->node-type oper) 
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   [:relational_expression expr] (raw->ast--expr expr)
   [:relational_expression l_expr oper r_expr] 
   {:node-type :bin-op :oper (oper->node-type oper) 
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   ;;;; Equality Operations

   [:equality_expression e] (raw->ast--expr e)
   [:equality_expression l_expr oper r_expr]
   {:node-type :bin-op :oper (oper->node-type oper) 
    :left (raw->ast--expr l_expr)
    :right (raw->ast--expr r_expr)}

   ;; ;;;; Unary Expressions
   ;; ;; TODO: replace :side with something more concrete
   [:unary_expression expr] (raw->ast--expr expr)
   [:unary_expression oper expr] 
   {:node-type :unary-op :oper (oper->node-type oper) 
    :side :left
    :sub-expr (raw->ast--expr expr)}

   [:postfix_expression expr] (raw->ast--expr expr)
   [:postfix_expression postfix_expression _ expr _] 
   {:node-type :array-index :expr (raw->ast--expr postfix_expression) :index (raw->ast--expr expr)}
   [:postfix_expression [:function_call r]] (raw->ast--expr r)   
   [:postfix_expression
    expr
    [:DOT _]
    [:FIELD_SELECTION [:IDENTIFIER id]]] 
   {:node-type :field-get :expr (raw->ast--expr expr) :field id}
   [:postfix_expression expr oper]
   {:node-type :unary-op :oper (oper->node-type oper)
    :side :right
    :sub-expr (raw->ast--expr expr)}

   [:primary_expression [:variable_identifier [:IDENTIFIER v]]]
   {:node-type :identifier :id v}

   [:primary_expression c] (raw->ast--expr c)

   [:INTCONSTANT [_ v]] v
   [:INTCONSTANT v] v
   [:UINTCONSTANT [_ v]] v
   [:FLOATCONSTANT v] v
   [:BOOLCONSTANT v] v
   [:DOUBLECONSTANT v] v

   [:primary_expression _ expr _] (raw->ast--expr expr)

   ;;;; Function Call Expressions
   [:function_call [:function_call_or_method f]]
   (raw->ast--expr f)

   [:function_call_generic fc _] (raw->ast--expr fc)

   [:function_call_header_no_parameters fch _] 
   {:node-type :fun-call :func (raw->ast--expr fch)}
   [:function_call_header_no_parameters fch] 
   {:node-type :fun-call :func (raw->ast--expr fch)}

   [:function_call_header_with_parameters fch & r]
   {:node-type :fun-call :func (raw->ast--expr fch) 
    :args
    (let [actual-args
          (filter (fn [a] (match a [:COMMA _] false :else true)) r)]
      (vec (map raw->ast--expr actual-args)))}

   [:function_call_header function_identifier _]
   (match 
    function_identifier
    [:function_identifier [:type_specifier & r]]
    (raw->ast--type (second function_identifier))
    [:function_identifier [:postfix_expression &r]]
    (raw->ast--expr (second function_identifier)))

   :else (str "UNSUPPORTED EXPR" " " t)))

(defn raw->ast--type [t]
  (match
   t
   [:fully_specified_type qual type]
   (conj (raw->ast--type type) (raw->ast--type qual))

   [:fully_specified_type type] (raw->ast--type type)

   [:type_specifier nonarr arr]
   {:node-type :type-decl
    :type (raw->ast--type nonarr)
    ;; TODO: fix this to be better somehow?
    :array-spec (raw->ast arr)}

   [:type_specifier nonarr]
   {:node-type :type-decl 
    :type (raw->ast--type nonarr)}

   [:type_specifier_nonarray [:TYPE_NAME [:IDENTIFIER id]]] 
   {:node-type :type :name id}

   [:type_qualifier & r] 
   {:qualifiers (vec (map (fn [f] (name (match f [_ [_ [ty _]]] ty))) r))}
   
   :else (str "UNSUPPORTED TYPE" " " t)))

(defn raw->ast--initializer [t]
  (match
   t
   [:initializer e] (raw->ast--expr e)
   [:initializer _ initializer_list _] "INITIALIZER LISTS UNSUPPORTED"
   [:initializer _ initializer_list _ _] "INITIALIZER LISTS UNSUPPORTED"

   :else (throw (str "BAD INITIALIZER" " " t))))

(defn oper->node-type [oper]
  (match 
   oper
   [:STAR _] :*
   [:SLASH _] :div
   [:PERCENT _] :%
   [:PLUS _] :+
   [:DASH _] :-
   [:LEFT_OP _] :<<
   [:RIGHT_OP _] :>>
   [:LEFT_ANGLE _] :<
   [:RIGHT_ANGLE _] :>
   [:LE_OP _] :<=
   [:GE_OP _] :>=
   [:EQ_OP _] :==
   [:NE_OP _] :!=
   [:DEC_OP _] :--
   [:INC_OP _] :++
   :else (throw (str "Unrecognized operator" oper))))

(defn assn-oper-type [assn]
  (match 
   assn
   [:EQUAL _] :=
   [:MUL_ASSIGN _] :*=
   [:DIV_ASSIGN _] :div=
   [:MOD_ASSIGN _] :%=
   [:ADD_ASSIGN _] :+=
   [:SUB_ASSIGN _] :-=
   [:LEFT_ASSIGN _] :<<=
   [:RIGHT_ASSIGN _] :>>=
   [:AND_ASSIGN _] :&=
   [:XOR_ASSIGN _] :xor=
   [:OR_ASSIGN _] :|=))

(def raw-parsed (str->raw (slurp "shaders/shader1.glsl")))
(raw->ast raw-parsed)

(defn parse [s] (-> s str->raw raw->ast))
