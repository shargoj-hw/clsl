(ns clsl.printer
  (require [clojure.string :as s]))

(defn- tabs [n] (s/join (repeat n "\t")))

(defn- unimplemented [x] (format "[[[Unimplemented print method: %s]]]" (str x)))

(defmulti ast->str (fn ([ast _] (or (:node-type ast) ast))))

(defmethod ast->str :root [ast ilevel]
  (s/join "\n" (map #(ast->str % ilevel) (:defns ast))))

(defmethod ast->str :decl [ast ilevel]
  (if (nil? (:initialized-to ast))
    (format "%s %s;" (ast->str (:type ast) ilevel) (:id ast))
    (format "%s %s = %s;" (ast->str (:type ast) ilevel) (:id ast) (ast->str (:initialized-to ast) ilevel))))

(defmethod ast->str :fun-def [ast ilevel] 
  (let [type (ast->str (:type ast) ilevel)
        name (str (:id ast))
        args (s/join ", " (map #(ast->str % ilevel) (:params ast)))
        body (ast->str (:body ast) ilevel)]
    (format "%s %s(%s) %s\n\n" type name args body)))

(defmethod ast->str :param-decl [ast ilevel]
  (format "%s %s" (ast->str (:type ast) ilevel) (str (:id ast))))

(defmethod ast->str :empty-expression [ast ilevel] "")

(defmethod ast->str :expression-stmt [ast ilevel] (str (ast->str (:expr ast) ilevel) ";"))

(defmethod ast->str :jump [ast ilevel]
  (case (:kind ast)
    :continue "continue;"
    :break "break;"
    :return (if-not (:expr ast) "return;" (format "return %s;" (ast->str (:expr ast) ilevel)))
    :discard "discard;"))

(defmethod ast->str :empty-stmt [ast ilevel] "")

(defmethod ast->str :compound-stmt [ast ilevel]
  (letfn [(tostr [ast] (str (tabs (inc ilevel)) (ast->str ast (inc ilevel))))] 
    (let [body (s/join "\n" (map tostr (or (:stmts ast) (:body ast))))
          end-tabs (tabs ilevel)]
      (format "{\n%s\n%s}\n" body end-tabs))))

(defmethod ast->str :while-loop [ast ilevel] 
  (let [condition (ast->str (:condition ast) ilevel)
        body (s/join "\n" (map #(ast->str % (inc ilevel)) (:body ast)))]
    (format "while (%s) {\n%s}\n" condition body)))

(defmethod ast->str :for-loop [ast ilevel] 
  (let [initializer (ast->str (:initializer ast) ilevel)
        condition (ast->str (:condition ast) ilevel)
        update (when (:update ast) (ast->str (:update ast) ilevel))
        body (ast->str (:body ast) ilevel)]
    (if update
      (format "for (%s %s; %s) %s" initializer condition update body)
      (format "for (%s %s) %s" initializer condition body))))

(defmethod ast->str :expression-list [ast ilevel]
  (s/join ", " (map #(ast->str % ilevel) (:expressions ast))))

(defmethod ast->str :assignment [ast ilevel] 
  (str (s/join " " [(ast->str (:to ast) ilevel) 
                    (ast->str (:oper ast) ilevel)
                    (ast->str (:value ast) ilevel)])))

(defmethod ast->str :ite [ast ilevel] (unimplemented ast))
(defmethod ast->str :void [_ _] "void")
(defmethod ast->str :condition [ast ilevel] (unimplemented ast))

(defmethod ast->str :bin-op [ast ilevel] 
  (format "%s %s %s"
          (ast->str (:left ast) ilevel)
          (ast->str (:oper ast) ilevel)
          (ast->str (:right ast) ilevel)))

(defmethod ast->str :unary-op [ast ilevel]
  (if (= (:side ast) 'left)
    (str (ast->str (:oper ast) ilevel) (ast->str (:sub-expr ast) ilevel))
    (str (ast->str (:sub-expr ast) ilevel) (ast->str (:oper ast) ilevel))))

(defmethod ast->str :array-index [ast ilevel]
  (str (ast->str (:expr ast) ilevel) "[" (ast->str (:index ast) ilevel) "]"))

(defmethod ast->str :field-get [ast ilevel]
  (str (ast->str (:expr ast) ilevel) "." (:field ast)))

(defmethod ast->str :identifier [ast ilevel] (str (:id ast)))

(defmethod ast->str :fun-call [ast ilevel] 
  (str (ast->str (:func ast) ilevel)
       (if (not (:args ast)) "()"
           (str "(" (s/join ", " (map #(ast->str % ilevel) (:args ast))) ")"))))

(defmethod ast->str :type-decl [ast ilevel]
  (let [qualifiers (if (:qualifiers ast) (str (s/join " " (map s/lower-case (:qualifiers ast))) " ") "")]
    (if (:array-spec ast)
      (format "%s%s[%s]" qualifiers (ast->str (:type ast) ilevel) (ast->str (:array-spec ast) ilevel))
      (format "%s%s" qualifiers (ast->str (:type ast) ilevel) (ast->str (:array-spec ast) ilevel)))))

(defmethod ast->str :type [ast ilevel] (str (:name ast)))

(defmethod ast->str :* [ast ilevel] "*")
(defmethod ast->str :div [ast ilevel] "/")
(defmethod ast->str :% [ast ilevel] "%")
(defmethod ast->str :+ [ast ilevel] "+")
(defmethod ast->str :- [ast ilevel] "-")
(defmethod ast->str :<< [ast ilevel] "<<")
(defmethod ast->str :>> [ast ilevel] ">>")
(defmethod ast->str :< [ast ilevel] "<")
(defmethod ast->str :> [ast ilevel] ">")
(defmethod ast->str :<= [ast ilevel] "<=")
(defmethod ast->str :>= [ast ilevel] ">=")
(defmethod ast->str :== [ast ilevel] "==")
(defmethod ast->str :!= [ast ilevel] "!=")
(defmethod ast->str :-- [ast ilevel] "--")
(defmethod ast->str :++ [ast ilevel] "++")
(defmethod ast->str := [ast ilevel] "=")
(defmethod ast->str :*= [ast ilevel] "*=")
(defmethod ast->str :div= [ast ilevel] "/=")
(defmethod ast->str :%= [ast ilevel] "%=")
(defmethod ast->str :+= [ast ilevel] "+=")
(defmethod ast->str :-= [ast ilevel] "-=")
(defmethod ast->str :<<= [ast ilevel] "<<=")
(defmethod ast->str :>>= [ast ilevel] ">>=")
(defmethod ast->str :&= [ast ilevel] "&=")
(defmethod ast->str :xor= [ast ilevel] "^=")
(defmethod ast->str :|= [ast ilevel] "|=")

(defmethod ast->str :default [ast ilevel] (str ast))

(use '[clsl.parse :as p])
(print (ast->str (p/parse (slurp "shaders/shader1.glsl")) 0))

