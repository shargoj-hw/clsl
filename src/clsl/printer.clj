(ns clsl.printer
  (require [clojure.string :as s]))

(defn- unimplemented [x] (format "[[[Unimplemented print method: %s]]]" (str x)))

(defmulti ast->str (fn ([ast _] (or (:node-type ast) ast))))

(defmethod ast->str :root [ast ilevel]
  (s/join "\n" (map #(ast->str % ilevel) (:defns ast))))

(defmethod ast->str :decl [ast ilevel]
  (format "%s %s" (ast->str (:type ast) ilevel) (:id ast)))

(defmethod ast->str :fun-def [ast ilevel] 
  (let [type (ast->str (:type ast) ilevel)
        name (str (:id ast))
        args (s/join ", " (map #(ast->str % ilevel) (:params ast)))
        body (ast->str (:body ast) ilevel)]
    (format "%s %s(%s) %s\n\n" type name args body)))

(defmethod ast->str :param-decl [ast ilevel]
  (format "%s %s" (ast->str (:type ast) ilevel) (str (:id ast))))

(defmethod ast->str :empty-expression [ast ilevel] "")

(defmethod ast->str :expression-stmt [ast ilevel] (ast->str (:expr ast) ilevel))

(defmethod ast->str :empty-stmt [ast ilevel] "")

(defmethod ast->str :compound-stmt [ast ilevel]
  (format "{\n%s\n}\n"
          (s/join "\n" (map #(str "\t"
                                  (ast->str % (inc ilevel))
                                  ";")
                            (or (:stmts ast) (:body ast))))))

(defmethod ast->str :while-loop [ast ilevel] 
  (let [condition (ast->str (:condition ast) ilevel)
        body (s/join "\n" (map #(ast->str % (inc ilevel)) (:body ast)))]
    (format "while (%s) {\n%s}n" condition body)))

(defmethod ast->str :for-loop [ast ilevel] 
  (let [initializer (ast->str (:initializer ast) ilevel)
        body (ast->str (:body ast) ilevel)]
    (format "for (%s) %s" initializer body)))

(defmethod ast->str :expression-list [ast ilevel]
  (s/join ", " (map #(ast->str % ilevel) (:expressions ast))))

(defmethod ast->str :assignment [ast ilevel] 
  (s/join " " [(ast->str (:to ast) ilevel) 
               (ast->str (:oper ast) ilevel)
               (ast->str (:value ast) ilevel)]))

(defmethod ast->str :ite [ast ilevel] (unimplemented ast))
(defmethod ast->str :void [_ _] "void")
(defmethod ast->str :condition [ast ilevel] (unimplemented ast))

(defmethod ast->str :bin-op [ast ilevel] 
  (str (ast->str (:left ast) ilevel)
       (ast->str (:oper ast) ilevel)
       (ast->str (:right ast) ilevel)))

(defmethod ast->str :unary-op [ast ilevel]
  (if (= (:side ast) 'left)
    (str (:oper ast) (:sub-expr ast))
    (str (:sub-expr ast) (:oper ast))))

(defmethod ast->str :array-index [ast ilevel]
  (str (ast->str (:expr ast) ilevel) "[" (ast->str (:index ast) ilevel) "]"))

(defmethod ast->str :field-get [ast ilevel]
  (str (ast->str (:expr ast) ilevel) (:field ast)))

(defmethod ast->str :identifier [ast ilevel] (str (:id ast)))

(defmethod ast->str :fun-call [ast ilevel] 
  (str (ast->str (:func ast) ilevel)
       (if (not (:args ast)) "()"
           (str "(" (s/join ", " (map #(ast->str % ilevel) (:args ast))) ")"))))

(defmethod ast->str :type-decl [ast ilevel]
  (let [qualifiers (if (:qualifiers ast) (str (s/join " " (:qualifiers ast)) " ") "")]
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

(def prog1
  {:node-type :root,
   :defns
   [{:node-type :decl,
     :id "time",
     :type
     {:qualifiers ["UNIFORM"],
      :node-type :type-decl,
      :type {:node-type :type, :name "float"}}}
    {:node-type :decl,
     :id "mouse",
     :type
     {:qualifiers ["UNIFORM"],
      :node-type :type-decl,
      :type {:node-type :type, :name "vec2"}}}
    {:node-type :decl,
     :id "resolution",
     :type
     {:qualifiers ["UNIFORM"],
      :node-type :type-decl,
      :type {:node-type :type, :name "vec2"}}}
    {:node-type :fun-def,
     :id "blob",
     :type
     {:node-type :type-decl, :type {:node-type :type, :name "float"}},
     :params
     [{:node-type :param-decl,
       :id "c",
       :type
       {:node-type :type-decl, :type {:node-type :type, :name "vec2"}}}
      {:node-type :param-decl,
       :id "r1",
       :type
       {:node-type :type-decl, :type {:node-type :type, :name "float"}}}
      {:node-type :param-decl,
       :id "r2",
       :type
       {:node-type :type-decl,
        :type {:node-type :type, :name "float"}}}],
     :body {:node-type :compound-stmt, :stmts ["jump_statement"]}}
    {:node-type :fun-def,
     :id "main",
     :type
     {:node-type :type-decl, :type {:node-type :type, :name "void"}},
     :params
     [{:node-type :param-decl,
       :type
       {:node-type :type-decl, :type {:node-type :type, :name "void"}}}],
     :body
     {:node-type :compound-stmt,
      :stmts
      [{:node-type :decl,
        :id "b",
        :type
        {:node-type :type-decl, :type {:node-type :type, :name "float"}},
        :initialzed-to "0.0"}
       {:node-type :for-loop,
        :initializer "DO SOMETHING WITH S-OR-D",
        :condition
        {:node-type :expression-list,
         :expressions
         [{:node-type :bin-op,
           :oper :<,
           :left {:node-type :identifier, :id "n"},
           :right "14"}]},
        :body
        {:node-type :compound-stmt,
         :stmts
         [{:node-type :expression-stmt,
           :expr
           {:node-type :expression-list,
            :expressions
            [{:node-type :assignment,
              :oper :+=,
              :to {:node-type :identifier, :id "b"},
              :value
              {:node-type :fun-call,
               :func
               {:node-type :type-decl,
                :type {:node-type :type, :name "blob"}},
               :args
               [{:node-type :bin-op,
                 :oper :+,
                 :left
                 {:node-type :bin-op,
                  :oper :*,
                  :left
                  {:node-type :bin-op,
                   :oper :*,
                   :left
                   {:node-type :fun-call,
                    :func
                    {:node-type :type-decl,
                     :type {:node-type :type, :name "vec2"}},
                    :args
                    [{:node-type :fun-call,
                      :func
                      {:node-type :type-decl,
                       :type {:node-type :type, :name "sin"}},
                      :args
                      [{:node-type :bin-op,
                        :oper :*,
                        :left
                        {:node-type :bin-op,
                         :oper :*,
                         :left
                         {:node-type :expression-list,
                          :expressions
                          [{:node-type :bin-op,
                            :oper :+,
                            :left "150.",
                            :right
                            {:node-type :identifier, :id "time"}}]},
                         :right
                         {:node-type :fun-call,
                          :func
                          {:node-type :type-decl,
                           :type {:node-type :type, :name "float"}},
                          :args [{:node-type :identifier, :id "n"}]}},
                        :right "0.009"}]}
                     {:node-type :fun-call,
                      :func
                      {:node-type :type-decl,
                       :type {:node-type :type, :name "cos"}},
                      :args
                      [{:node-type :bin-op,
                        :oper :*,
                        :left
                        {:node-type :bin-op,
                         :oper :*,
                         :left
                         {:node-type :expression-list,
                          :expressions
                          [{:node-type :bin-op,
                            :oper :+,
                            :left "150.",
                            :right
                            {:node-type :identifier, :id "time"}}]},
                         :right
                         {:node-type :fun-call,
                          :func
                          {:node-type :type-decl,
                           :type {:node-type :type, :name "float"}},
                          :args
                          [{:node-type :bin-op,
                            :oper :-,
                            :left "14",
                            :right {:node-type :identifier, :id "n"}}]}},
                        :right "0.025"}]}]},
                   :right {:node-type :identifier, :id "resolution"}},
                  :right "0.4"},
                 :right
                 {:node-type :bin-op,
                  :oper :div,
                  :left {:node-type :identifier, :id "resolution"},
                  :right "2.0"}}
                "10."
                "200."]}}]}}]}}
       {:node-type :expression-stmt,
        :expr
        {:node-type :expression-list,
         :expressions
         [{:node-type :assignment,
           :oper :-=,
           :to {:node-type :identifier, :id "b"},
           :value
           {:node-type :bin-op,
            :oper :*,
            :left "1.0",
            :right
            {:node-type :fun-call,
             :func
             {:node-type :type-decl,
              :type {:node-type :type, :name "blob"}},
             :args
             [{:node-type :bin-op,
               :oper :*,
               :left {:node-type :identifier, :id "mouse"},
               :right {:node-type :identifier, :id "resolution"}}
              "10."
              "150."]}}}]}}
       {:node-type :decl,
        :id "color",
        :type
        {:node-type :type-decl, :type {:node-type :type, :name "vec3"}},
        :initialzed-to
        {:node-type :bin-op,
         :oper :*,
         :left
         {:node-type :fun-call,
          :func
          {:node-type :type-decl,
           :type {:node-type :type, :name "step"}},
          :args ["0.5" {:node-type :identifier, :id "b"}]},
         :right
         {:node-type :fun-call,
          :func
          {:node-type :type-decl,
           :type {:node-type :type, :name "vec3"}},
          :args
          [{:node-type :bin-op,
            :oper :*,
            :left
            {:node-type :bin-op,
             :oper :*,
             :left
             {:node-type :expression-list,
              :expressions
              [{:node-type :bin-op,
                :oper :+,
                :left
                {:node-type :fun-call,
                 :func
                 {:node-type :type-decl,
                  :type {:node-type :type, :name "cos"}},
                 :args
                 [{:node-type :bin-op,
                   :oper :*,
                   :left "0.3",
                   :right {:node-type :identifier, :id "time"}}]},
                :right "1."}]},
             :right "0.25"},
            :right
            {:node-type :expression-list,
             :expressions
             [{:node-type :bin-op,
               :oper :+,
               :left
               {:node-type :fun-call,
                :func
                {:node-type :type-decl,
                 :type {:node-type :type, :name "step"}},
                :args
                ["0.4"
                 {:node-type :bin-op,
                  :oper :-,
                  :left "1.",
                  :right
                  {:node-type :fun-call,
                   :func
                   {:node-type :type-decl,
                    :type {:node-type :type, :name "cos"}},
                   :args
                   [{:node-type :bin-op,
                     :oper :*,
                     :left {:node-type :identifier, :id "b"},
                     :right "0.5"}]}}]},
               :right
               {:node-type :fun-call,
                :func
                {:node-type :type-decl,
                 :type {:node-type :type, :name "step"}},
                :args
                ["0.6"
                 {:node-type :fun-call,
                  :func
                  {:node-type :type-decl,
                   :type {:node-type :type, :name "sin"}},
                  :args
                  [{:node-type :bin-op,
                    :oper :*,
                    :left {:node-type :identifier, :id "b"},
                    :right "15."}]}]}}]}}
           {:node-type :bin-op,
            :oper :*,
            :left
            {:node-type :bin-op,
             :oper :*,
             :left
             {:node-type :expression-list,
              :expressions
              [{:node-type :bin-op,
                :oper :+,
                :left
                {:node-type :fun-call,
                 :func
                 {:node-type :type-decl,
                  :type {:node-type :type, :name "sin"}},
                 :args
                 [{:node-type :bin-op,
                   :oper :*,
                   :left "0.3",
                   :right {:node-type :identifier, :id "time"}}]},
                :right "1."}]},
             :right "0.25"},
            :right
            {:node-type :expression-list,
             :expressions
             [{:node-type :bin-op,
               :oper :+,
               :left
               {:node-type :fun-call,
                :func
                {:node-type :type-decl,
                 :type {:node-type :type, :name "step"}},
                :args
                ["0.1"
                 {:node-type :fun-call,
                  :func
                  {:node-type :type-decl,
                   :type {:node-type :type, :name "sin"}},
                  :args
                  [{:node-type :bin-op,
                    :oper :*,
                    :left {:node-type :identifier, :id "b"},
                    :right "9."}]}]},
               :right
               {:node-type :fun-call,
                :func
                {:node-type :type-decl,
                 :type {:node-type :type, :name "step"}},
                :args
                ["0.3"
                 {:node-type :fun-call,
                  :func
                  {:node-type :type-decl,
                   :type {:node-type :type, :name "cos"}},
                  :args
                  [{:node-type :bin-op,
                    :oper :*,
                    :left {:node-type :identifier, :id "b"},
                    :right "9."}]}]}}]}}
           {:node-type :bin-op,
            :oper :*,
            :left
            {:node-type :bin-op,
             :oper :*,
             :left
             {:node-type :expression-list,
              :expressions
              [{:node-type :bin-op,
                :oper :+,
                :left
                {:node-type :fun-call,
                 :func
                 {:node-type :type-decl,
                  :type {:node-type :type, :name "sin"}},
                 :args
                 [{:node-type :bin-op,
                   :oper :+,
                   :left
                   {:node-type :bin-op,
                    :oper :*,
                    :left "0.3",
                    :right {:node-type :identifier, :id "time"}},
                   :right "2."}]},
                :right "1."}]},
             :right "0.5"},
            :right
            {:node-type :expression-list,
             :expressions
             [{:node-type :fun-call,
               :func
               {:node-type :type-decl,
                :type {:node-type :type, :name "step"}},
               :args
               ["0.1"
                {:node-type :fun-call,
                 :func
                 {:node-type :type-decl,
                  :type {:node-type :type, :name "sin"}},
                 :args
                 [{:node-type :bin-op,
                   :oper :*,
                   :left {:node-type :identifier, :id "b"},
                   :right "5."}]}]}]}}]}}}
       {:node-type :expression-stmt,
        :expr
        {:node-type :expression-list,
         :expressions
         [{:node-type :assignment,
           :oper :+=,
           :to {:node-type :identifier, :id "color"},
           :value
           {:node-type :fun-call,
            :func
            {:node-type :type-decl,
             :type {:node-type :type, :name "vec3"}},
            :args
            [{:node-type :bin-op,
              :oper :-,
              :left
              {:node-type :fun-call,
               :func
               {:node-type :type-decl,
                :type {:node-type :type, :name "step"}},
               :args ["0.5" {:node-type :identifier, :id "b"}]},
              :right
              {:node-type :fun-call,
               :func
               {:node-type :type-decl,
                :type {:node-type :type, :name "step"}},
               :args ["0.52" {:node-type :identifier, :id "b"}]}}]}}]}}
       {:node-type :expression-stmt,
        :expr
        {:node-type :expression-list,
         :expressions
         [{:node-type :assignment,
           :oper :=,
           :to {:node-type :identifier, :id "gl_FragColor"},
           :value
           {:node-type :fun-call,
            :func
            {:node-type :type-decl,
             :type {:node-type :type, :name "vec4"}},
            :args [{:node-type :identifier, :id "color"} "1.0"]}}]}}]}}]})

(print (ast->str prog1 0))

