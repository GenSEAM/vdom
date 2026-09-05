(module asl-vdom/polyglot
  :d "Universal Polyglot UI Transpiler: React 19 TSX, Vue 3 SFC, Svelte 5, and SSR HTML"
  :x [emit-react-tsx
      emit-vue-sfc
      emit-svelte-component
      emit-ssr-html
      compile-component
      vue-attr-key
      svelte-attr-key
      html-attr-key
      is-self-closing?
      indent-spaces]
  :i [(vdom :a v)])

(df indent-spaces [(level Int64)] -> String
  :d "Generates indentation string of 2 spaces per level"
  (if (<= level 0)
      ""
      (str "  " (indent-spaces (- level 1)))))

(df is-self-closing? [(tag String)] -> Bool
  :d "Checks if an HTML tag is a self-closing void element"
  (or (= tag "input")
      (or (= tag "img")
          (or (= tag "br")
              (or (= tag "hr")
                  (or (= tag "meta")
                      (= tag "link")))))))

(df vue-attr-key [(k String)] -> String
  :d "Maps attribute key to Vue 3 template directive or attribute"
  (cond
    ((= k "onclick") "@click")
    ((= k "oninput") "@input")
    ((= k "onchange") "@change")
    ((= k "onsubmit") "@submit.prevent")
    (:else k)))

(df svelte-attr-key [(k String)] -> String
  :d "Maps attribute key to Svelte 5 event or attribute"
  (cond
    ((= k "onclick") "onclick")
    ((= k "oninput") "oninput")
    ((= k "onchange") "onchange")
    (:else k)))

(df html-attr-key [(k String)] -> String
  :d "Standard HTML attribute key"
  k)

(df emit-attrs [(attrs (Map String String)) (target String)] -> String
  :d "Renders attribute map according to target framework conventions"
  (let [(pairs (map-pairs attrs))]
    (if (= (list-length pairs) 0)
        ""
        (let [(rendered (map (fn [(p (Pair String String))] -> String
                               (let [(raw-k (.-first p))
                                     (v (.-second p))
                                     (k (cond
                                          ((= target "vue") (vue-attr-key raw-k))
                                          ((= target "svelte") (svelte-attr-key raw-k))
                                          ((= target "react") (if (= raw-k "class") "className" raw-k))
                                          (:else (html-attr-key raw-k))))]
                                 (str " " k "=\"" v "\"")))
                             pairs))]
          (string-join rendered "")))))

(df emit-vnode [(target String) (node v/VNode) (indent Int64)] -> String
  :d "Recursively renders a VNode to the target markup syntax"
  (let [(pad (indent-spaces indent))]
    (mt node
      ((v/text-node content)
       (str pad content))
      ((v/element-node tag attrs children)
       (let [(attr-str (emit-attrs attrs target))
             (ch-len (list-length children))]
         (if (= ch-len 0)
             (if (is-self-closing? tag)
                 (str pad "<" tag attr-str " />")
                 (str pad "<" tag attr-str "></" tag ">"))
             (let [(ch-strs (map (fn [(ch v/VNode)] -> String
                                   (emit-vnode target ch (+ indent 1)))
                                 children))
                   (ch-joined (string-join ch-strs "\n"))]
               (str pad "<" tag attr-str ">\n" ch-joined "\n" pad "</" tag ">"))))))))

(df emit-react-tsx [(name String) (props-type String) (node v/VNode)] -> String
  :d "Generates React 19 functional component TSX"
  (let [(jsx (emit-vnode "react" node 2))]
    (str "// React 19 TSX Component\n"
         "import React from \"react\";\n\n"
         "export interface " props-type " {\n"
         "  className?: string;\n"
         "  children?: React.ReactNode;\n"
         "}\n\n"
         "export const " name " = (props: " props-type ") => {\n"
         "  return (\n"
         jsx "\n"
         "  );\n"
         "};\n"
         "export default " name ";\n")))

(df emit-vue-sfc [(name String) (props-type String) (node v/VNode)] -> String
  :d "Generates Vue 3 Single-File Component with <script setup lang=\"ts\">"
  (let [(tmpl (emit-vnode "vue" node 1))]
    (str "<script setup lang=\"ts\">\n"
         "export interface " props-type " {\n"
         "  className?: string;\n"
         "}\n"
         "const props = withDefaults(defineProps<" props-type ">(), {\n"
         "  className: \"\",\n"
         "});\n"
         "</script>\n\n"
         "<template>\n"
         tmpl "\n"
         "</template>\n")))

(df emit-svelte-component [(name String) (props-type String) (node v/VNode)] -> String
  :d "Generates Svelte 5 component with runes ($props)"
  (let [(body (emit-vnode "svelte" node 0))]
    (str "<script lang=\"ts\">\n"
         "  let { className = \"\", children }: { className?: string; children?: any } = $props();\n"
         "</script>\n\n"
         body "\n")))

(df emit-ssr-html [(node v/VNode)] -> String
  :d "Generates pure server-side rendered HTML"
  (emit-vnode "html" node 0))

(df compile-component [(target String) (name String) (props-type String) (node v/VNode)] -> String
  :d "Dispatches compilation to the requested target (react, vue, svelte, html)"
  (cond
    ((= target "react") (emit-react-tsx name props-type node))
    ((= target "vue") (emit-vue-sfc name props-type node))
    ((= target "svelte") (emit-svelte-component name props-type node))
    ((= target "html") (emit-ssr-html node))
    (:else (emit-ssr-html node))))
