(module asl-vdom/asn-svg
  :d "Pure AgentScript ASN Vector Graphics S-Expression Transpiler to W3C SVG"
  :x [SvgNode SvgResult asn-to-svg]
  :i [])

(dfs SvgNode
  (:f kind Str "Node type: svg, rc, circ, ln, poly, p, txt, g, grad, rgrad, stop, def")
  (:f attrs (List (Pair Str Str)) "Declared key-value attributes")
  (:f children (List SvgNode) "Child nodes in tree")
  (:f text-content Str "Inner text content for text and stop nodes"))

(dfs SvgResult
  (:f svg Str "Rendered W3C SVG XML payload")
  (:f success Bool "Transpilation success status")
  (:f error-msg Str "Error description if parsing failed"))

(df make-svg-node [(kind Str) (attrs (List (Pair Str Str))) (children (List SvgNode))] -> SvgNode
  :d "Constructs an SvgNode with empty inner text"
  (SvgNode :kind kind :attrs attrs :children children :text-content ""))

(df extract-attr [(attrs (List (Pair Str Str))) (key Str) (default-val Str)] -> Str
  :d "Retrieves attribute value by key from pairs list, or returns default-val"
  (let [(matches (filter (fn [(p (Pair Str Str))] (= (fst p) key)) attrs))]
    (mt (list-head matches)
      ((some p) (snd p))
      ((none) default-val))))

(df has-attr? [(attrs (List (Pair Str Str))) (key Str)] -> Bool
  :d "Returns true if attribute key exists in pairs list"
  (not (list-empty? (filter (fn [(p (Pair Str Str))] (= (fst p) key)) attrs))))

(df render-rect [(node SvgNode)] -> Str
  :d "Renders ASN :rc rectangle to SVG <rect>"
  (let [(attrs (.-attrs node))
        (x (extract-attr attrs "x" "0"))
        (y (extract-attr attrs "y" "0"))
        (w (extract-attr attrs "w" "100"))
        (h (extract-attr attrs "h" "100"))
        (rx (extract-attr attrs "rx" "0"))
        (ry (extract-attr attrs "ry" rx))
        (f (extract-attr attrs "f" (extract-attr attrs "fill" "none")))
        (s (extract-attr attrs "s" (extract-attr attrs "stroke" "none")))
        (sw (extract-attr attrs "sw" (extract-attr attrs "stroke-width" "1")))
        (r-attrs (if (= rx "0") "" (str " rx=\"" rx "\" ry=\"" ry "\"")))]
    (str "<rect x=\"" x "\" y=\"" y "\" width=\"" w "\" height=\"" h "\" fill=\"" f "\" stroke=\"" s "\" stroke-width=\"" sw "\"" r-attrs " />")))

(df render-circle [(node SvgNode)] -> Str
  :d "Renders ASN :circ circle to SVG <circle>"
  (let [(attrs (.-attrs node))
        (cx (extract-attr attrs "cx" "160"))
        (cy (extract-attr attrs "cy" "160"))
        (r (extract-attr attrs "r" "50"))
        (f (extract-attr attrs "f" (extract-attr attrs "fill" "none")))
        (s (extract-attr attrs "s" (extract-attr attrs "stroke" "none")))
        (sw (extract-attr attrs "sw" (extract-attr attrs "stroke-width" "1")))]
    (str "<circle cx=\"" cx "\" cy=\"" cy "\" r=\"" r "\" fill=\"" f "\" stroke=\"" s "\" stroke-width=\"" sw "\" />")))

(df render-line [(node SvgNode)] -> Str
  :d "Renders ASN :ln line to SVG <line>"
  (let [(attrs (.-attrs node))
        (x1 (extract-attr attrs "x1" "0"))
        (y1 (extract-attr attrs "y1" "0"))
        (x2 (extract-attr attrs "x2" "100"))
        (y2 (extract-attr attrs "y2" "100"))
        (s (extract-attr attrs "s" (extract-attr attrs "stroke" "#38bdf8")))
        (sw (extract-attr attrs "sw" (extract-attr attrs "stroke-width" "2")))]
    (str "<line x1=\"" x1 "\" y1=\"" y1 "\" x2=\"" x2 "\" y2=\"" y2 "\" stroke=\"" s "\" stroke-width=\"" sw "\" stroke-linecap=\"round\" />")))

(df render-polygon [(node SvgNode)] -> Str
  :d "Renders ASN :poly polygon to SVG <polygon> with fallback visible stroke"
  (let [(attrs (.-attrs node))
        (pts (extract-attr attrs "pts" (extract-attr attrs "points" "")))
        (has-f (or (has-attr? attrs "f") (has-attr? attrs "fill")))
        (has-s (or (has-attr? attrs "s") (has-attr? attrs "stroke")))
        (f (if has-f (extract-attr attrs "f" (extract-attr attrs "fill" "none")) (if has-s "none" "rgba(56, 189, 248, 0.2)")))
        (s (if has-s (extract-attr attrs "s" (extract-attr attrs "stroke" "none")) (if has-f "none" "#38bdf8")))
        (sw (extract-attr attrs "sw" (extract-attr attrs "stroke-width" "1")))]
    (str "<polygon points=\"" pts "\" fill=\"" f "\" stroke=\"" s "\" stroke-width=\"" sw "\" />")))

(df render-path [(node SvgNode)] -> Str
  :d "Renders ASN :p path to SVG <path> with fallback visible stroke"
  (let [(attrs (.-attrs node))
        (d (extract-attr attrs "d" ""))
        (has-f (or (has-attr? attrs "f") (has-attr? attrs "fill")))
        (has-s (or (has-attr? attrs "s") (has-attr? attrs "stroke")))
        (f (if has-f (extract-attr attrs "f" (extract-attr attrs "fill" "none")) "none"))
        (s (if has-s (extract-attr attrs "s" (extract-attr attrs "stroke" "none")) (if has-f "none" "#38bdf8")))
        (sw (extract-attr attrs "sw" (extract-attr attrs "stroke-width" "2")))]
    (str "<path d=\"" d "\" fill=\"" f "\" stroke=\"" s "\" stroke-width=\"" sw "\" stroke-linecap=\"round\" stroke-linejoin=\"round\" />")))

(df render-text [(node SvgNode)] -> Str
  :d "Renders ASN :txt text element to SVG <text>"
  (let [(attrs (.-attrs node))
        (x (extract-attr attrs "x" "20"))
        (y (extract-attr attrs "y" "30"))
        (f (extract-attr attrs "f" "#ffffff"))
        (sz (extract-attr attrs "sz" "14"))
        (txt (extract-attr attrs "text" (extract-attr attrs "t" (.-text-content node))))]
    (str "<text x=\"" x "\" y=\"" y "\" fill=\"" f "\" font-size=\"" sz "\" font-family=\"system-ui, sans-serif\">" txt "</text>")))

(df render-svg-node [(node SvgNode)] -> Str
  :d "Recursively renders an SvgNode tree to valid W3C XML"
  (let [(kind (.-kind node))]
    (cond
      [(= kind "svg")
       (let [(attrs (.-attrs node))
             (w (extract-attr attrs "w" "320"))
             (h (extract-attr attrs "h" "320"))
             (v (extract-attr attrs "v" (str "0 0 " w " " h)))
             (children-xml (string-join "\n  " (map render-svg-node (.-children node))))]
         (str "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"" v "\" width=\"100%\" height=\"100%\" preserveAspectRatio=\"xMidYMid meet\">\n  " children-xml "\n</svg>"))]
      [(= kind "rc") (render-rect node)]
      [(= kind "circ") (render-circle node)]
      [(= kind "ln") (render-line node)]
      [(= kind "poly") (render-polygon node)]
      [(= kind "p") (render-path node)]
      [(= kind "txt") (render-text node)]
      [(= kind "g")
       (let [(attrs (.-attrs node))
             (tr (extract-attr attrs "transform" (extract-attr attrs "tr" "")))
             (tr-attr (if (string-empty? tr) "" (str " transform=\"" tr "\"")))
             (children-xml (string-join "\n    " (map render-svg-node (.-children node))))]
         (str "<g" tr-attr ">\n    " children-xml "\n  </g>"))]
      [true ""])))

(df extract-val [(src Str) (key Str)] -> (Option Str)
  :d "Extracts attribute value string after keyword in source fragment"
  (mt (string-index-of src (str key " "))
    ((none) (none))
    ((some idx)
     (let [(klen (string-length key))
           (start (+ (+ idx klen) 1))
           (tail (string-trim (option-or (string-slice src start (string-length src)) "")))]
       (if (string-starts-with? tail "\""))
         (let [(p (string-split tail "\""))
               (val (option-or (list-head (list-drop p 1)) ""))]
           (some val))
         (let [(words (string-split tail " "))
               (raw-val (option-or (list-head words) ""))
               (clean-val (string-replace (string-replace (string-replace raw-val ")" "") "\n" "") "\t" ""))]
           (if (string-empty? clean-val) (none) (some clean-val)))))))

(df collect-attrs [(src Str) (keys (List Str))] -> (List (Pair Str Str))
  :d "Collects present attribute key-value pairs from source fragment"
  (fold (fn [(acc (List (Pair Str Str))) (k Str)]
          (let [(k-clean (if (string-starts-with? k ":") (option-or (string-slice k 1 (string-length k)) k) k))]
            (mt (extract-val src k)
              ((some v) (list-append acc (list (pair k-clean v))))
              ((none) acc))))
        (list)
        keys))

(df parse-child-chunk [(chunk Str) (kind Str) (keys (List Str))] -> SvgNode
  (let [(attrs (collect-attrs chunk keys))
        (txt (mt (extract-val chunk ":text") ((some tv) tv) ((none) (mt (extract-val chunk ":t") ((some t2) t2) ((none) "")))))]
    (SvgNode :kind kind :attrs attrs :children (list) :text-content txt)))

(df extract-form-between [(src Str) (prefix Str)] -> (Option Str)
  (mt (string-index-of src prefix)
    ((none) (none))
    ((some start-idx)
     (let [(sub (option-or (string-slice src start-idx (string-length src)) ""))]
       (mt (string-index-of sub ")")
         ((none) (some sub))
         ((some end-idx) (string-slice sub 0 (+ end-idx 1))))))))

(df parse-all-children [(src Str)] -> (List SvgNode)
  :d "Dynamically extracts child SVG nodes from ASN S-expression string"
  (let [(rc-keys (list ":x" ":y" ":w" ":h" ":rx" ":ry" ":f" ":fill" ":s" ":stroke" ":sw" ":stroke-width"))
        (circ-keys (list ":cx" ":cy" ":r" ":f" ":fill" ":s" ":stroke" ":sw" ":stroke-width"))
        (ln-keys (list ":x1" ":y1" ":x2" ":y2" ":s" ":stroke" ":sw" ":stroke-width"))
        (poly-keys (list ":pts" ":points" ":f" ":fill" ":s" ":stroke" ":sw" ":stroke-width"))
        (p-keys (list ":d" ":f" ":fill" ":s" ":stroke" ":sw" ":stroke-width"))
        (txt-keys (list ":x" ":y" ":f" ":fill" ":sz" ":size" ":text" ":t"))
        (c1 (mt (extract-form-between src "(:rc ") ((some f) (list (parse-child-chunk f "rc" rc-keys))) ((none) (list))))
        (c2 (mt (extract-form-between src "(:circ ") ((some f) (list-append c1 (list (parse-child-chunk f "circ" circ-keys)))) ((none) c1)))
        (c3 (mt (extract-form-between src "(:ln ") ((some f) (list-append c2 (list (parse-child-chunk f "ln" ln-keys)))) ((none) c2)))
        (c4 (mt (extract-form-between src "(:poly ") ((some f) (list-append c3 (list (parse-child-chunk f "poly" poly-keys)))) ((none) c3)))
        (c5 (mt (extract-form-between src "(:p ") ((some f) (list-append c4 (list (parse-child-chunk f "p" p-keys)))) ((none) c4)))
        (c6 (mt (extract-form-between src "(:txt ") ((some f) (list-append c5 (list (parse-child-chunk f "txt" txt-keys)))) ((none) c5)))]
    c6))

(df asn-to-svg [(raw-asn Str)] -> SvgResult
  :d "Parses native ASN vector graphics S-expression and transpiles to SVG dynamically"
  (if (string-empty? raw-asn)
    (SvgResult :svg "" :success false :error-msg "Empty ASN input")
    (let [(clean (string-trim raw-asn))]
      (if (string-starts-with? clean "(:svg")
        (let [(w-val (mt (extract-val clean ":w") ((some w) w) ((none) "320")))
              (h-val (mt (extract-val clean ":h") ((some h) h) ((none) "320")))
              (v-val (mt (extract-val clean ":v") ((some v) v) ((none) (str "0 0 " w-val " " h-val))))
              (root-attrs (list (pair "w" w-val) (pair "h" h-val) (pair "v" v-val)))
              (children (parse-all-children clean))
              (root (SvgNode :kind "svg" :attrs root-attrs :children children :text-content ""))
              (xml (render-svg-node root))]
          (SvgResult :svg xml :success true :error-msg ""))
        (SvgResult :svg "" :success false :error-msg "Input must start with (:svg")))))
