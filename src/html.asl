(module asl-vdom/html
  :d "Declarative HTML DSL and compact UI constructors for ASL"
  :x [Props make-props props-to-attrs
      attrs-empty attr attr-class attr-id attr-type attr-value attr-name attr-placeholder attr-href attr-src attr-alt
      attrs-of with-attr with-class
      div span p h1 h2 h3 button input form card header footer
      section nav main article aside table label
      d s sp b btn sec hdr ftr frm inp art tbl lbl txt t el c comp
      div-plain span-plain p-plain h1-plain h2-plain h3-plain button-plain form-plain card-plain header-plain footer-plain
      section-plain nav-plain main-plain article-plain table-plain label-plain
      d-plain s-plain sp-plain b-plain btn-plain sec-plain hdr-plain ftr-plain frm-plain art-plain tbl-plain lbl-plain el-plain c-plain comp-plain]
  :i [(vdom :a v)])

(dfs Props
  (:f id String "Element DOM id")
  (:f class-name String "CSS class string")
  (:f role String "Accessibility role")
  (:f title String "Element title")
  (:f extra (Map String String) "Extra arbitrary attributes"))

(df make-props [(class-name String)] -> Props
  :d "Creates a Props record with given class-name and defaults"
  (Props :id "" :class-name class-name :role "" :title "" :extra (map-empty)))

(df props-to-attrs [(p Props)] -> (Map String String)
  :d "Converts a Props record into an attribute map"
  (let [(m0 (.-extra p))
        (m1 (if (string-empty? (.-id p)) m0 (map-set m0 "id" (.-id p))))
        (m2 (if (string-empty? (.-class-name p)) m1 (map-set m1 "class" (.-class-name p))))
        (m3 (if (string-empty? (.-role p)) m2 (map-set m2 "role" (.-role p))))
        (m4 (if (string-empty? (.-title p)) m3 (map-set m3 "title" (.-title p))))]
    m4))

(df attrs-empty [] -> (Map String String)
  :d "Returns an empty attribute map"
  (map-empty))

(df attr [(k String) (v String)] -> (Pair String String)
  :d "Creates an attribute key-value pair"
  (pair k v))

(df attr-class [(v String)] -> (Pair String String)
  :d "Creates a class attribute pair"
  (pair "class" v))

(df attr-id [(v String)] -> (Pair String String)
  :d "Creates an id attribute pair"
  (pair "id" v))

(df attr-type [(v String)] -> (Pair String String)
  :d "Creates a type attribute pair"
  (pair "type" v))

(df attr-value [(v String)] -> (Pair String String)
  :d "Creates a value attribute pair"
  (pair "value" v))

(df attr-name [(v String)] -> (Pair String String)
  :d "Creates a name attribute pair"
  (pair "name" v))

(df attr-placeholder [(v String)] -> (Pair String String)
  :d "Creates a placeholder attribute pair"
  (pair "placeholder" v))

(df attr-href [(v String)] -> (Pair String String)
  :d "Creates an href attribute pair"
  (pair "href" v))

(df attr-src [(v String)] -> (Pair String String)
  :d "Creates a src attribute pair"
  (pair "src" v))

(df attr-alt [(v String)] -> (Pair String String)
  :d "Creates an alt attribute pair"
  (pair "alt" v))

(df attrs-of [(pairs (List (Pair String String)))] -> (Map String String)
  :d "Constructs an attribute map from a list of key-value pairs"
  (fold (fn [(acc (Map String String)) (p (Pair String String))] -> (Map String String)
          (map-set acc (.-first p) (.-second p)))
        (map-empty)
        pairs))

(df with-attr [(attrs (Map String String)) (k String) (v String)] -> (Map String String)
  :d "Adds or replaces an attribute key-value pair in a map"
  (map-set attrs k v))

(df with-class [(attrs (Map String String)) (cls String)] -> (Map String String)
  :d "Adds or sets class attribute in a map"
  (map-set attrs "class" cls))

(df t [(content String)] -> v/VNode
  :d "Ultra-compact text node constructor"
  (v/t content))

(df txt [(content String)] -> v/VNode
  :d "Compact alias for vdom text node"
  (v/text content))

(df div [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a div VNode"
  (v/elem "div" attrs children))

(df span [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a span VNode"
  (v/elem "span" attrs children))

(df p [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a p VNode"
  (v/elem "p" attrs children))

(df h1 [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs an h1 VNode"
  (v/elem "h1" attrs children))

(df h2 [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs an h2 VNode"
  (v/elem "h2" attrs children))

(df h3 [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs an h3 VNode"
  (v/elem "h3" attrs children))

(df button [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a button VNode"
  (v/elem "button" attrs children))

(df input [(attrs (Map String String))] -> v/VNode
  :d "Constructs a void input VNode"
  (v/elem "input" attrs (list)))

(df form [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a form VNode"
  (v/elem "form" attrs children))

(df card [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a card container VNode"
  (v/elem "div" (map-set attrs "class" "card") children))

(df header [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a header VNode"
  (v/elem "header" attrs children))

(df footer [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a footer VNode"
  (v/elem "footer" attrs children))

(df section [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a section VNode"
  (v/elem "section" attrs children))

(df sec [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for section"
  (v/elem "section" attrs children))

(df nav [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a nav VNode"
  (v/elem "nav" attrs children))

(df main [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a main VNode"
  (v/elem "main" attrs children))

(df article [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs an article VNode"
  (v/elem "article" attrs children))

(df art [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for article"
  (v/elem "article" attrs children))

(df aside [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs an aside VNode"
  (v/elem "aside" attrs children))

(df table [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a table VNode"
  (v/elem "table" attrs children))

(df tbl [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for table"
  (v/elem "table" attrs children))

(df label [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Constructs a label VNode"
  (v/elem "label" attrs children))

(df lbl [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for label"
  (v/elem "label" attrs children))

(df hdr [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for header"
  (v/elem "header" attrs children))

(df ftr [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for footer"
  (v/elem "footer" attrs children))

(df frm [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for form"
  (v/elem "form" attrs children))

(df inp [(attrs (Map String String))] -> v/VNode
  :d "Compact alias for input"
  (v/elem "input" attrs (list)))

(df d [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for div"
  (v/elem "div" attrs children))

(df s [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for span"
  (v/elem "span" attrs children))

(df sp [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for span"
  (v/elem "span" attrs children))

(df b [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for button"
  (v/elem "button" attrs children))

(df btn [(attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact alias for button"
  (v/elem "button" attrs children))

(df el [(tag String) (attrs (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact element constructor"
  (v/el tag attrs children))

(df c [(name String) (props (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Compact component constructor"
  (v/c name props children))

(df comp [(name String) (props (Map String String)) (children (List v/VNode))] -> v/VNode
  :d "Component constructor with props and children"
  (v/comp name props children))

(df div-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a div VNode with empty attributes"
  (v/elem "div" (map-empty) children))

(df span-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a span VNode with empty attributes"
  (v/elem "span" (map-empty) children))

(df p-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a p VNode with empty attributes"
  (v/elem "p" (map-empty) children))

(df h1-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs an h1 VNode with empty attributes"
  (v/elem "h1" (map-empty) children))

(df h2-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs an h2 VNode with empty attributes"
  (v/elem "h2" (map-empty) children))

(df h3-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs an h3 VNode with empty attributes"
  (v/elem "h3" (map-empty) children))

(df button-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a button VNode with empty attributes"
  (v/elem "button" (map-empty) children))

(df form-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a form VNode with empty attributes"
  (v/elem "form" (map-empty) children))

(df card-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a card container VNode with empty attributes"
  (v/elem "div" (map-set (map-empty) "class" "card") children))

(df header-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a header VNode with empty attributes"
  (v/elem "header" (map-empty) children))

(df footer-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a footer VNode with empty attributes"
  (v/elem "footer" (map-empty) children))

(df d-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for div-plain"
  (v/elem "div" (map-empty) children))

(df s-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for span-plain"
  (v/elem "span" (map-empty) children))

(df b-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for button-plain"
  (v/elem "button" (map-empty) children))

(df btn-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for button-plain"
  (v/elem "button" (map-empty) children))

(df el-plain [(tag String) (children (List v/VNode))] -> v/VNode
  :d "Compact element constructor without attributes"
  (v/el-plain tag children))

(df c-plain [(name String) (children (List v/VNode))] -> v/VNode
  :d "Compact component constructor without props"
  (v/c-plain name children))

(df comp-plain [(name String) (children (List v/VNode))] -> v/VNode
  :d "Component constructor without props"
  (v/comp-plain name children))

(df section-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a section VNode with empty attributes"
  (v/elem "section" (map-empty) children))

(df sec-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for section-plain"
  (v/elem "section" (map-empty) children))

(df sp-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for span-plain"
  (v/elem "span" (map-empty) children))

(df nav-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a nav VNode with empty attributes"
  (v/elem "nav" (map-empty) children))

(df main-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a main VNode with empty attributes"
  (v/elem "main" (map-empty) children))

(df article-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs an article VNode with empty attributes"
  (v/elem "article" (map-empty) children))

(df art-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for article-plain"
  (v/elem "article" (map-empty) children))

(df table-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a table VNode with empty attributes"
  (v/elem "table" (map-empty) children))

(df tbl-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for table-plain"
  (v/elem "table" (map-empty) children))

(df label-plain [(children (List v/VNode))] -> v/VNode
  :d "Constructs a label VNode with empty attributes"
  (v/elem "label" (map-empty) children))

(df lbl-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for label-plain"
  (v/elem "label" (map-empty) children))

(df hdr-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for header-plain"
  (v/elem "header" (map-empty) children))

(df ftr-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for footer-plain"
  (v/elem "footer" (map-empty) children))

(df frm-plain [(children (List v/VNode))] -> v/VNode
  :d "Compact alias for form-plain"
  (v/elem "form" (map-empty) children))
