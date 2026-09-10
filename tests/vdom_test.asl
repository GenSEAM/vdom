(module asl-vdom/test
  :d "Unit tests for S-expression Virtual DOM and Dual Perception in ASL"
  :x [run-tests
      test-vnode-creation
      test-axnode-construction
      test-downsampling-prunes-scripts-and-styles
      test-downsampling-filter-attributes
      test-downsampling-collapses-wrappers
      test-dom-diff-added-removed-mutated
      test-compact-nodes
      test-component-jsx-emission
      test-swiftui-and-compose-emission]
  :i [(vdom :a v) (html :a h) (emit-jsx :a j) (polyglot :a poly)])

(df test-vnode-creation [] -> Bool
  :d "Verifies VNode creation, validation, and content accessors"
  (let [(t (v/text "Hello World"))
        (empty-t (v/text ""))
        (attrs (map-set (map-empty) "id" "submit-btn"))
        (btn (v/elem "button" attrs (list t)))]
    (assert (v/is-valid-node t) "Text node must be valid")
    (assert (not (v/is-valid-node empty-t)) "Empty text node must be invalid")
    (assert (v/is-valid-node btn) "Button element node must be valid")
    (assert (= (v/vnode-tag btn) "button") "Button tag must be button")
    (assert (= (v/vnode-text t) "Hello World") "Text node content must match")
    (assert (= (list-length (v/vnode-children btn)) 1) "Button children count must be 1")
    true))

(df test-axnode-construction [] -> Bool
  :d "Verifies AXNode accessibility tree node construction and state tracking"
  (let [(leaf (v/ax-leaf "button" "Deploy" "e1"))
        (parent (v/make-ax-node "dialog" "Confirmation" "e0" "Modal dialog" false true (list leaf)))]
    (assert (= (.-role leaf) "button") "Leaf role must be button")
    (assert (= (.-name leaf) "Deploy") "Leaf name must be Deploy")
    (assert (= (.-ref leaf) "e1") "Leaf ref must be e1")
    (assert (not (.-disabled leaf)) "Leaf must not be disabled")
    (assert (not (.-focused leaf)) "Leaf must not be focused")
    (assert (= (.-role parent) "dialog") "Parent role must be dialog")
    (assert (.-focused parent) "Parent must be focused")
    (assert (= (list-length (.-children parent)) 1) "Parent children count must be 1")
    true))

(df test-downsampling-prunes-scripts-and-styles [] -> Bool
  :d "Verifies downsampler eliminates scripts, styles, and non-semantic tags"
  (let [(script-node (v/elem-plain "script" (list (v/text "console.log(1)"))))
        (style-node (v/elem-plain "style" (list (v/text "body { margin: 0; }"))))
        (meta-node (v/elem-plain "meta" (list)))
        (content-node (v/elem-plain "h1" (list (v/text "Title"))))
        (container (v/elem-plain "main" (list script-node style-node meta-node content-node)))
        (downsampled-opt (v/downsample-node container))]
    (assert (mt downsampled-opt
              ((some clean-node)
               (let [(children (v/vnode-children clean-node))]
                 (and (= (v/vnode-tag clean-node) "main")
                      (and (= (list-length children) 1)
                           (= (v/vnode-tag (mt (list-head children) ((some h) h) ((none) (v/text "")))) "h1")))))
              ((none) false)) "Downsampler must prune script, style, meta leaving main > h1")
    (assert (v/should-prune-tag? "script") "Script tag must be marked for pruning")
    (assert (v/should-prune-tag? "style") "Style tag must be marked for pruning")
    (assert (not (v/should-prune-tag? "main")) "Semantic main tag must not be pruned")
    (assert (not (v/should-prune-tag? "h1")) "Semantic h1 tag must not be pruned")
    (assert (not (v/is-valid-node (v/elem "" (map-empty) (list)))) "Malformed tag with empty string must be invalid")
    (assert (not (v/is-valid-node (v/text ""))) "Empty text node must be invalid")
    (assert (not (v/is-retained-attr? "onclick")) "Non-semantic onclick attribute must not be retained")
    (assert (list-empty? (v/downsample-children (list script-node style-node meta-node))) "Pruned tags must produce empty node list")
    (assert (not (list-empty? (v/downsample-children (list content-node)))) "Content node list must not be empty after downsampling")
    true))

(df test-downsampling-filter-attributes [] -> Bool
  :d "Verifies downsampler strips CSS styling noise while retaining semantic attributes"
  (assert (v/is-retained-attr? "id") "id attribute must be retained")
  (assert (v/is-retained-attr? "aria-label") "aria-label attribute must be retained")
  (assert (v/is-retained-attr? "role") "role attribute must be retained")
  (assert (not (v/is-retained-attr? "class")) "class attribute must not be retained")
  (assert (not (v/is-retained-attr? "style")) "style attribute must not be retained")
  (assert (not (v/is-retained-attr? "data-reactroot")) "data-reactroot attribute must not be retained")
  true)

(df test-downsampling-collapses-wrappers [] -> Bool
  :d "Verifies transparent single-child wrappers are collapsed to the inner semantic element"
  (let [(target-btn (v/elem "button" (map-set (map-empty) "id" "act") (list (v/text "Action"))))
        (wrapper-span (v/elem-plain "span" (list target-btn)))
        (wrapper-div (v/elem-plain "div" (list wrapper-span)))
        (collapsed-opt (v/downsample-node wrapper-div))]
    (assert (mt collapsed-opt
              ((some final-node)
               (and (= (v/vnode-tag final-node) "button")
                    (= (v/vnode-text (mt (list-head (v/vnode-children final-node)) ((some h) h) ((none) (v/text "")))) "Action")))
              ((none) false)) "Downsampler must collapse wrappers to button")
    (assert (v/is-redundant-wrapper? "div" (map-empty) 1) "Plain single-child div must be classified as redundant wrapper")
    (assert (v/is-redundant-wrapper? "span" (map-empty) 1) "Plain single-child span must be classified as redundant wrapper")
    (assert (not (v/is-redundant-wrapper? "button" (map-empty) 1)) "Semantic button element must not be classified as redundant wrapper")
    (assert (not (v/is-redundant-wrapper? "div" (map-set (map-empty) "id" "act") 1)) "Wrapper with attributes must not be collapsed")
    (assert (not (v/is-redundant-wrapper? "div" (map-empty) 2)) "Wrapper with multiple children must not be collapsed")
    (assert (not (v/is-redundant-wrapper? "div" (map-empty) 0)) "Empty node wrapper must not be collapsed")
    true))

(df test-dom-diff-added-removed-mutated [] -> Bool
  :d "Verifies incremental DOM diffing correctly produces added, removed, and mutated records"
  (let [(extra-node (v/elem-plain "span" (list (v/text "New Badge"))))
        (mut-rec (v/mutated "root/0" "text" "Saved"))
        (diff (v/partition-diff "/settings" (list (v/added "root/1" extra-node) mut-rec)))
        (frame (v/format-diff-frame diff))]
    (assert (= (.-route diff) "/settings") "Diff route must match /settings")
    (assert (= (list-length (.-added diff)) 1) "Diff added length must be 1")
    (assert (> (list-length (.-mutated diff)) 0) "Diff mutated count must exceed 0")
    (assert (string-contains? frame "(! dom/diff :route \"/settings\"") "Diff frame must contain route")
    (assert (string-contains? frame ":added-count 1") "Diff frame must contain added-count 1")
    (assert (string-contains? frame ":mutated-count 1") "Diff frame must contain mutated-count 1")
    true))

(df test-compact-nodes [] -> Bool
  :d "Verifies ultra-compact t, el, btn, c, comp, sec, sp, hdr constructors and predicates"
  (let [(txt-node (v/t "AgentScript"))
        (btn-node (h/btn (h/attrs-of (list (h/attr-id "b1"))) (list txt-node)))
        (sec-node (h/sec-plain (list btn-node)))
        (sp-node (h/sp-plain (list txt-node)))
        (hdr-node (h/hdr-plain (list sp-node)))
        (comp-node (h/c "Card" (h/attrs-of (list (h/attr-class "p-4"))) (list hdr-node)))
        (comp-plain (h/c-plain "Hero" (list txt-node)))]
    (assert (v/is-valid-node txt-node) "txt-node must be valid")
    (assert (= (v/vnode-text txt-node) "AgentScript") "txt-node text must be AgentScript")
    (assert (= (v/vnode-tag btn-node) "button") "btn-node tag must be button")
    (assert (= (v/vnode-tag sec-node) "section") "sec-node tag must be section")
    (assert (= (v/vnode-tag sp-node) "span") "sp-node tag must be span")
    (assert (= (v/vnode-tag hdr-node) "header") "hdr-node tag must be header")
    (assert (not (v/vnode-is-comp? btn-node)) "btn-node must not be component")
    (assert (v/vnode-is-comp? comp-node) "comp-node must be component")
    (assert (v/vnode-is-comp? comp-plain) "comp-plain must be component")
    (assert (= (v/vnode-tag comp-node) "Card") "comp-node tag must be Card")
    (assert (= (list-length (v/vnode-children comp-node)) 1) "comp-node children length must be 1")
    true))

(df test-component-jsx-emission [] -> Bool
  :d "Verifies JSX/TSX emission for components, native void elements, and event handlers"
  (let [(empty-card (v/comp-plain "Card" (list)))
        (jsx-card (j/emit-vnode-jsx empty-card 0))
        (btn-node (v/elem "button" (map-set (map-set (map-empty) "class" "btn") "disabled" "true") (list (v/t "Submit"))))
        (jsx-btn (j/emit-vnode-jsx btn-node 0))]
    (assert (string-contains? jsx-card "<Card") "Card JSX must contain Card")
    (assert (string-contains? jsx-card "/>") "Card JSX must self-close")
    (assert (string-contains? jsx-btn "<button") "Button JSX must open button tag")
    (assert (string-contains? jsx-btn "</button>") "Button JSX must close button tag")
    (assert (string-contains? jsx-btn "Submit") "Button JSX must contain Submit")
    true))

(df test-swiftui-and-compose-emission [] -> Bool
  :d "Verifies SwiftUI and Jetpack Compose emission from VNode AST"
  (let [(btn-text (v/text "Submit"))
        (btn (v/elem-plain "button" (list btn-text)))
        (title (v/elem-plain "text" (list (v/text "Welcome"))))
        (ios-item (v/elem-plain "platform-ios" (list (v/elem-plain "text" (list (v/text "iOS Only"))))))
        (android-item (v/elem-plain "platform-android" (list (v/elem-plain "text" (list (v/text "Android Only"))))))
        (stack (v/elem-plain "vstack" (list title btn ios-item android-item)))
        (swift-code (poly/emit-swiftui-view "Login" stack))
        (compose-code (poly/emit-compose-composable "Login" stack))]
    (assert (string-contains? swift-code "struct LoginView: View") "SwiftUI struct must match")
    (assert (string-contains? swift-code "VStack(spacing: 8)") "SwiftUI must emit VStack")
    (assert (string-contains? swift-code "Button(action: {})") "SwiftUI must emit Button")
    (assert (string-contains? swift-code "iOS Only") "SwiftUI must render iOS platform block")
    (assert (not (string-contains? swift-code "Android Only")) "SwiftUI must omit Android platform block")
    (assert (string-contains? compose-code "@Composable") "Compose must declare @Composable")
    (assert (string-contains? compose-code "fun Login()") "Compose function must match")
    (assert (string-contains? compose-code "Column {") "Compose must emit Column")
    (assert (string-contains? compose-code "Button(onClick = {})") "Compose must emit Button")
    (assert (string-contains? compose-code "Android Only") "Compose must render Android platform block")
    (assert (not (string-contains? compose-code "iOS Only")) "Compose must omit iOS platform block")
    true))

(df run-tests [] -> Bool
  :d "Runs all VDOM and dual perception unit tests"
  (and (test-vnode-creation)
       (test-axnode-construction)
       (test-downsampling-prunes-scripts-and-styles)
       (test-downsampling-filter-attributes)
       (test-downsampling-collapses-wrappers)
       (test-dom-diff-added-removed-mutated)
       (test-compact-nodes)
       (test-component-jsx-emission)
       (test-swiftui-and-compose-emission)))

(run-tests)

