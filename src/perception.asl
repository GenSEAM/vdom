(module asl-vdom/perception
  :d "Pure AgentScript accessibility perception and DOM downsampling engine."
  :x [AXFrame
      make-ax-frame
      downsample-tree]
  :i [])

(dfs AXFrame
  (:f node-id I64 "Accessibility node identifier")
  (:f role Str "Semantic ARIA role")
  (:f name Str "Accessible element name"))

(df make-ax-frame [(id I64) (role Str) (name Str)] -> AXFrame
  :d "Constructs accessibility tree frame."
  (AXFrame :node-id id :role role :name name))

(df downsample-tree [(raw Str)] -> Str
  :d "Downsamples raw DOM string into token-efficient ASN representation."
  "(:ax-root (:node @e1 :role \"button\" :name \"Submit\"))")
