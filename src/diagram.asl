(module asl-vdom/diagram
  :d "Pure AgentScript diagram AST, Mermaid transpiler, and browser visual HTML generator"
  :x [DiagramNode DiagramEdge DiagramSubgraph Diagram
      make-diagram-node make-diagram-edge make-diagram-subgraph make-diagram
      format-node format-edge format-subgraph
      diagram-to-mermaid mermaid-to-html diagram-to-html]
  :i [])

(dfs DiagramNode
  (:f id Str "Node identifier")
  (:f label Str "Display label or HTML description")
  (:f shape Str "Node shape: rect, rounded, stadium, circle, diamond"))

(dfs DiagramEdge
  (:f from-id Str "Source node identifier")
  (:f to-id Str "Target node identifier")
  (:f label Str "Optional edge label")
  (:f style Str "Edge style: arrow, dotted, thick"))

(dfs DiagramSubgraph
  (:f id Str "Subgraph cluster identifier")
  (:f title Str "Cluster display title")
  (:f nodes (List DiagramNode) "Nodes contained within cluster"))

(dfs Diagram
  (:f kind Str "Diagram type: flowchart, sequence, state")
  (:f direction Str "Layout direction: TD, LR, TB, RL")
  (:f subgraphs (List DiagramSubgraph) "Cluster subgraphs")
  (:f edges (List DiagramEdge) "Directed edges"))

(df make-diagram-node [(id Str) (label Str) (shape Str)] -> DiagramNode
  :d "Constructs a DiagramNode with shape specification"
  (DiagramNode :id id :label label :shape shape))

(df make-diagram-edge [(from-id Str) (to-id Str) (label Str)] -> DiagramEdge
  :d "Constructs a DiagramEdge between two node identifiers"
  (DiagramEdge :from-id from-id :to-id to-id :label label :style "arrow"))

(df make-diagram-subgraph [(id Str) (title Str) (nodes (List DiagramNode))] -> DiagramSubgraph
  :d "Constructs a DiagramSubgraph cluster"
  (DiagramSubgraph :id id :title title :nodes nodes))

(df make-diagram [(direction Str) (subgraphs (List DiagramSubgraph)) (edges (List DiagramEdge))] -> Diagram
  :d "Constructs a root Diagram specification"
  (Diagram :kind "flowchart" :direction direction :subgraphs subgraphs :edges edges))

(df format-node [(node DiagramNode)] -> Str
  :d "Formats a single DiagramNode into Mermaid syntax"
  (let [(s (.-shape node))
        (id (.-id node))
        (lbl (.-label node))]
    (if (= s "rounded")
      (str id "(\"" lbl "\")")
      (if (= s "stadium")
        (str id "([" lbl "])")
        (if (= s "circle")
          (str id "((\"" lbl "\"))")
          (if (= s "diamond")
            (str id "{\"" lbl "\"}")
            (str id "[\"" lbl "\"]")))))))

(df format-node-list-rec [(nodes (List DiagramNode)) (acc Str)] -> Str
  :d "Recursively formats a list of DiagramNode records"
  (if (list-empty? nodes)
    acc
    (let [(head (option-or (list-head nodes) (make-diagram-node "" "" "")))
          (tail (option-or (list-tail nodes) (list)))
          (line (str "        " (format-node head) "\n"))]
      (format-node-list-rec tail (str acc line)))))

(df format-subgraph [(sub DiagramSubgraph)] -> Str
  :d "Formats a DiagramSubgraph and its child nodes"
  (let [(nodes-str (format-node-list-rec (.-nodes sub) ""))]
    (str "    subgraph " (.-id sub) "[\"" (.-title sub) "\"]\n"
         nodes-str
         "    end\n")))

(df format-subgraphs-rec [(subs (List DiagramSubgraph)) (acc Str)] -> Str
  :d "Recursively formats a list of DiagramSubgraph records"
  (if (list-empty? subs)
    acc
    (let [(head (option-or (list-head subs) (make-diagram-subgraph "" "" (list))))
          (tail (option-or (list-tail subs) (list)))
          (sg-str (format-subgraph head))]
      (format-subgraphs-rec tail (str acc sg-str)))))

(df format-edge [(edge DiagramEdge)] -> Str
  :d "Formats a single DiagramEdge into Mermaid arrow syntax"
  (let [(lbl (.-label edge))]
    (if (string-empty? lbl)
      (str "    " (.-from-id edge) " --> " (.-to-id edge) "\n")
      (str "    " (.-from-id edge) " -->|\"" lbl "\"| " (.-to-id edge) "\n"))))

(df format-edges-rec [(edges (List DiagramEdge)) (acc Str)] -> Str
  :d "Recursively formats a list of DiagramEdge records"
  (if (list-empty? edges)
    acc
    (let [(head (option-or (list-head edges) (make-diagram-edge "" "" "")))
          (tail (option-or (list-tail edges) (list)))
          (e-str (format-edge head))]
      (format-edges-rec tail (str acc e-str)))))

(df diagram-to-mermaid [(diag Diagram)] -> Str
  :d "Transpiles structured Diagram into valid Mermaid flowchart syntax"
  (str "flowchart " (.-direction diag) "\n"
       (format-subgraphs-rec (.-subgraphs diag) "")
       (format-edges-rec (.-edges diag) "")))

(df mermaid-to-html [(mermaid-code Str) (page-title Str)] -> Str
  :d "Generates a responsive standalone visual HTML viewer with Mermaid.js and pan-zoom"
  (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n  <title>"
       page-title
       "</title>\n  <style>\n    :root {\n      --bg-primary: #090d16;\n      --bg-secondary: #0f172a;\n      --border-color: #1e293b;\n      --accent-color: #38bdf8;\n      --text-main: #f1f5f9;\n      --text-muted: #94a3b8;\n    }\n    * { box-sizing: border-box; margin: 0; padding: 0; }\n    body {\n      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;\n      background-color: var(--bg-primary);\n      color: var(--text-main);\n      display: flex;\n      flex-direction: column;\n      height: 100vh;\n      overflow: hidden;\n    }\n    header {\n      background: var(--bg-secondary);\n      border-bottom: 1px solid var(--border-color);\n      padding: 12px 24px;\n      display: flex;\n      align-items: center;\n      justify-content: space-between;\n      z-index: 10;\n    }\n    .title {\n      font-size: 16px;\n      font-weight: 600;\n      letter-spacing: -0.01em;\n      display: flex;\n      align-items: center;\n      gap: 10px;\n    }\n    .badge {\n      font-size: 11px;\n      text-transform: uppercase;\n      background: rgba(56, 189, 248, 0.15);\n      color: var(--accent-color);\n      padding: 2px 8px;\n      border-radius: 4px;\n      font-weight: 700;\n    }\n    .toolbar {\n      display: flex;\n      gap: 8px;\n    }\n    button {\n      background: #1e293b;\n      color: var(--text-main);\n      border: 1px solid #334155;\n      padding: 6px 14px;\n      border-radius: 6px;\n      cursor: pointer;\n      font-size: 13px;\n      font-weight: 500;\n      transition: all 0.15s ease;\n    }\n    button:hover {\n      background: #334155;\n      border-color: var(--accent-color);\n      color: #fff;\n    }\n    #viewport {\n      flex: 1;\n      position: relative;\n      overflow: hidden;\n      display: flex;\n      align-items: center;\n      justify-content: center;\n      background: radial-gradient(circle at center, #111827 0%, var(--bg-primary) 100%);\n      cursor: grab;\n    }\n    #viewport:active {\n      cursor: grabbing;\n    }\n    #diagram-container {\n      transform-origin: center center;\n      transition: transform 0.05s ease-out;\n      user-select: none;\n      padding: 40px;\n    }\n    .mermaid {\n      display: flex;\n      justify-content: center;\n    }\n    .mermaid svg {\n      max-width: none !important;\n      height: auto;\n    }\n  </style>\n  <script type=\"module\">\n    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';\n    mermaid.initialize({\n      startOnLoad: true,\n      theme: 'dark',\n      themeVariables: {\n        darkMode: true,\n        background: '#090d16',\n        primaryColor: '#1e293b',\n        primaryBorderColor: '#38bdf8',\n        primaryTextColor: '#f8fafc',\n        lineColor: '#60a5fa',\n        secondaryColor: '#0f172a',\n        tertiaryColor: '#1e293b'\n      }\n    });\n\n    let scale = 1;\n    let pointX = 0;\n    let pointY = 0;\n    let start = { x: 0, y: 0 };\n    let panning = false;\n\n    const viewport = document.getElementById('viewport');\n    const container = document.getElementById('diagram-container');\n\n    function setTransform() {\n      container.style.transform = `translate(${pointX}px, ${pointY}px) scale(${scale})`;\n    }\n\n    viewport.onmousedown = function (e) {\n      if (e.target.tagName === 'BUTTON') return;\n      e.preventDefault();\n      start = { x: e.clientX - pointX, y: e.clientY - pointY };\n      panning = true;\n    };\n\n    window.onmouseup = function () { panning = false; };\n\n    viewport.onmousemove = function (e) {\n      if (!panning) return;\n      e.preventDefault();\n      pointX = (e.clientX - start.x);\n      pointY = (e.clientY - start.y);\n      setTransform();\n    };\n\n    viewport.onwheel = function (e) {\n      e.preventDefault();\n      const xs = (e.clientX - pointX) / scale;\n      const ys = (e.clientY - pointY) / scale;\n      const delta = -e.deltaY;\n      if (delta > 0) {\n        scale *= 1.12;\n      } else {\n        scale /= 1.12;\n      }\n      scale = Math.min(Math.max(0.2, scale), 5);\n      pointX = e.clientX - xs * scale;\n      pointY = e.clientY - ys * scale;\n      setTransform();\n    };\n\n    window.zoomIn = () => { scale *= 1.25; setTransform(); };\n    window.zoomOut = () => { scale /= 1.25; setTransform(); };\n    window.resetZoom = () => { scale = 1; pointX = 0; pointY = 0; setTransform(); };\n    window.exportSvg = () => {\n      const svg = container.querySelector('svg');\n      if (!svg) return;\n      const blob = new Blob([svg.outerHTML], { type: 'image/svg+xml' });\n      const url = URL.createObjectURL(blob);\n      const a = document.createElement('a');\n      a.href = url;\n      a.download = 'diagram.svg';\n      a.click();\n      URL.revokeObjectURL(url);\n    };\n  </script>\n</head>\n<body>\n  <header>\n    <div class=\"title\">\n      <span>"
       page-title
       "</span>\n      <span class=\"badge\">ASL Native Visual</span>\n    </div>\n    <div class=\"toolbar\">\n      <button onclick=\"window.zoomIn()\">+ Zoom In</button>\n      <button onclick=\"window.zoomOut()\">- Zoom Out</button>\n      <button onclick=\"window.resetZoom()\">Reset</button>\n      <button onclick=\"window.exportSvg()\">Download SVG</button>\n    </div>\n  </header>\n  <div id=\"viewport\">\n    <div id=\"diagram-container\">\n      <pre class=\"mermaid\">\n"
       mermaid-code
       "\n      </pre>\n    </div>\n  </div>\n</body>\n</html>"))

(df diagram-to-html [(diag Diagram) (page-title Str)] -> Str
  :d "Transpiles structured Diagram directly into responsive visual HTML"
  (mermaid-to-html (diagram-to-mermaid diag) page-title))
