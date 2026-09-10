(module asl-vdom/tests/diagram-test
  :d "Unit tests for pure AgentScript Diagram AST and Mermaid HTML generator"
  :x [run-tests]
  :i [(diagram :a diag)])

(df test-diagram-mermaid-transpile [] -> Bool
  :d "Verifies that structured Diagram record transpiles into valid Mermaid flowchart syntax"
  (let [(n1 (diag/make-diagram-node "Harness" "Antigravity Harness" "rect"))
        (n2 (diag/make-diagram-node "Rewrite" "Subagent Rewrite System" "rect"))
        (n3 (diag/make-diagram-node "Queue" "Task Queue" "rounded"))
        (sg (diag/make-diagram-subgraph "Host" "Host Environment" (list n1 n2 n3)))
        (e1 (diag/make-diagram-edge "Harness" "Rewrite" ""))
        (e2 (diag/make-diagram-edge "Rewrite" "Queue" "enqueues"))
        (d (diag/make-diagram "TD" (list sg) (list e1 e2)))
        (m (diag/diagram-to-mermaid d))]
    (assert (string-contains? m "flowchart TD") "Mermaid output must specify flowchart TD")
    (assert (string-contains? m "subgraph Host[\"Host Environment\"]") "Mermaid output must contain subgraph")
    (assert (string-contains? m "Harness[\"Antigravity Harness\"]") "Node 1 must be rendered")
    (assert (string-contains? m "Queue(\"Task Queue\")") "Rounded node must use parens")
    (assert (string-contains? m "Harness --> Rewrite") "Edge 1 without label")
    (assert (string-contains? m "Rewrite -->|\"enqueues\"| Queue") "Edge 2 with label")
    true))

(df test-mermaid-to-html [] -> Bool
  :d "Verifies standalone HTML viewer generation with dark theme and pan-zoom"
  (let [(code "flowchart TD\n  A --> B")
        (html (diag/mermaid-to-html code "Test Architecture"))]
    (assert (string-contains? html "<!DOCTYPE html>") "HTML must have doctype")
    (assert (string-contains? html "<title>Test Architecture</title>") "HTML must set title")
    (assert (string-contains? html "cdn.jsdelivr.net/npm/mermaid@10") "HTML must load mermaid from CDN")
    (assert (string-contains? html "theme: 'dark'") "HTML must initialize dark theme")
    (assert (string-contains? html "class=\"mermaid\"") "HTML must include mermaid container")
    (assert (string-contains? html "A --> B") "HTML must embed raw diagram code")
    (assert (string-contains? html "window.zoomIn") "HTML must provide zoom controls")
    (assert (string-contains? html "window.exportSvg") "HTML must provide SVG export")
    true))

(df test-diagram-to-html-end-to-end [] -> Bool
  :d "Verifies end-to-end compilation from Diagram record to visual HTML viewer"
  (let [(n1 (diag/make-diagram-node "A" "Supervisor" "rect"))
        (n2 (diag/make-diagram-node "B" "Worker" "circle"))
        (sg (diag/make-diagram-subgraph "Group" "Core Cluster" (list n1 n2)))
        (e (diag/make-diagram-edge "A" "B" "dispatches"))
        (d (diag/make-diagram "LR" (list sg) (list e)))
        (html (diag/diagram-to-html d "End to End"))]
    (assert (string-contains? html "flowchart LR") "End to end must specify LR")
    (assert (string-contains? html "B((\"Worker\"))") "Circle node rendered properly")
    (assert (string-contains? html "A -->|\"dispatches\"| B") "Dispatches edge rendered")
    true))

(df run-tests [] -> Bool
  :d "Executes all diagram unit tests"
  (and (test-diagram-mermaid-transpile)
       (test-mermaid-to-html)
       (test-diagram-to-html-end-to-end)))

(run-tests)
