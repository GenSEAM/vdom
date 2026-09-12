(module asl-vdom/tests/asn-svg-test
  :d "Unit tests for pure AgentScript ASN Vector Graphics Transpiler"
  :x [run-tests]
  :i [(asn_svg :a svg)])

(df test-render-rect [] -> Bool
  :d "Verifies rectangle XML generation with corner radiuses"
  (let [(node (svg/SvgNode :kind "rc" :attrs (list (pair "x" "10") (pair "y" "20") (pair "w" "300") (pair "h" "200") (pair "rx" "16") (pair "f" "#090d16") (pair "s" "#38bdf8")) :children (list) :text-content ""))
        (xml (svg/render-rect node))]
    (assert (string-contains? xml "<rect x=\"10\"") "Rect XML must contain x=10")
    (assert (string-contains? xml "width=\"300\"") "Rect XML must contain width=300")
    (assert (string-contains? xml "rx=\"16\"") "Rect XML must contain rx=16")
    (assert (string-contains? xml "fill=\"#090d16\"") "Rect XML must contain fill color")
    true))

(df test-render-circle [] -> Bool
  :d "Verifies circle XML generation"
  (let [(node (svg/SvgNode :kind "circ" :attrs (list (pair "cx" "160") (pair "cy" "160") (pair "r" "80") (pair "f" "#38bdf8") (pair "s" "#ffffff")) :children (list) :text-content ""))
        (xml (svg/render-circle node))]
    (assert (string-contains? xml "<circle cx=\"160\"") "Circle XML must contain cx=160")
    (assert (string-contains? xml "r=\"80\"") "Circle XML must contain r=80")
    (assert (string-contains? xml "stroke=\"#ffffff\"") "Circle XML must contain stroke color")
    true))

(df test-render-poly-fallback [] -> Bool
  :d "Verifies polygon gets visible fallback stroke when neither fill nor stroke are passed"
  (let [(node (svg/SvgNode :kind "poly" :attrs (list (pair "points" "10,10 50,50 10,50")) :children (list) :text-content ""))
        (xml (svg/render-polygon node))]
    (assert (string-contains? xml "<polygon points=\"10,10 50,50 10,50\"") "Polygon XML must contain points")
    (assert (string-contains? xml "stroke=\"#38bdf8\"") "Polygon XML must contain fallback stroke")
    true))

(df test-render-path-fallback [] -> Bool
  :d "Verifies path gets visible fallback stroke when color is omitted"
  (let [(node (svg/SvgNode :kind "p" :attrs (list (pair "d" "M 10 10 L 50 50 Z")) :children (list) :text-content ""))
        (xml (svg/render-path node))]
    (assert (string-contains? xml "<path d=\"M 10 10 L 50 50 Z\"") "Path XML must contain d attribute")
    (assert (string-contains? xml "stroke=\"#38bdf8\"") "Path XML must contain fallback stroke")
    true))

(df test-asn-to-svg [] -> Bool
  :d "Verifies end-to-end ASN S-expression transpilation into valid SVG"
  (let [(res (svg/asn-to-svg "(:svg :w 320 :h 320 (:rc :x 0 :y 0 :w 320 :h 320 :rx 24 :f \"#090d16\"))"))
        (xml (.-svg res))
        (ok (.-success res))]
    (assert ok "ASN to SVG transpilation must succeed")
    (assert (string-contains? xml "<svg xmlns=\"http://www.w3.org/2000/svg\"") "SVG must have xmlns")
    (assert (string-contains? xml "viewBox=\"0 0 320 320\"") "SVG must have viewBox")
    (assert (string-contains? xml "<rect x=\"0\"") "SVG must contain rect")
    true))

(df test-multi-sibling-rects [] -> Bool
  :d "Verifies multiple sibling rectangles are all parsed and preserved"
  (let [(res (svg/asn-to-svg "(:svg :w 320 :h 320 (:rc :x 10 :y 10 :w 50 :h 50) (:rc :x 70 :y 10 :w 50 :h 50) (:rc :x 130 :y 10 :w 50 :h 50) (:rc :x 190 :y 10 :w 50 :h 50) (:rc :x 250 :y 10 :w 50 :h 50))"))
        (xml (.-svg res))
        (ok (.-success res))]
    (assert ok "ASN to SVG transpilation must succeed for 5 rects")
    (assert (string-contains? xml "<rect x=\"10\"") "Must contain rect 1")
    (assert (string-contains? xml "<rect x=\"70\"") "Must contain rect 2")
    (assert (string-contains? xml "<rect x=\"130\"") "Must contain rect 3")
    (assert (string-contains? xml "<rect x=\"190\"") "Must contain rect 4")
    (assert (string-contains? xml "<rect x=\"250\"") "Must contain rect 5")
    true))

(df test-multi-sibling-mixed-shapes [] -> Bool
  :d "Verifies mixed sibling shape types are all parsed and preserved without loss"
  (let [(res (svg/asn-to-svg "(:svg :w 320 :h 320 (:rc :x 10 :y 10 :w 40 :h 40) (:circ :cx 50 :cy 50 :r 20) (:ln :x1 0 :y1 0 :x2 100 :y2 100) (:rc :x 60 :y 60 :w 40 :h 40) (:circ :cx 150 :cy 150 :r 30))"))
        (xml (.-svg res))
        (ok (.-success res))]
    (assert ok "ASN to SVG transpilation must succeed for mixed shapes")
    (assert (string-contains? xml "<rect x=\"10\"") "Must contain rect 1")
    (assert (string-contains? xml "<rect x=\"60\"") "Must contain rect 2")
    (assert (string-contains? xml "<circle cx=\"50\"") "Must contain circle 1")
    (assert (string-contains? xml "<circle cx=\"150\"") "Must contain circle 2")
    (assert (string-contains? xml "<line x1=\"0\"") "Must contain line 1")
    true))

(df run-tests [] -> Bool
  :d "Executes all test cases in suite"
  (and (test-render-rect)
       (test-render-circle)
       (test-render-poly-fallback)
       (test-render-path-fallback)
       (test-asn-to-svg)
       (test-multi-sibling-rects)
       (test-multi-sibling-mixed-shapes)))

(run-tests)
