(module asl-vdom/tests/physics_test
  :d "Unit test suite for graph physics reactor: repulsion, spring elasticity, and damping."
  :x [test-coulomb-repulsion-direction
      test-euler-damping]
  :i [(physics-reactor :a pr)])

(df test-coulomb-repulsion-direction [] -> Bool
  :d "Verifies node at (10, 0) is repelled in positive X direction away from node at (0, 0)."
  (let [(p1 (pr/make-particle 10.0 0.0))
        (p2 (pr/make-particle 0.0 0.0))
        (f (pr/coulomb-repulsion p1 p2 1000.0))]
    (> (.-fx f) 0.0)))

(df test-euler-damping [] -> Bool
  :d "Verifies damping reduces particle velocity."
  (let [(p1 (pr/Particle :x 0.0 :y 0.0 :vx 10.0 :vy 0.0 :mass 1.0))
        (f0 (pr/Force2D :fx 0.0 :fy 0.0))
        (p2 (pr/euler-integrate p1 f0 1.0 0.9))]
    (< (.-vx p2) 10.0)))
