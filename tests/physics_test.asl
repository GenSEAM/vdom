(module asl-vdom/tests/physics_test
  :d "Unit test suite for graph physics reactor: repulsion, spring elasticity, and damping."
  :x [test-coulomb-repulsion-direction
      test-euler-damping
      test-hooke-spring-linearity
      run-tests]
  :i [(physics-reactor :a pr)])

(df test-coulomb-repulsion-direction [] -> Bool
  :d "Verifies node at (10, 0) is repelled in positive X direction away from node at (0, 0)."
  (let [(p1 (pr/make-particle 10.0 0.0))
        (p2 (pr/make-particle 0.0 0.0))
        (f (pr/coulomb-repulsion p1 p2 1000.0))
        (f-rev (pr/coulomb-repulsion p2 p1 1000.0))]
    (assert (> (.-fx f) 0.0) "Coulomb repulsion must produce positive fx")
    (assert (< (.-fx f-rev) 0.0) "Opposite particle must be repelled in negative X direction")
    (assert (not (<= (.-fx f) 0.0)) "Coulomb repulsion must not produce non-positive fx")
    (assert (not (> (.-fx f-rev) 0.0)) "Opposite particle repulsion must not produce positive fx")
    (assert (not (> (.-fy f) 0.0)) "Pure horizontal repulsion must not produce positive fy")
    (assert (not (< (.-fy f) 0.0)) "Pure horizontal repulsion must not produce negative fy")
    true))

(df test-euler-damping [] -> Bool
  :d "Verifies damping reduces particle velocity."
  (let [(p1 (pr/Particle :x 0.0 :y 0.0 :vx 10.0 :vy 0.0 :mass 1.0))
        (f0 (pr/Force2D :fx 0.0 :fy 0.0))
        (p2 (pr/euler-integrate p1 f0 1.0 0.9))]
    (assert (< (.-vx p2) 10.0) "Euler damping must reduce velocity below 10.0")
    (assert (> (.-vx p2) 0.0) "Damped velocity must remain positive")
    (assert (not (>= (.-vx p2) 10.0)) "Damped velocity must not stay at or exceed original velocity")
    (assert (not (< (.-vx p2) 0.0)) "Damped velocity must not invert direction")
    (assert (not (= (.-vx p2) 10.0)) "Undamped velocity must not persist under damping factor 0.9")
    true))

(df test-hooke-spring-linearity [] -> Bool
  :d "Verifies Hooke spring restorative force displacement scales linearly with Euclidean distance."
  (let [(p1 (pr/make-particle 0.0 0.0))
        (p2 (pr/make-particle 10.0 0.0))
        (f1 (pr/hooke-spring-force p1 p2 5.0 1.0))
        (p3 (pr/make-particle 5.0 0.0))
        (f0 (pr/hooke-spring-force p1 p3 5.0 1.0))]
    (assert (> (.-fx f1) 0.0) "Spring force on p1 toward p2 must be positive in X")
    (assert (= (.-fx f1) 50.0) "Spring force fx1 must equal 50.0 for dist 10, rest 5, k 1")
    (assert (= (.-fy f1) 0.0) "Spring force fy1 must equal 0.0 for horizontal displacement")
    (assert (= (.-fx f0) 0.0) "At equilibrium rest distance spring force fx must be zero")
    true))

(df run-tests [] -> Bool
  :d "Runs all physics reactor tests."
  (and (test-coulomb-repulsion-direction)
       (test-euler-damping)
       (test-hooke-spring-linearity)))

(run-tests)

