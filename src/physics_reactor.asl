(module asl-vdom/physics-reactor
  :d "Pure AgentScript graph physics engine: Coulomb repulsion, Hooke spring force, and Euler numerical integration."
  :x [Particle
      Spring
      Force2D
      make-particle
      coulomb-repulsion
      hooke-spring-force
      euler-integrate]
  :i [])

(dfs Particle
  (:f x F64 "Position X coordinate")
  (:f y F64 "Position Y coordinate")
  (:f vx F64 "Velocity X component")
  (:f vy F64 "Velocity Y component")
  (:f mass F64 "Inertial particle mass"))

(dfs Spring
  (:f from-idx I64 "Source node index")
  (:f to-idx I64 "Target node index")
  (:f rest-length F64 "Equilibrium spring distance")
  (:f stiffness F64 "Hooke elasticity constant"))

(dfs Force2D
  (:f fx F64 "Force X component")
  (:f fy F64 "Force Y component"))

(df make-particle [(x F64) (y F64)] -> Particle
  :d "Constructs particle at position with zero initial velocity."
  (Particle :x x :y y :vx 0.0 :vy 0.0 :mass 1.0))

(df coulomb-repulsion [(p1 Particle) (p2 Particle) (k F64)] -> Force2D
  :d "Calculates inverse-square repulsive force with softening factor to prevent singularity."
  (let [(dx (- (.-x p1) (.-x p2)))
        (dy (- (.-y p1) (.-y p2)))
        (dist-sq (+ (* dx dx) (+ (* dy dy) 0.01)))
        (force (/ k dist-sq))]
    (Force2D :fx (* dx force) :fy (* dy force))))

(df hooke-spring-force [(p1 Particle) (p2 Particle) (rest F64) (k F64)] -> Force2D
  :d "Calculates Hooke linear spring restorative force along connecting edge."
  (let [(dx (- (.-x p2) (.-x p1)))
        (dy (- (.-y p2) (.-y p1)))
        (dist (sqrt (+ (* dx dx) (* dy dy))))
        (displacement (- dist rest))]
    (Force2D :fx (* (* dx k) displacement) :fy (* (* dy k) displacement))))

(df euler-integrate [(p Particle) (f Force2D) (dt F64) (damping F64)] -> Particle
  :d "Performs Euler numerical integration step updating velocity and position."
  (let [(new-vx (* (+ (.-vx p) (* (/ (.-fx f) (.-mass p)) dt)) damping))
        (new-vy (* (+ (.-vy p) (* (/ (.-fy f) (.-mass p)) dt)) damping))
        (new-x (+ (.-x p) (* new-vx dt)))
        (new-y (+ (.-y p) (* new-vy dt)))]
    (Particle :x new-x :y new-y :vx new-vx :vy new-vy :mass (.-mass p))))
