(ns jenjin.runner)

(import com.badlogic.gdx.ApplicationListener
        com.badlogic.gdx.Gdx
        com.badlogic.gdx.graphics.GL20
        com.badlogic.gdx.graphics.GL11
        com.badlogic.gdx.graphics.OrthographicCamera
        com.badlogic.gdx.backends.lwjgl.LwjglApplication
        com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration
        com.badlogic.gdx.graphics.glutils.ShaderProgram
        com.badlogic.gdx.graphics.Mesh
        com.badlogic.gdx.math.Vector2
        com.badlogic.gdx.graphics.VertexAttribute
        com.badlogic.gdx.graphics.VertexAttributes)

(def vert-shader
  "
attribute vec4 a_position;
uniform mat4 u_world;
uniform vec2 resolution;

void main () {
  gl_Position = u_world * a_position;
}
")

(def frag-shader
  "
uniform vec2 resolution;

void main () {
  gl_FragColor = vec4 (gl_FragCoord.xy / resolution, 0, 1.);
}
")

(defn make-mesh
  [isStatic maxVertices maxIndices & vertAttrs]
  (let [isStatic (boolean isStatic),
        maxVertices (int maxVertices),
        maxIndices (int maxIndices)]
    (Mesh. isStatic maxVertices maxIndices (into-array vertAttrs))))

(defn make-camera 
  []
  (let [cam (OrthographicCamera. (.getWidth Gdx/graphics) (.getHeight Gdx/graphics))]
    (.set (.position cam) (/ (.getWidth Gdx/graphics) 2) (/ (.getHeight Gdx/graphics) 2) 0)
    (.translate cam (- (/ (.getWidth Gdx/graphics) 2)) (- (/ (.getHeight Gdx/graphics) 2)))
    (.update cam)
    cam))

(def shader-program (atom nil))
(def quad (atom nil))
(def cam (atom nil))

(defn create []
  ;; Initialize the initial shader program!
  (reset! shader-program (ShaderProgram. vert-shader frag-shader))
  ;; Initialize the camera!
  (reset! cam (make-camera))
  ;; Initialize the quad we're rendering!
  (reset! quad (make-mesh true 4 0 (VertexAttribute/Position)))
  (let [w (/ (.getWidth (Gdx/graphics)) 2)
        h (/ (.getHeight (Gdx/graphics)) 2)
        nw (- w)
        nh (- h)]
    (.setVertices @quad (float-array [nw nh 0.
                                      w nh 0.
                                      w h 0.
                                      nw h 0.]))))

(defmacro no-break [& exprs]
  `(try ~@exprs (catch Throwable throbl#)))

(defn render []
  (.glViewport (Gdx/gl20) 0 0 (.getWidth (Gdx/graphics)) (.getHeight (Gdx/graphics)))
  (.glClearColor (Gdx/gl) 1 0.8 1 1)
  (.glClear (Gdx/gl) GL20/GL_COLOR_BUFFER_BIT)

  (let [gl (.getGL20 (Gdx/graphics))]
    (.update @cam)

    (do (.begin @shader-program)
        (.setUniformMatrix @shader-program "u_world" (.combined @cam))
        (.setUniformf @shader-program "resolution"
                      (Vector2. (.getWidth (Gdx/graphics)) (.getHeight (Gdx/graphics))))
        (.render @quad @shader-program GL20/GL_TRIANGLE_FAN)
        (.end @shader-program))))

(def Runner
  (proxy [ApplicationListener] []
    (create [] (create))
    (pause [])
    (resume [])
    (render [] (render))
    (resize [x y])
    (dispose [])))

(def config
  (let [cfg (LwjglApplicationConfiguration.)]
    (set! (.title cfg) "my-gdx-game")
    (set! (.useGL20 cfg) true)
    (set! (.width cfg) 480)
    (set! (.height cfg) 320)
    cfg))


(defonce app (new LwjglApplication Runner config))

;(use 'clojure.reflect)
;(reflect Mesh)

