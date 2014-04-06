(defproject clsl "0.1.0-SNAPSHOT"
  :description "GLSL shaders in clojure made awesome"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.badlogicgames.gdx/gdx "0.9.9"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "0.9.9"]
                 [com.badlogicgames.gdx/gdx-platform "0.9.9" 
                  :classifier "natives-desktop"]
                 [instaparse "1.2.16"]
                 [org.clojure/core.match "0.2.1"]]
  :repositories [["sonatype" 
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :resource-paths ["shaders" "grammars"])
