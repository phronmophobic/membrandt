(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io ]
            [clojure.string :as str]))

(def lib 'com.phronemophobic/membrandt)
(def version "0.1.0-SNAPSHOT")

(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile [_]
  #_(b/javac {:src-dirs ["src-java"]
            :class-dir class-dir
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))

(defn jar [opts]
  (compile opts)
  (b/write-pom {:pom-data [[:licenses
                              [:license
                               [:name "MIT License"]
                               [:url "https://github.com/phronmophobic/membrandt/blob/main/LICENSE"]]]]
                :class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy [opts]
  (jar opts)
  (try ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
        (merge {:installer :remote
                :artifact jar-file
                :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
               opts))
       (catch Exception e
         (if-not (str/includes? (ex-message e) "redeploying non-snapshots is not allowed")
           (throw e)
           (println "This release was already deployed."))))
  opts)

(defn deploy-icon-resources [opts]
  (let [basis (b/create-basis {:project "ant-icons/deps.edn"})
        coord 'com.phronemophobic/membrandt-icons

        base-dir "ant-icons"
        build-dir (-> (io/file base-dir "target")
                      (.getCanonicalPath))
        class-dir (-> (io/file build-dir "classes")
                      (.getCanonicalPath))

        jar-file (-> (io/file build-dir
                              (format "%s-%s.jar" (name coord) version))
                     (.getCanonicalPath))]
    (b/delete {:path build-dir})
    (b/write-pom {:pom-data [[:licenses
                              [:license
                               [:name "MIT License"]
                               [:url "https://github.com/phronmophobic/membrandt/blob/main/ant-icons/LICENSE"]]]]
                  :class-dir class-dir
                  :lib coord
                  :version version
                  :basis basis})
    (b/copy-dir {:src-dirs ["ant-icons/resources"]
                 :target-dir class-dir})
    (b/jar {:jar-file jar-file
            :class-dir class-dir})
    (try ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
          (merge {:installer :remote
                  :artifact jar-file
                  :pom-file (b/pom-path {:lib coord
                                         :class-dir class-dir})}
                 opts))
         (catch Exception e
           (if-not (str/includes? (ex-message e) "redeploying non-snapshots is not allowed")
             (throw e)
             (println "This release was already deployed."))))))

