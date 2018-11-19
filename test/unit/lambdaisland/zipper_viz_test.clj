(ns lambdaisland.zipper-viz-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [lambdaisland.zipper-viz :as viz])
  (:import java.util.concurrent.atomic.AtomicInteger))

(defmacro with-stable-gensym [& body]
  `(let [id# (AtomicInteger. 1)]
     (with-redefs [clojure.core/gensym (fn gensym#
                                         ([]
                                          (gensym# "G__"))
                                         ([prefix#]
                                          (symbol (str prefix# (.getAndIncrement id#)))))
                   viz/id-map (atom {})]
       (let [res# (do ~@body)]
         (prn-str res#) ;; like a deep do-all to realize any lingering laziness
         res#))))

(deftest tree->dot-test
  (is (= [["G__1" {:label "[1 2 3]", :color "#3388ff", :style "filled"}]
          ["G__2" {:label "1"}]
          ["G__3" {:label "2"}]
          ["G__4" {:label "3"}]
          ["G__1" "G__2"]
          ["G__1" "G__3"]
          ["G__1" "G__4"]
          {:type :lambdaisland.zipper-viz/same-rank,
           :edge ["G__2" "G__3" {:style "invis"}]}
          {:type :lambdaisland.zipper-viz/same-rank,
           :edge ["G__3" "G__4" {:style "invis"}]}]

         (with-stable-gensym
           (viz/tree->dot (z/vector-zip [1 2 3]))))))

(deftest zipper->dot-test
  (is (= {:type :dorothy.core/digraph,
          :id nil,
          :strict? false,
          :statements
          [{:type :dorothy.core/node,
            :attrs {:label "[1 2 3]\nTop", :shape "diamond"},
            :id {:type :dorothy.core/node-id, :id "G__1", :port nil, :compass-pt nil}}]}

         (with-stable-gensym
           (viz/zipper->dot (z/vector-zip [1 2 3]))))))
