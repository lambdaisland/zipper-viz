(ns lambdaisland.zipper-viz
  (:refer-clojure :exclude [spit])
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]
            [dorothy.core :as dot]
            [dorothy.jvm :as dotj]
            [clojure.string :as str])
  (:import java.io.ByteArrayInputStream
           javax.imageio.ImageIO))

(def end-color "#BA55D3")
(def cursor-color "#3388ff")
(def changed-color "#F0706A")

(def id-map (atom {}))

(defn gen-id [target]
  (if-let [id (get @id-map target)]
    id
    (let [id (name (gensym))]
      (swap! id-map assoc target id)
      id)))

;; dot stuff

(defn same-rank [edge]
  {:type ::same-rank
   :edge edge})

(defmethod dot/dot* ::same-rank [{:keys [edge]}]
  (str "{rank=same; " (dot/dot* (#'dot/to-ast edge)) ";}"))

(defmethod dot/dot* ::dot/edge [{:keys [attrs node-ids]}]
  (str
   (str/join (str " " (:edge-op attrs (:edge-op @#'dot/*options*)) " ") (map dot/dot* node-ids))
   (#'dot/dot*-trailing-attrs (dissoc attrs :edge-op))))

(defn xml-label [node]
  (if-let [tag (:tag node)]
    (str "<" (name tag) (str/join (map (fn [[k v]] (str "\n " (name k) "=" v)) (:attrs node))) ">")
    (pr-str node)))

(defn prevs [loc]
  (take-while (complement nil?) (next (iterate z/prev loc))))

(defn nexts [loc]
  (take-while (complement z/end?) (next (iterate z/next loc))))

(defn loc->dot-node [loc & [attrs]]
  [(gen-id loc) attrs])

(defn loc->dot-edge [loc]
  (when (some-> loc z/up z/node)
    [(gen-id (z/up loc)) (gen-id loc)]))

(defn loc->constraints [loc]
  (when-let [l (z/left loc)]
    (same-rank [(gen-id l) (gen-id loc) {:style "invis"}])))

(defn loc->opts [loc]
  {:label (if (= (type loc) ::z/xml-zipper)
            xml-label
            pr-str)})

(defn tree->dot
  ([loc]
   (tree->dot loc (loc->opts loc)))
  ([loc {:keys [label] :or {label pr-str}}]
   (let [end? (z/end? loc)
         loc (cond-> loc end? (assoc 1 nil))
         prevs (prevs loc)
         nexts (nexts loc)]
     (concat (map #(loc->dot-node % {:label (label (z/node %))}) prevs)
             [(loc->dot-node loc {:label (str (label (z/node loc))
                                              (when end? "\n:end"))
                                  :color (if end? end-color cursor-color)
                                  :style "filled"})]
             (map #(loc->dot-node % {:label (label (z/node %))}) nexts)
             (keep loc->dot-edge (concat prevs [loc] nexts))
             (keep loc->constraints prevs)
             (some-> loc loc->constraints vector)
             (keep loc->constraints nexts)))))

(defn subtree->dot [loc & [{:keys [label] :or {label pr-str}}]]
  (let [dot-node #(loc->dot-node % {:label (label (z/node %))})]
    (if-let [down (and (z/branch? loc) (z/down loc))]
      (let [downs (take-while identity (iterate z/right down))]
        (concat [(dot-node loc)]
                (keep identity (mapcat (juxt loc->dot-edge loc->constraints) downs))
                (mapcat subtree->dot downs)))
      [(dot-node loc)])))

(defn zipper? [z]
  (#{::z/zipper ::z/xml-zipper} (type z)))

(defn parent [loc]
  (let [[_ {ppath :ppath pnodes :pnodes :as ploc}] loc]
    (when (and ploc (not (z/end? loc)))
      (with-meta [(last pnodes) ppath] (meta loc)))))

(defn lefts->dot [loc & [{:keys [label] :or {label pr-str} :as opts}]]
  (let [lefts (z/lefts loc)]
    (if (empty? lefts)
      [[(gen-id [:lefts loc]) {:shape "point"}]
       [(gen-id loc) (gen-id [:lefts loc]) {:label "L"}]]
      (->> `[~@lefts ~loc]
           (partition 2 1)
           (mapcat (fn [[ln rn]]
                     [[(gen-id ln) {:label (label ln)}]
                      (if (zipper? rn)
                        [(gen-id rn) (gen-id ln) {:label "L"}]
                        (same-rank [(gen-id ln) (gen-id rn) {:arrowhead "none"}]))]))))))

(defn rights->dot [loc & [{:keys [label] :or {label pr-str} :as opts}]]
  (let [rights (z/rights loc)]
    (if (empty? rights)
      [[(gen-id [:rights loc]) {:shape "point"}]
       [(gen-id loc) (gen-id [:rights loc]) {:label "R"}]]
      (->> `[~loc ~@rights]
           (partition 2 1)
           (mapcat (fn [[ln rn]]
                     [[(gen-id rn) {:label (label rn)}]
                      (if (zipper? ln)
                        [(gen-id ln) (gen-id rn) {:label "R"}]
                        (same-rank [(gen-id ln) (gen-id rn) {:arrowhead "none"}]))]))))))


(defn loc->dot [loc & [{:keys [label] :or {label pr-str} :as opts}]]
  (when-not (nil? (second loc))
    `[~@(lefts->dot loc opts)
      ~@(when-let [up (parent loc)]
          `[~[(gen-id up) (cond-> {:shape "diamond" :label (str (label (z/node up))
                                                                (when (nil? (get up 1)) "\nTop"))}
                            (:changed? (second up))
                            (assoc :color changed-color :style "filled"))]
            ~[(gen-id loc) (gen-id up) {:label "P"}]
            ~@(loc->dot up opts)])
      ~@(rights->dot loc opts)]))

(defn zipper->dot
  ([loc]
   (dot/digraph (zipper->dot loc (loc->opts loc))))
  ([loc & [{:keys [label] :or {label pr-str} :as opts}]]
   (let [end? (z/end? loc)
         loc (cond-> loc end? (assoc 1 nil))]
     `[~@(loc->dot loc opts)
       ~[(gen-id loc) (cond-> {:label (str (label (z/node loc))
                                           (when end? "\n:end")
                                           (when (nil? (get loc 1)) "\nTop")) :shape "diamond"}
                        (:changed? (second loc))
                        (assoc :color changed-color :style "filled")

                        end?
                        (assoc :color end-color :style "filled"))]])))

(defn render [dot]
  (-> dot
      dot/dot
      (dotj/render {:format :jpg})
      ByteArrayInputStream.
      ImageIO/read))

(defn spit [dot file type]
  (-> dot
      dot/dot
      (dotj/save! file {:format type})))

(defn viz-tree [z & [opts]]
  (-> z (tree->dot opts) render))

;; (if (find-ns 'cider.nrepl.middleware.content-type)
;;   (eval
;;    '(defmethod cider.nrepl.middleware.content-type/content-type-response ::z/zipper [response]
;;       (cider.nrepl.middleware.content-type/content-type-response
;;        (update response :value viz-tree)))))

;; (if (find-ns 'cider.nrepl.middleware.content-type)
;;   (eval
;;    '(defmethod cider.nrepl.middleware.content-type/content-type-response ::z/xml-zipper [response]
;;       (cider.nrepl.middleware.content-type/content-type-response
;;        (update response :value viz-tree {:label xml-label})))))

(doseq [var [#'z/zipper #'z/vector-zip #'z/seq-zip]]
  (alter-var-root var (fn [f]
                        (fn [& args]
                          (vary-meta (apply f args) assoc :type ::z/zipper)))))

(doseq [var [#'z/next #'z/prev]]
  (alter-var-root var (fn [f]
                        (fn [arg]
                          (let [res (f arg)]
                            (if (and res (meta arg))
                              (with-meta res (meta arg))
                              res))))))

(alter-var-root #'z/xml-zip (fn [f]
                              (fn [& args]
                                (vary-meta (apply f args) assoc :type ::z/xml-zipper))))
