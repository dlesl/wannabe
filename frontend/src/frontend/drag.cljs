(ns frontend.drag
  (:require [reagent.core :as r]
            [reagent.dom :as d]
            [clojure.string :as str]
            ["interactjs" :as interact]))

(defn ^:private element-id [id]
  (str "draggable-" id))

(defn ^:private translate [{:keys [dx dy]}]
  (str "translate(" dx " " dy ")"))

;; HACK!
(defn ^:private collect-transforms
  ([^js/DOMNode dom-node]
   (collect-transforms dom-node []))
  ([^js/DOMNode dom-node acc]
   (if (= (.-nodeName dom-node) "svg")
     (str/join " " acc)
     (recur (.-parentNode dom-node)
            (conj acc (some-> dom-node .-attributes .-transform .-value))))))

(defn ^:private get-scale [^js/DOMNode dom-node]
  (if (= (.-nodeName dom-node) "svg")
    (let [vb-width (-> dom-node .-viewBox .-baseVal .-width)
          width (-> dom-node .getClientRects (aget 0) .-width)]
      (/ vb-width width))
    (recur (.-parentNode dom-node))))

(defn init [on-drag on-end]
  {:drag-atom (r/atom nil)
   :on-drag on-drag
   :on-end on-end})

(defn last-item [{:keys [drag-atom]}]
  (when-let [{:keys [current-drag-id transform]} @drag-atom]
    [:use {:href (str \# (element-id current-drag-id))
           :transform transform}]))

(defn dragged [{:keys [drag-atom]} {:keys [id child class]}]
  (let [{:keys [current-drag-id]} @drag-atom
        dragging? (= current-drag-id id)]
    [:g {:style {:opacity (str (when dragging? "0"))}
         :class class}
     [:g {:id (element-id id)
          :transform (str (when dragging? (translate @drag-atom)))}
      child]]))

(defn draggable [{:keys [drag-atom on-drag on-end]} {:keys [id on-drop]}]
  (r/create-class
   {:reagent-render
    (fn [drag-state opts]
      [:g [dragged drag-state opts]])

    :component-did-mount
    (fn [this]
      (-> (interact (d/dom-node this))
          (.draggable #js {:inertia "true"})
          (.on "dragstart"
               (fn [_]
                 (reset! drag-atom {:current-drag-id id
                                    :scale (get-scale (d/dom-node this))
                                    :transform (collect-transforms (d/dom-node this))
                                    :dx 0
                                    :dy 0
                                    :on-drop on-drop})))
          (.on "dragmove"
               (fn [ev]
                 (let [{:keys [scale]} @drag-atom]
                   (swap! drag-atom
                          #(-> %
                               (update :dx + (* scale (.-dx ev)))
                               (update :dy + (* scale (.-dy ev))))))
                 (let [{:keys [current-drag-id dx dy]} @drag-atom]
                   (on-drag current-drag-id dx dy))))
          (.on "dragend" (fn []
                           (reset! drag-atom nil)
                           (on-end)))))}))

(defn dropzone [{:keys [drag-atom]} {:keys [name on-enter on-leave]}]
  (r/create-class
   {:reagent-render
    (fn [_drag-state {:keys [child]}] child)

    :component-did-mount
    (fn [this]
      (-> (interact (d/dom-node this))
          (.dropzone {:overlap 0.75})
          (.on "drop" (fn [_]
                        (when-let [f (:on-drop @drag-atom)] (f name))
                        (when on-leave (on-leave))))
          (.on "dragenter" #(when on-enter (on-enter)))
          (.on "dragleave" #(when on-leave (on-leave)))))}))

(defn set-remote-drag [{:keys [drag-atom]} {:keys [id dx dy]}]
  (let [{:keys [scale transform]} @drag-atom
        node (js/document.getElementById (element-id id))
        scale (or scale (get-scale node))
        transform (or transform (collect-transforms node))]
    (reset! drag-atom {:current-drag-id id
                       :remote? true
                       :scale scale
                       :transform transform
                       :dx dx
                       :dy dy})))

(defn end-remote-drag [{:keys [drag-atom]}]
  (reset! drag-atom nil))
