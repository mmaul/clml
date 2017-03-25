
(defpackage :clml.graph.read-graph

  (:use :cl
        :parse-number
         :clml.hjs.matrix
        :clml.hjs.vector
        :clml.hjs.meta

        )
  #+allegro
  (:use :excl)

  (:export #:node
           #:link
           #:node-id
           #:node-name
           #:node-links
           #:node-buff
           #:link-weight
           #:link-node1
           #:link-node2
           #:link-directed

           #:simple-graph
           #:simple-graph-series
           #:nodes
           #:links
           #:directed-p
           #:graphs

           #:read-graph
           #:do-graph-series
           #:read-graph-series
           #:make-simple-graph
           ))
(defpackage :clml.graph.graph-utils
  (:use :cl
        :parse-number
         :clml.hjs.matrix
        :clml.hjs.vector
        :clml.hjs.meta
        :clml.graph.read-graph
        )
  (:export #:retrieve-node
           #:retrieve-link
           #:adjacency
           #:adjacency-matrix
           #:get-connected-components))

(defpackage :clml.graph.shortest-path

  (:use :cl
        :clml.hjs.meta
        :clml.hjs.vector
        :clml.hjs.read-data
        :clml.hjs.matrix
        :clml.graph.read-graph
        :clml.graph.graph-utils
        :clml.utility.priority-que
        :clml.hjs.missing-value
        )
  #+allegro
  (:use :excl)
  (:export #:find-shortest-path-dijkstra
           #:graph-distance-matrix
           #:%find-all-shortest-paths))

(defpackage :clml.graph.graph-centrality
  (:use :cl
        :clml.hjs.vars
        :clml.hjs.matrix
        :clml.hjs.vector
        :clml.hjs.meta
        :clml.statistics
        :clml.hjs.eigensystems
        :clml.graph.read-graph
        :clml.graph.graph-utils
        :clml.graph.shortest-path)
  (:export #:eccentricity-centrality
           #:closeness-centrality
           #:degree-centrality
           #:eigen-centrality
           #:pagerank)
  (:import-from :clml.graph.shortest-path #:%find-all-shortest-paths)
  (:documentation "Graph Centrailty

*** sample usage
#+INCLUDE: \"../sample/graph-centrality.org\"  example lisp ")
  )


(defpackage :clml.graph.graph-anomaly-detection
  (:use :cl

        :clml.hjs.vars
        :clml.hjs.read-data
        :clml.hjs.matrix
        :clml.hjs.vector
        :clml.hjs.meta
        :clml.hjs.eigensystems
        :clml.statistics
        :clml.hjs.read-data
        :clml.hjs.missing-value
        :clml.utility.csv
        :clml.time-series.util
        :clml.time-series.read-data
        :clml.time-series.statistics
        :clml.time-series.state-space
        :clml.time-series.autoregression
        :clml.graph.read-graph
        :clml.graph.graph-centrality
        :clml.graph.shortest-path)
  #-(or lispworks allegro)
  (:import-from :cl-fad #:pathname-as-directory #:directory-exists-p)
  #+allegro
   (:import-from :cl-fad #:directory-exists-p)
  #+allegro
    (:use :excl)
  ;(:shadow #:predict)
  (:export ))





