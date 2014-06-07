
(defpackage :clml.graph.read-graph
  (:nicknames :read-graph)
  (:use :cl 
        :parse-number
         :hjs.util.matrix
        :hjs.util.vector
        :hjs.util.meta
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
         :hjs.util.matrix
        :hjs.util.vector
        :hjs.util.meta
        :clml.graph.read-graph
        )
  (:export #:retrieve-node
           #:retrieve-link
           #:adjacency
           #:adjacency-matrix
           #:get-connected-components))

(defpackage :clml.graph.shortest-path
  (:nicknames :graph-shortest-path)
  (:use :cl
        :hjs.util.meta
        :hjs.util.vector
        :hjs.learn.read-data
        :hjs.util.matrix
        :clml.graph.read-graph
        :clml.graph.graph-utils
        :clml.utility.priority-que
        :hjs.util.missing-value
        )
  #+allegro
  (:use :excl)
  (:export #:find-shortest-path-dijkstra
           #:graph-distance-matrix
           #:%find-all-shortest-paths))

(defpackage :clml.graph.graph-centrality
  (:use :cl
        :hjs.learn.vars
        :hjs.util.matrix
        :hjs.util.vector
        :hjs.util.meta
        :statistics
        :clml.graph.read-graph
        :clml.graph.graph-utils
        :clml.graph.shortest-path)
  (:export #:eccentricity-centrality
           #:closeness-centrality
           #:degree-centrality
           #:eigen-centrality
           #:pagerank)
  (:import-from :graph-shortest-path #:%find-all-shortest-paths))


(defpackage :clml.graph.graph-anomaly-detection
  (:use :cl
        
        :hjs.learn.vars
        :hjs.learn.read-data
        :hjs.util.matrix
        :hjs.util.vector
        :hjs.util.meta
        
        :statistics
        :read-data
        :missing-val
        :clml.utility.csv
        :clml.time-series.util
        :clml.time-series.read-data
        :clml.time-series.statistics
        :clml.time-series.state-space
        :clml.time-series.autoregression
        :clml.graph.read-graph
        :clml.graph.graph-centrality
        :clml.graph.shortest-path)
  #+allegro
  (:use :excl)
  (:export ))





