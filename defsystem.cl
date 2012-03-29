;;; -*- Mode: Lisp -*-

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ignore                              ; don't use asdf in this project
  (require :asdf)
  #+sbcl (progn
           ;; Modules won't load if sb-fasl:*fasl-file-type* is not "fasl"
           ;; So load them first
           (loop for module in '(:sb-posix :sb-aclrepl :sb-bsd-sockets :sb-cltl2 :sb-cover
                                 :sb-introspect :sb-md5 :sb-rotate-byte :sb-sprof)
                 do (require module))))


;;; Write your own DEFSYSTEM if you need libraries
#+ignore
(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for dir in (append (directory "*/*.asd") (directory "addons/*/*.asd")) do 
    (pushnew (make-pathname :directory (pathname-directory dir))
             asdf:*central-registry*
             :test 'equal))) 

#-allegro (load "allegro/defsys.cl")
#+allegro (load "allegro/defsys-patch.cl")

#+sbcl (setq sb-fasl:*fasl-file-type* "sbfasl")

#+sbcl (setq *print-level* 6
             *print-length* 11)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :future *features*))

#+(and unix (not lispworks))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :fork-future *features*))

#+(or mswindows linux)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :mkl *features*))

;; #+(version>= 8 2)
;; (setq excl:*fasl-default-type* "fasl82")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+lispworks
  (progn
    (shadow 'concatenate-system)
    (shadowing-import'(defsys:defsystem defsys:load-system defsys:compile-system)))
  (use-package :defsys)
  (loop while (not (eq *read-default-float-format* 'double-float))
        do
     (restart-case
         (error "Please set *read-default-float-format* to 'double-float before loading/compiling the system.")
       (use-double-float ()
         :report "Set double-float to *read-default-float-format*."
         (setq *read-default-float-format* 'double-float)))))

#+lispworks
(defmacro concatenate-system (name destination)
  `(let* ((files (defsys::concatenate-system ,name ,destination))
          (system (eval `(lw:defsystem temp () :members ,files))))
     (lw:concatenate-system ,destination system)))

(defsystem :alexandria (:default-pathname "addons/alexandria/")
  (:serial "package"
           "strings"
           "macros"
           "lists"
           "symbols"
           "definitions"
           "functions"
           "types"
           "sequences"
           "control-flow"
           "binding"
           "conditions"
           "hash-tables"
           "io"
           "arrays"
           "numbers"
           "features"))

(defsystem :trivial-features (:default-pathname "addons/trivial-features/src/")
  (:serial #+allegro    "tf-allegro"
           #+lispworks  "tf-lispworks"
           #+sbcl       "tf-sbcl"
           ))

(defsystem :babel (:default-pathname "addons/babel/src/")
  (:serial "packages"
           "encodings"
           "enc-ascii"
           "enc-ebcdic"
           "enc-iso-8859"
           "enc-unicode"
           "external-format"
           "strings"
           "sharp-backslash"))

(defsystem :cffi (:default-pathname "addons/cffi/src/")
  (:serial :alexandria
           :trivial-features
           :babel
           #+sbcl       "cffi-sbcl"
           #+lispworks  "cffi-lispworks"
           #+allegro    "cffi-allegro"
           "package"
           "utils"
           "libraries"
           "early-types"
           "types"
           "enum"
           "strings"
           "functions"
           "foreign-vars"
           "features"))

(defsystem :cl-store (:default-pathname "addons/cl-store/")
  (:serial "package"
           "utils"
           "backends"
           "plumbing"
           "circularities"
           "default-backend"))

#+ignore
(defsystem :metatilities-base (:default-pathname "addons/metatilities-base/dev/")
  (:serial "package"
           "api"
           "l0-utils"
           "l0-strings"
           "l0-macros"
           "l0-arrays"
           "l0-clos"
           "l0-files"
           "l0-time"
           "set-equal"
           "generic-lisp"
           "generic-interface"
           "defclass-star"
           "copy-file"))

#+ignore
(defsystem :cl-containers (:default-pathname "addons/cl-containers/dev/")
  (:serial :metatilities-base
           "package"
           "conditions"
           "container-api"
           "containers"
           "basic-operations"
           "queues"
           "stacks"
           "trees"
           "lists"
           "bags-and-sets"
           "ring-buffers"
           "miscellaneous"
           "associative"
           "compatibility"
           "vectors"
           "quad-tree"
           "heaps"
           "container-mixins"
           "union-find-container"
           "package-container"
           "iterator-api"
           "iterators"
           "file-iterators"))

(defsystem :fork-future (:default-pathname "addons/fork-future/src/")
  (:serial :cl-store
           :cffi 
           "package"
           "simple-queue"
           "posix-wrapper"
           "fork-future"))

(defsystem :future (:default-pathname "addons/future/src/")
  (:serial :alexandria
           "package"
           "thread-api"
           "thread-safe-simple-queue"
           "thread-pool"
           "future"))

(defsystem :iterate (:default-pathname "addons/iterate/")
  (:serial "package"
           "iterate"))

(defsystem :f2cl-lib (:default-pathname "blas/")
  (:serial "package"
           "macro"))

(defsystem :blas-package (:default-pathname "blas/")
  (:serial :f2cl-lib
           "blas-package"))

(defsystem :blas-hompack (:default-pathname "blas/")
  (:serial :blas-package
           "daxpy"
           "dcopy"
           "ddot"
           "dnrm2"
           "dscal"
           "idamax"
           ))

(defsystem :blas-real (:default-pathname "blas/")
  (:serial :blas-package
           "lsame"
           "xerbla"
           "dasum"
           "dcabs1"
           "dgbmv"
           "dgemm"
           "dgemv"
           "dger"
           "drot"
           "drotg"
           "dsbmv"
           "dspmv"
           "dspr"
           "dspr2"
           "dswap"
           "dsymm"
           "dsymv"
           "dsyr"
           "dsyr2"
           "dsyr2k"
           "dsyrk"
           "dtbmv"
           "dtbsv"
           "dtpmv"
           "dtpsv"
           "dtrmm"
           "dtrmv"
           "dtrsm"
           "dtrsv"
           "dzasum"
           "dznrm2"
           "icamax"
           "isamax"
           "izamax"
           ))

(defsystem :blas-complex (:default-pathname "blas/")
  (:serial :blas-package
           "zaxpy"
           "zcopy"
           "zdotc"
           "zdotu"
           "zdscal"
           "zgbmv"
           "zgemm"
           "zgemv"
           "zgerc"
           "zgeru"
           "zhbmv"
           "zhemm"
           "zhemv"
           "zher"
           "zher2"
           "zher2k"
           "zherk"
           "zhpmv"
           "zhpr"
           "zhpr2"
           "zrotg"
           "zscal"
           "zswap"
           "zsymm"
           "zsyr2k"
           "zsyrk"
           "ztbmv"
           "ztbsv"
           "ztpmv"
           "ztpsv"
           "ztrmm"
           "ztrmv"
           "ztrsm"
           "ztrsv"))

(defsystem :blas (:default-pathname "blas/")
  (:serial :f2cl-lib
           :blas-package
           :blas-hompack
           :blas-real
           :blas-complex))

(defsystem :lapack-package (:default-pathname "lapack/")
  (:serial :blas-package
           "lapack-package"))

(defsystem :lapack-real (:default-pathname "lapack/")
  (:serial :lapack-package
           "dlamch"
           "dlapy2"
           "dlartg"
           "dgebak"
           "dlabad"
           "dladiv"
           "dlaln2"
           "dtrevc"
           "dlarfx"
           "dlarfg"
           "dlacpy"
           "dlassq"
           "dlanhs"
           "dlanv2"
           "dlahqr"
           "ieeeck"
           "ilaenv"
           "dlaset"
           "dhseqr"
           "dlarf"
           "dlarft"
           "dlarfb"
           "dorg2r"
           "dorgqr"
           "dorghr"
           "dlahrd"
           "dgehd2"
           "dgehrd"
           "dgebal"
           "dlascl"
           "dlange"
           "dgeev"
           "dlasy2"
           "dlaexc"
           "dtrexc"
           "dlacon"
           "dlaqtr"
           "dtrsna"
           "dtrti2" ; naganuma added
           "dtrtri" ; naganuma added
           "dgeevx"
           "dgetf2"
           "dlaswp"
           "dgetrf"
           "dgetrs"
           "dgetri" ; naganuma added
           "dgesv"
           "dorgl2"
           "dorglq"
           "dgelq2"
           "dgelqf"
           "dorgbr"
           "dorm2r"
           "dormqr"
           "dorml2"
           "dormlq"
           "dormbr"
           "dlasr"
           "dlamrg"
           "dlasd7"
           "dlasd5"
           "dlaed6"
           "dlasd4"
           "dlasd8"
           "dlasd6"
           "dlas2"
           "dlasdt"
           "dlasrt"
           "dlasq4"
           "dlasq5"
           "dlasq6"
           "dlasq3"
           "dlasq2"
           "dlasq1"
           "dlasv2"
           "dbdsqr"
           "dlasdq"
           "dlasda"
           "dlasd2"
           "dlasd3"
           "dlasd1"
           "dlasd0"
           "dlanst"
           "dbdsdc"
           "dlabrd"
           "dgebd2"
           "dgebrd"
           "dgeqr2"
           "dgeqrf"
           "dgesdd"
           "dgesvd"
           ;; For condition numbers of the singular vectors
           "ddisna"
           ))

(defsystem :lapack (:default-pathname "lapack/")
  (:serial :blas
           :lapack-package
           :lapack-real))

(defsystem :mkl-blas (:default-pathname "addons/blas-lapack-ffi/src/")
  (:serial :alexandria
           :cffi
           :iterate
           "packages"
           "preload-for-customized-library"
           "ffi-utils"
           "blas-lapack-common"
           "blas"))

(defsystem :mkl-lapack (:default-pathname "addons/blas-lapack-ffi/src/")
           (:serial :mkl-blas
                    "lapack"))

#-allegro
(defsystem :excl (:default-pathname "allegro/")
  (:serial "excl.cl"))

#-allegro
(defsystem :yacc (:default-pathname "allegro/yacc/")
  (:serial "yacc-defs.cl"
           "yacc-compile.cl"
           "yacc-runtime.cl"))

#-allegro
(defsystem :regexp (:default-pathname "allegro/regexp/")
  (:serial "regexp2-s.cl"
           "package.cl"
           "regexp-parser.cl"
           "regexp-cset.cl"
           "regexp-fe.cl"
           "regexp-vm.cl"
           "regexp-driver.cl"))

(defsystem :statistics (:default-pathname "statistics/")
  (:serial "package"
           "utilities"
           "math"
           "statistics"))

(defsystem :som (:default-pathname "som/")
  (:serial "package"
           "param"
           (:definitions 
               "som_utils"
               "lvq_pak"
             "labels"
             "fileio"
             "datafile"
             "randinit"
             "som_rout"
             "som_pak"
             "vsom"
             "vcal"
             "visual"
             "sammon"
             "test")
           ))

(defsystem :nonparametric (:default-pathname "nonparametric/")
  (:serial "statistics"
	   "dpm"
	   "multi-dpm"
	   "hdp-lda"
	   
	   "hdp"
	   "hdp-hmm"
	   "gauss-hmm"
	   
	   "sticky-hdp-hmm"
	   "blocked-hdp-hmm"
	   "ihmm"))

(defsystem :nearest-search (:default-pathname "nearest-search/")
  (:serial "nearest-search"
	   "k-nn-new"
	   "optics-speed"))

(defsystem :graph-mining (:default-pathname "graph/")
  (:serial "read-graph"
           "utils"
           "shortest-path"
           "centrality"
           "anomaly-detection"
           ))

(defsystem :ml-test (:default-pathname "test/")
  (:serial "package"
           "test-utils"
           "test-read-data"
           "test-missing-value"
           "test-spectral-clustering"
           "test-linear-regression"
           "test-assoc" 
           "test-cluster-validation"
           "test-expl-smthing"
           "test-hc"
           "test-decision-tree"
           "test-random-forest"
           "test-k-means"
           "test-k-nn"
           "test-nmf"
           "test-optics"
           "test-pca"
           "test-som"
           "test-stat"
           "test-svm"
           "test-ts-ar"
           "test-ts-read-data"
           "test-ts-stat"
           "test-ts-stsp"   
           "test-classifier"
           "test-matrix"
           "test-text-utils"
           "test-smo-svm"
           "test-wss3-svm"
           "test-svr"
           "test-one-class-svm"
           "test-hdp-lda"
           "test-dpm"
           ))
  
(defsystem :machine-learning (:default-pathname ".")
  (:serial #-allegro :excl
           #-allegro :yacc
           #-allegro :regexp
           :lapack
           #+mkl :mkl-blas
           #+mkl :mkl-lapack
           :statistics
           :som
           :iterate
           #+fork-future :fork-future
           #+future :future
           "lisp-unit"
           "parse-number"
           "csv"
           "utils"
           "vars"
           "vector"
           "matrix"
           "eigenproblem"
           "priority-que"
           "missing-value"
           "fft"
           "read-data"
           "text-utils"
           "pca"
           "k-means"
           "cluster-validation"
           "optics"
           "association-rule"
           "assoc-da"                   ; another association-rule by FUJII
           "fp-growth"                  ; FP-growth association-rule by FUJII
           "eclat"                      ; Eclat association-rule by FUJII
           "lcm"                        ; LCM association-rule by FUJII
           "svm"
           "k-nn"
           "spectral-clustering"
           "hc"
           "linear-regression"
           "nmf"
           "decision-tree"
           "random-forest"
           "ts-read-data"
           "ts-util"
           "ts-stat" 
           "ts-state-space-model"
           "ts-ar"
           "exponential-smoothing"
           "face-recognition"
           "smo-svm"
           "wss3-svm"
           "svr"
           "one-class-svm"
           :nonparametric
           "hdp-lda"
           "dpm"
           :graph-mining
	   :nearest-search
           :ml-test
           ))


(eval-when (:load-toplevel :execute)
  (format t "~%To build, execute this:~%(load-system :machine-learning :compile t :module-keys '(:external-format #+allegro :932 #-allegro :sjis))~%"))

