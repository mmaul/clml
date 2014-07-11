(in-package :test)

(defparameter *statistics-group* '(
                                   test-sample-stat
                                   ))

(defparameter *decision-tree-group* '(
                                      test-decision-tree
                                      test-random-forest
                                      ))

(defparameter *clustering-test-group* '(
                                        test-sample-cluster-validation
                                        test-sample-spectral-clustering
                                        test-sample-w
                                        test-random-w
                                        test-sample-optics
                                        test-nmf
                                        test-hc
                                        test-sample-kmeans
                                        ))


(defparameter *time-series-test-group* '(
                                         test-sample-ts-ar
                                         test-ts-burst-detection
                                         test-sample-ts-read-data
                                         test-sample-ts-stat
                                         test-sample-ts-stsp
                                         test-sample-expl-smthing
                                         ; disabled tests due to blass:dgemm not handling multi-dimensional arrays
                                         ;test-ts-anomaly-detection
                                         ;test-changefinder
                                         ))


(defparameter *svm-test-group* '(test-sample-svm
                                 svm.smo.kernels
                                 smo.svm
                                 test-pwss3-kernels
                                 test-pwss3-svm
                                 test-wss3-kernels
                                 test-wss3-svm
                                 test-one-class-svm
                                 test-svr
                                 ))

(defparameter *classifiers-test-group* '(test-sample-linear-regression
                                         simple-linear-regression-case
                                         multi-linear-regression-case
                                         test-nbayes))

(defparameter *association-rule-test-group* '(test-sample-assoc))

(defparameter *nonparametric-test-group* '(
                                        ;test-sample-dpm ; gaussian-dpm does not exist
                                        ;test-sample-hdp-lda
                                           ))
(defparameter *som-test-group* '(test-sample-som))

(defparameter *text-test-group* '(test-sample-text-utils))

(defparameter *pca-group* '(test-sample-pca))

(defparameter *hjs-group* '(matrix-vecs-conversion-test
                            matrix-transpose-test
                            matrix-m*m-test
                            test-sample-read-data
                            test-ps
                            test-fill-rem
                            test-spline))
