(in-package :clml.test)

(defparameter *statistics-tests* '(
                                   test-sample-stat
                                   ))

(defparameter *decision-tree-tests* '(
                                      test-decision-tree
                                      test-random-forest
                                      ))

(defparameter *clustering-tests* '(
                                        test-sample-cluster-validation
                                        test-sample-spectral-clustering
                                        test-sample-w
                                        test-random-w
                                        test-sample-optics
                                        test-nmf
                                        test-hc
                                        test-sample-kmeans
                                        ))


(defparameter *time-series-tests* '(
                                         test-sample-ts-ar
                                         test-ts-burst-detection
                                         test-sample-ts-read-data
                                         test-sample-ts-stat
                                         test-sample-ts-stsp
                                         test-sample-expl-smthing
                                         ; disabled tests due to blass:dgemm not handling multi-dimensional arrays
                                         ;test-ts-anomaly-detection
                                         test-changefinder
                                         ))


(defparameter *svm-tests* '(test-sample-svm
                                 svm.smo.kernels
                                 smo.svm
                                 test-pwss3-kernels
                                 test-pwss3-svm
                                 test-wss3-kernels
                                 test-wss3-svm
                                 test-one-class-svm
                                 test-svr
                                 ))

(defparameter *classifiers-tests* '(test-sample-linear-regression
                                         simple-linear-regression-case
                                         multi-linear-regression-case
                                         test-nbayes))

(defparameter *association-rule-tests* '(test-sample-assoc))

(defparameter *nonparametric-tests* '(
                                        ;test-sample-dpm ; gaussian-dpm does not exist
                                        ;test-sample-hdp-lda
                                           ))
(defparameter *som-tests* '(test-sample-som))

(defparameter *text-tests* '(test-sample-text-utils))

(defparameter *pca-tests* '(test-sample-pca))

(defparameter *hjs-tests* '(matrix-vecs-conversion-test
                            matrix-transpose-test
                            matrix-m*m-test
                            test-sample-read-data
                            test-ps
                            test-fill-rem
                            test-spline))
(defparameter *all-tests* (append *statistics-tests* *decision-tree-tests* *clustering-tests*
                                  *time-series-tests* *svm-tests* *classifiers-tests*
                                  *association-rule-tests* *nonparametric-tests*
                                  *som-tests* *text-tests* *pca-tests* *hjs-tests*))
