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
                                        #- ccl
                                        test-hc ; memory access error in ccl
                                        test-sample-k-means
                                        ))

(defparameter *nearest-search* '(test-sample-k-nn))

(defparameter *time-series-tests* '(
                                    test-sample-ts-ar
                                    test-ts-burst-detection
                                    test-sample-ts-read-data
                                    test-sample-ts-stat
                                    test-sample-ts-stsp
                                    test-sample-expl-smthing
                                    test-ts-anomaly-detection-data
                                    test-ts-anomaly-detection-snn
                                    ;test-ts-anomaly-detection-periodic-detector
                                    ;test-ts-anomaly-detection-eec-detector
                                    ;test-ts-anomoly-detection-db-detector
                                    test-changefinder))


(defparameter *svm-tests* '(test-sample-svm
                            svm.smo.kernels
                            smo.svm
                            test-pwss3-kernels
                            ;test-pwss3-svm
                            test-wss3-kernels
                            test-wss3-svm
                            test-one-class-svm
                            test-svr))

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
                            ; test-ps error
                            test-fill-rem
                            ; test-spline error
                            ))

(defparameter *util-tests* '(parse-arff))

(defparameter *all-tests* (append *statistics-tests* *decision-tree-tests* *clustering-tests*
                                  *time-series-tests* *svm-tests* *classifiers-tests*
                                  ; *association-rule-tests* error result key is cons not string
                                  *som-tests* *text-tests*
                                  ; *pca-tests* takes too long
                                  *hjs-tests*
                                  *nonparametric-tests*
                                  *nearest-search*
                                  *util-tests*))
#| #'test-sample-assoc #'test-sample-cluster-validation
             #'test-decision-tree #'test-sample-expl-smthing
             #'test-hc #'test-sample-k-means
             #'test-sample-k-nn #'test-sample-linear-regression
             #'test-nmf #'test-sample-optics
             #'test-sample-pca #'test-random-forest
             #'test-sample-read-data #'test-sample-som
             #'test-sample-spectral-clustering #'test-sample-stat
             #'test-sample-svm
             #'test-wss3.kernels
             #'test-wss3.svm test-svr
             #'test-test-one-class-svm
             #'test-sample-ts-ar test-sample-ts-read-data test-sample-ts-stat
             #'test-sample-ts-stsp
             #'test-changefinder
             #'test-ts-anomaly-detection
             #'test-sample-hdp-lda
             #'test-sample-dpm
#'test-sample-text-utils
|#
