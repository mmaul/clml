(defsystem :nonparametric (:default-pathname ".")
  (:serial "statistics"
	   "gamma"
	   "dpm"
	   "multi-dpm"
	   "hdp-lda"
	   
	   "hdp"
	   "hdp-hmm"
	   "gauss-hmm"
	   
	   "sticky-hdp-hmm"
	   "blocked-hdp-hmm"
	   "ihmm"))

(eval-when (load eval)
  (format t "~%To build, execute this:~%(excl:load-system :nonparametric :compile t)~%"))