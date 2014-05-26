(defsystem :som (:default-pathname #.(translate-logical-pathname "./"))
  (:serial "package"
	   "param"
	   (:definitions "som_utils"
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
	     "test"
	   )))

(load-system :som :compile t)
