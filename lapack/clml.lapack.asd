

(setq *read-default-float-format* 'double-float)
(asdf:defsystem :clml.lapack-package 
                :pathname "src/"
                :serial :clml.blas-package
                :components (
                             (:file "package")))

(asdf:defsystem :clml.lapack-real
                :pathname "src/"
                :serial :clml.lapack-package
                :components (
                             (:file "dlamch")
                             (:file "dlapy2")
                             (:file "dlartg")
                             (:file "dgebak")
                             (:file "dlabad")
                             (:file "dladiv")
                             (:file "dlaln2")
                             (:file "dtrevc")
                             (:file "dlarfx")
                             (:file "dlarfg")
                             (:file "dlacpy")
                             (:file "dlassq")
                             (:file "dlanhs")
                             (:file "dlanv2")
                             (:file "dlahqr")
                             (:file "ieeeck")
                             (:file "ilaenv")
                             (:file "dlaset")
                             (:file "dhseqr")
                             (:file "dlarf")
                             (:file "dlarft")
                             (:file "dlarfb")
                             (:file "dorg2r")
                             (:file "dorgqr")
                             (:file "dorghr")
                             (:file "dlahrd")
                             (:file "dgehd2")
                             (:file "dgehrd")
                             (:file "dgebal")
                             (:file "dlascl")
                             (:file "dlange")
                             (:file "dgeev")
                             (:file "dlasy2")
                             (:file "dlaexc")
                             (:file "dtrexc")
                             (:file "dlacon")
                             (:file "dlaqtr")
                             (:file "dtrsna")
                             (:file "dtrti2") ; naganuma added
                             (:file "dtrtri") ; naganuma added
                             (:file "dgeevx")
                             (:file "dgetf2")
                             (:file "dlaswp")
                             (:file "dgetrf")
                             (:file "dgetrs")
                             (:file "dgetri") ; naganuma added
                             (:file "dgesv")
                             (:file "dorgl2")
                             (:file "dorglq")
                             (:file "dgelq2")
                             (:file "dgelqf")
                             (:file "dorgbr")
                             (:file "dorm2r")
                             (:file "dormqr")
                             (:file "dorml2")
                             (:file "dormlq")
                             (:file "dormbr")
                             (:file "dlasr")
                             (:file "dlamrg")
                             (:file "dlasd7")
                             (:file "dlasd5")
                             (:file "dlaed6")
                             (:file "dlasd4")
                             (:file "dlasd8")
                             (:file "dlasd6")
                             (:file "dlas2")
                             (:file "dlasdt")
                             (:file "dlasrt")
                             (:file "dlasq4")
                             (:file "dlasq5")
                             (:file "dlasq6")
                             (:file "dlasq3")
                             (:file "dlasq2")
                             (:file "dlasq1")
                             (:file "dlasv2")
                             (:file "dbdsqr")
                             (:file "dlasdq")
                             (:file "dlasda")
                             (:file "dlasd2")
                             (:file "dlasd3")
                             (:file "dlasd1")
                             (:file "dlasd0")
                             (:file "dlanst")
                             (:file "dbdsdc")
                             (:file "dlabrd")
                             (:file "dgebd2")
                             (:file "dgebrd")
                             (:file "dgeqr2")
                             (:file "dgeqrf")
                             (:file "dgesdd")
                             (:file "dgesvd")
                             ;; For condition numbers of the singular vectors
                             (:file "ddisna")
           
                             )
  )
(asdf:defsystem :clml.lapack
                  :pathname "src/"
                  :serial :clml.blas
                  :depends-on (
                               :f2cl
                               :clml.blas
                               :clml.lapack-package
                               :clml.lapack-real))
