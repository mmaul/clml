( eval-when (:compile-toplevel)
  (setq *read-default-float-format* 'double-float))
(defpackage :clml.lapack
  (:use :cl :f2cl-lib :clml.blas)
  (:shadow :zswap)
  (:export
   :dbdsdc :dbdsqr :ddisna :dgebak :dgebal :dgebd2 :dgebrd :dgeev :dgeevx
   :dgehd2 :dgehrd :dgelq2 :dgelqf :dgeqr2 :dgeqrf :dgesdd :dgesvd :dgesv
   :dgetf2 :dgetrf :dgetri :dgetrs :dhseqr :dlabad :dlabrd :dlacon :dlacpy
   :dladiv :dlaed6 :dlaexc :dlahqr :dlahrd :dlaln2 :dlamc3 :dlamc1 :dlamc4
   :dlamc5 :dlamc2 :dlamch :dlamrg :dlange :dlanhs :dlanst :dlanv2 :dlapy2
   :dlaqtr :dlarfb :dlarfg :dlarf  :dlarft :dlarfx :dlartg :dlas2  :dlascl
   :dlasd0 :dlasd1 :dlasd2 :dlasd3 :dlasd4 :dlasd5 :dlasd6 :dlasd7 :dlasd8
   :dlasda :dlasdq :dlasdt :dlaset :dlasq1 :dlasq2 :dlasq3 :dlasq4 :dlasq5
   :dlasq6 :dlasr  :dlasrt :dlassq :dlasv2 :dlaswp :dlasy2 :dorg2r :dorgbr
   :dorghr :dorgl2 :dorglq :dorgqr :dorm2r :dormbr :dorml2 :dormlq :dormqr
   :dtrevc :dtrexc :dtrsna :dtrti2 :dtrtri :ieeeck :ilaenv)
)
