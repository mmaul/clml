(in-package :mkl.lapack)



;;;;;; Driver routines

;;;;; LAPACK routines: Linear Equations

;;; gesv
(deflapack gesv (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

(defffun dsgesv :void
  (n blas-int)
  (nrhs blas-int)
  (a (:array :double * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :double * *) :in-out)
  (ldb blas-int)
  (x (:array :double * *) :in-out)
  (ldx blas-int)
  (work (:array :double *))
  (swork (:array :single *))
  (iter blas-int :out)
  (info blas-int :out))
(export 'dsgesv)

(defffun zcgesv :void
  (n blas-int)
  (nrhs blas-int)
  (a (:array complex-double * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array complex-double * *) :in-out)
  (ldb blas-int)
  (x (:array complex-double * *) :in-out)
  (ldx blas-int)
  (work (:array complex-double *))
  (swork (:array complex-float *))
  (iter blas-int :out)
  (info blas-int :out))
(export 'zcgesv)

;;; gesvx
(deflapack gesvx (:single :double) :void
  (fact :string)
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (af (:array :precision * *) :in-out)
  (ldaf blas-int)
  (ipiv (:array blas-int *) :in-out)
  (equed :string :in-out)
  (r (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (c (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *) :in-out)
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack gesvx (:complex-single :complex-double) :void
  (fact :string)
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (af (:array :precision * *) :in-out)
  (ldaf blas-int)
  (ipiv (:array blas-int *) :in-out)
  (equed :string :in-out)
  (r (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (c (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *) :in-out)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (info blas-int :out))

;;; gbsv
(deflapack gbsv (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; gbsvx

;;; gtsv
(deflapack gtsv (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (nrhs blas-int)
  (dl (:array :precision *) :in-out)
  (d (:array :precision *) :in-out)
  (du (:array :precision *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; gtsvx

;;; posv
(deflapack posv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; posvx

;;; ppsv
(deflapack ppsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; ppsvx

;;; pbsv
(deflapack pbsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; pbsvx

;;; ptsv
(deflapack ptsv (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (nrhs blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (:array :precision *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; ptsvx

;;; sysv
(deflapack sysv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; sysvx

;;; hesv
(deflapack hesv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; hesvx

;;; spsv
(deflapack spsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; spsvx

;;; hpsv
(deflapack hpsv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))


;;;;; LAPACK routines: Least square and eigenvalue problems

;;;; routines for linear least squares (LLS) problems

;;; gels
(deflapack gels (:single :double :complex-single :complex-double) :void
  (trans :string)
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; gelsy
(deflapack gelsy (:single :double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (jpvt (:array blas-int *) :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *))
  (lwork blas-int)
  (info blas-int :out))

(deflapack gelsy (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (jpvt (:array blas-int *) :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *))
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; gelss
(deflapack gelss (:single :double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack gelss (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; gelsd
(deflapack gelsd (:single :double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (info blas-int :out))

(deflapack gelsd (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rank blas-int :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (iwork (:array blas-int *) :in-out)
  (info blas-int :out))


;;;; routines for generalized LLS problems

;;; gglse
(deflapack gglse (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (p blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (c (:array :precision *) :in-out)
  (d (:array :precision *) :in-out)
  (x (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ggglm
(deflapack ggglm (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (m blas-int)
  (p blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (d (:array :precision *) :in-out)
  (x (:array :precision *) :in-out)
  (y (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))


;;;; routines for symmetric eigenproblems

;;; syev
(deflapack syev (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; heev
(deflapack heev (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; syevd
(deflapack syevd (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; heevd
(deflapack heevd (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; syevx
(deflapack syevx (:single :double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; heevx
(deflapack heevx (:complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; syevr
(deflapack syevr (:single :double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (isuppz (:array blas-int *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; heevr
(deflapack heevr (:complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (isuppz (:array blas-int *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; spev
(deflapack spev (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hpev
(deflapack hpev (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; spevd
(deflapack spevd (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; hpevd
(deflapack hpevd (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; spevx
(deflapack spevx (:single :double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hpevx
(deflapack hpevx (:complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; sbev
(deflapack sbev (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hbev
(deflapack hbev (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; sbevd
(deflapack sbevd (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; hbevd
(deflapack hbevd (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; sbevx
(deflapack sbevx (:single :double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hbevx
(deflapack hbevx (:complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; stev
(deflapack stev (:single :double) :void
  (jobz :string)
  (n blas-int)
  (d (:array :precision *) :in-out)
  (e (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; stevd
(deflapack stevd (:single :double) :void
  (jobz :string)
  (n blas-int)
  (d (:array :precision *) :in-out)
  (e (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; stevx
(deflapack stevx (:single :double) :void
  (jobz :string)
  (range :string)
  (n blas-int)
  (d (:array :precision *) :in-out)
  (e (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; stevr
(deflapack stevr (:single :double) :void
  (jobz :string)
  (range :string)
  (n blas-int)
  (d (:array :precision *) :in-out)
  (e (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (isuppz (:array blas-int *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))


;;;; routines for nonsymmetric eigenproblems

;;; TODO: gees, geesx

;;; geev
(deflapack geev (:single :double) :void
  (jobvl :string)
  (jobvr :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (wr (:array :precision *) :in-out)
  (wi (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack geev (:complex-single :complex-double) :void
  (jobvl :string)
  (jobvr :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; geevx
(deflapack geevx (:single :double) :void
  (balanc :string)
  (jobvl :string)
  (jobvr :string)
  (sense :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (wr (:array :precision *) :in-out)
  (wi (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (scale (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (abnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (rconde (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rcondv (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack geevx (:complex-single :complex-double) :void
  (balanc :string)
  (jobvl :string)
  (jobvr :string)
  (sense :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (w (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (scale (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (abnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (rconde (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rcondv (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))


;;;; routines for singular value decomposition

;;; gesvd
(deflapack gesvd (:single :double) :void
  (jobu :string)
  (jobvt :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack gesvd (:complex-single :complex-double) :void
  (jobu :string)
  (jobvt :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; gesdd
(deflapack gesdd (:single :double) :void
  (jobz :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack gesdd (:complex-single :complex-double) :void
  (jobz :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (info blas-int :out))

;;; ggsvd
(deflapack ggsvd (:single :double) :void
  (jobu :string)
  (jobv :string)
  (jobq :string)
  (m blas-int)
  (n blas-int)
  (p blas-int)
  (k blas-int :out)
  (l blas-int :out)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alpha (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (beta (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *) :in-out)
  (info blas-int :out))

(deflapack ggsvd (:complex-single :complex-double) :void
  (jobu :string)
  (jobv :string)
  (jobq :string)
  (m blas-int)
  (n blas-int)
  (p blas-int)
  (k blas-int :out)
  (l blas-int :out)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alpha (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (beta (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *) :in-out)
  (info blas-int :out))

;;;; routines generalized symmetric definite eigenproblems

;;; sygv
(deflapack sygv (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (w (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; hegv
(deflapack hegv (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; sygvd
(deflapack sygvd (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; hegvd
(deflapack hegvd (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; sygvx
(deflapack sygvx (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (vl (:array :precision * *))
  (vu (:array :precision * *))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hegvx
(deflapack hegvx (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (vl (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (vu (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; spgv
(deflapack spgv (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hpgv
(deflapack hpgv (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; spgvd
(deflapack spgvd (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; hpgvd
(deflapack hpgvd (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; spgvx
(deflapack spgvx (:single :double) :void
  (itype blas-int)
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (vu (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hpgvx
(deflapack hpgvx (:complex-single :complex-double) :void
  (itype blas-int)
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *) :in-out)
  (vl (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (vu (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; sbgv
(deflapack sbgv (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hbgv
(deflapack hbgv (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; sbgvd
(deflapack sbgvd (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; hbgvd
(deflapack hbgvd (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; sbgvx
(deflapack sbgvx (:single :double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (vl (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (vu (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hbgvx
(deflapack hbgvx (:complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (vl (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (vu (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifail (:array blas-int *) :in-out)
  (info blas-int :out))


;;;; routines for generalized nonsymmetric eigenproblems

;;; TODO: gges ggesx

;;; ggev
(deflapack ggev (:single :double) :void
  (jobvl :string)
  (jobvr :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alphar (:array :precision * *) :in-out)
  (alphai (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack ggev (:complex-single :complex-double) :void
  (jobvl :string)
  (jobvr :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alpha (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; ggevx
(deflapack ggevx (:single :double) :void
  (balanc :string)
  (jobvl :string)
  (jobvr :string)
  (sense :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alphar (:array :precision * *) :in-out)
  (alphai (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (lscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (abnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (bbnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (rconde (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rcondv (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (bwork (:array :boolean *))
  (info blas-int :out))

(deflapack ggevx (:complex-single :complex-double) :void
  (balanc :string)
  (jobvl :string)
  (jobvr :string)
  (sense :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alpha (:array :precision * *) :in-out)
  (beta (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (lscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (abnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (bbnrm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (rconde (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rcondv (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (bwork (:array :boolean *))
  (info blas-int :out))



;;;;;; Computational routines

;;;;; LAPACK routines: Linear Equations

;;;; routine for matrix factorization

;;; getrf
(deflapack getrf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (info blas-int :out))

;;; gbtrf
(deflapack gbtrf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (ipiv (:array blas-int *) :in-out)
  (info blas-int :out))

;;; gttrf
(deflapack gttrf (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (dl (:array :precision *) :in-out)
  (d (:array :precision *) :in-out)
  (du (:array :precision *) :in-out)
  (du2 (:array :precision *) :in-out)
  (ipiv (:array blas-int *) :in-out)
  (info blas-int :out))

;;; potrf
(deflapack potrf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (info blas-int :out))

;;; pptrf 
(deflapack pptrf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (info blas-int :out))

;;; pbtrf
(deflapack pbtrf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (info blas-int :out))

;;; pttrf
(deflapack pttrf (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e :precision :in-out)
  (info blas-int :out))

;;; sytrf
(deflapack sytrf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; hetrf
(deflapack hetrf (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; sptrf
(deflapack sptrf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *) :in-out)
  (info blas-int :out))

;;; hptrf
(deflapack hptrf (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *) :in-out)
  (info blas-int :out))


;;;; routines for solving systems of linear equations

;;; getrs
(deflapack getrs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; gbtrs
(deflapack gbtrs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; gttrs
(deflapack gttrs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (dl (:array :precision *))
  (d (:array :precision *))
  (du (:array :precision *))
  (du2 (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; potrs
(deflapack potrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; pptrs
(deflapack pptrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; pbtrs
(deflapack pbtrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; pttrs
(deflapack pttrs (:single :double) :void
  (n blas-int)
  (nrhs blas-int)
  (d (:array :precision *))
  (e (:array :precision *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

(deflapack pttrs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (:array :precision *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; sytrs
(deflapack sytrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out)) 

;;; hetrs
(deflapack hetrs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; sptrs
(deflapack sptrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; hptrs
(deflapack hptrs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; trtrs
(deflapack trtrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; tptrs
(deflapack tptrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))

;;; tbtrs
(deflapack tbtrs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (kd blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (info blas-int :out))


;;;; routine for estimating the condition number

;;; gecon
(deflapack gecon (:single :double :complex-single :complex-double) :void
  (norm :string)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array blas-int *))))
  (info blas-int :out))

;;; gbcon
(deflapack gbcon (:single :double :complex-single :complex-double) :void
  (norm :string)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array blas-int *))))
  (info blas-int :out)) 

;;; gtcon
(deflapack gtcon (:single :double) :void
  (norm :string)
  (n blas-int)
  (dl (:array :precision *))
  (d (:array :precision *))
  (du (:array :precision *))
  (du2 (:array :precision *))
  (ipiv (:array blas-int *))
  (anorm :precision)
  (rcond :precision :out)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack gtcon (:complex-single :complex-double) :void
  (norm :string)
  (n blas-int)
  (dl (:array :precision *))
  (d (:array :precision *))
  (du (:array :precision *))
  (du2 (:array :precision *))
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (info blas-int :out))

;;; pocon
(deflapack pocon (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; ppcon
(deflapack ppcon (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; pbcon
(deflapack pbcon (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; ptcon
(deflapack ptcon (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (:array :precision *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (info blas-int :out))

;;; sycon
(deflapack sycon (:single :double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *))
  (ldab blas-int)
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack sycon (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *))
  (ldab blas-int)
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (info blas-int :out))

;;; hecon
(deflapack hecon (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *))
  (ldab blas-int)
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (info blas-int :out))

;;; spcon
(deflapack spcon (:single :double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (info blas-int :out))

(deflapack spcon (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (info blas-int :out))

;;; hpcon
(deflapack hpcon (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (ipiv (:array blas-int *))
  (anorm (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision)))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (info blas-int :out))

;;; trcon
(deflapack trcon (:single :double :complex-single :complex-double) :void
  (norm :string)
  (uplo :string)
  (diag :string)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; tpcon
(deflapack tpcon (:single :double :complex-single :complex-double) :void
  (norm :string)
  (uplo :string)
  (diag :string)
  (n blas-int)
  (ap (:array :precision *))
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; tbcon
(deflapack tbcon (:single :double :complex-single :complex-double) :void
  (norm :string)
  (uplo :string)
  (diag :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (rcond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))


;;;; refining solutions and estimating its error
(deflapack gerfs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (ldab blas-int)
  (af (:array :precision * *))
  (ldaf blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out)) 

;;; gbrfs
(deflapack gbrfs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (afb (:array :precision * *))
  (ldafb blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; gtrfs
(deflapack gtrfs (:single :double :complex-single :complex-double) :void
  (trans :string)
  (n blas-int)
  (nrhs blas-int)
  (dl (:array :precision *))
  (d (:array :precision *))
  (du (:array :precision *))
  (dlf (:array :precision *))
  (df (:array :precision *))
  (duf (:array :precision *))
  (du2 (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; porfs
(deflapack porfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (af (:array :precision * *))
  (ldaf blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; pprfs
(deflapack pprfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (afp (:array :precision *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (info blas-int :out))

;;; pbrfs
(deflapack pbrfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (afb (:array :precision * *))
  (ldafb blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; ptrfs
(deflapack ptrfs (:single :double) :void
  (n blas-int)
  (nrhs blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (:array :precision *))
  (df (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (ef (:array :precision *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (info blas-int :out))

(deflapack ptrfs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (:array :precision *))
  (df (case :precision
	(:complex-single '(:array :single *))
	(:complex-double '(:array :double *))
	(t '(:array :precision *))))
  (ef (:array :precision *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; syrfs
(deflapack syrfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (af (:array :precision * *))
  (ldaf blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; herfs
(deflapack herfs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (af (:array :precision * *))
  (ldaf blas-int)
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; sprfs
(deflapack sprfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (afp (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; hprfs
(deflapack hprfs (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (afp (:array :precision *))
  (ipiv (:array blas-int *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; trrfs
(deflapack trrfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (nrhs blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *))
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; tprfs
(deflapack tprfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (nrhs blas-int)
  (ap (:array :precision *))
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *))
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; tbrfs
(deflapack tbrfs (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (kd blas-int)
  (nrhs blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (x (:array :precision * *))
  (ldx blas-int)
  (ferr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (berr (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (work (:array :precision *))
  (i/rwork (case :precision
	     (:complex-single '(:array :single *))
	     (:complex-double '(:array :double *))
	     (t '(:array blas-int *))))
  (info blas-int :out))


;;;; routine for matrix inversion

;;; getri
(deflapack getri (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *))
  (work (:array :precision *))
  (lwork blas-int)
  (info blas-int :out))

;;; potri
(deflapack potri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (info blas-int :out))

;;; pptri
(deflapack pptri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (info blas-int :out))

;;; sytri
(deflapack sytri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *))
  (work (:array :precision *))
  (info blas-int :out))

;;; hetri
(deflapack hetri (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ipiv (:array blas-int *))
  (work (:array :precision *))
  (info blas-int :out))

;;; sptri
(deflapack sptri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *))
  (work (:array :precision *))
  (info blas-int :out))

;;; hptri
(deflapack hptri (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (ipiv (:array blas-int *))
  (work (:array :precision *))
  (info blas-int :out))

;;; trtri
(deflapack trtri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (diag :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (info blas-int :out))

;;; tptri
(deflapack tptri (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (diag :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (info blas-int :out))


;;;; routines for matrix equilibration

;;; geequ
(deflapack geequ (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (r (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (c (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rowcnd (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision))
	  :out)
  (colcnd (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision))
	  :out)
  (amax (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision))
	:out)
  (info blas-int :out))

;;; gbequ
(deflapack gbequ (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (r (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (c (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (rowcnd (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision))
	  :out)
  (colcnd (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision))
	  :out)
  (amax (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision))
	:out)
  (info blas-int :out))

;;; poequ
(deflapack poequ (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (scond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (amax (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision))
	:out)
  (info blas-int :out)) 

;;; ppequ
(deflapack ppequ (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (scond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (amax (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision))
	:out)
  (info blas-int :out))

;;; pbequ
(deflapack pbequ (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *))
  (ldab blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (scond (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (amax (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision))
	:out)
  (info blas-int :out))

 

;;;;; LAPACK routines: Least square and eigenvalue problems

;;;; routines for orthogonal factorizations

;;; geqrf
(deflapack geqrf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; geqpf
(deflapack geqpf (:single :double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (jpvt (:array blas-int *) :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *))
  (info blas-int :out))

(deflapack geqpf (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (jpvt (:array blas-int *) :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; geqp3
(deflapack geqp3 (:single :double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (jpvt (:array blas-int *) :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *))
  (lwork blas-int)
  (info blas-int :out))

(deflapack geqp3 (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (jpvt (:array blas-int *) :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (lwork blas-int)
  (info blas-int :out))

;;; orgqr
(deflapack orgqr (:single :double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormqr
(deflapack ormqr (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ungqr
(deflapack ungqr (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmqr
(deflapack unmqr (:complex-single :complex-double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; gelqf
(deflapack gelqf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; orglq
(deflapack orglq (:single :double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormlq
(deflapack ormlq (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unglq
(deflapack unglq (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmlq
(deflapack unmlq (:complex-single :complex-double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; geqlf
(deflapack geqlf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; orgql
(deflapack orgql (:single :double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ungql
(deflapack ungql (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormql
(deflapack ormql (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmql
(deflapack unmql (:complex-single :complex-double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; gerqf
(deflapack gerqf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; orgrq
(deflapack orgrq (:single :double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ungrq
(deflapack ungrq (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormrq
(deflapack ormrq (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmrq
(deflapack unmrq (:complex-single :complex-double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; tzrzf
(deflapack tzrzf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormrz
(deflapack ormrz (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (l blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ggqrf
(deflapack ggqrf (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (m blas-int)
  (p blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (taua (:array :precision *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (taub (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ggrqf
(deflapack ggrqf (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (p blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (taua (:array :precision *) :in-out)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (taub (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))


;;;; routines for singular value decomposition

;;; gebrd
(deflapack gebrd (:single :double :complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tauq (:array :precision *) :in-out)
  (taup (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; gbbrd
(deflapack gbbrd (:single :double) :void
  (vect :string)
  (m blas-int)
  (n blas-int)
  (ncc blas-int)
  (kl blas-int)
  (ku blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (pt (:array :precision * *) :in-out)
  (ldpt blas-int)
  (c (:array :precision * *))
  (ldc blas-int)
  (work (:array :precision *)) 
  (info blas-int :out))

(deflapack gbbrd (:complex-single :complex-double) :void
  (vect :string)
  (m blas-int)
  (n blas-int)
  (ncc blas-int)
  (kl blas-int)
  (ku blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (pt (:array :precision * *) :in-out)
  (ldpt blas-int)
  (c (:array :precision * *))
  (ldc blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; orgbr
(deflapack orgbr (:single :double) :void
  (vect :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormbr
(deflapack ormbr (:single :double) :void
  (vect :string)
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ungbr
(deflapack ungbr (:complex-single :complex-double) :void
  (vect :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmbr
(deflapack unmbr (:complex-single :complex-double) :void
  (vect :string)
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; bdsqr
(deflapack bdsqr (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ncvt blas-int)
  (nru blas-int)
  (ncc blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (lwork blas-int)
  (info blas-int :out))

;;; bdsdc
(deflapack bdsdc (:single :double) :void
  (uplo :string)
  (compq :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (vt (:array :precision * *) :in-out)
  (ldvt blas-int)
  (q (:array :precision * *) :in-out)
  (iq (:array blas-int *) :in-out)
  (work (:array :precision *))
  (iwork (:array blas-int *))
  (info blas-int :out))


;;;; routines for symmetric eigenvalue problems

;;; sytrd
(deflapack sytrd (:single :double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; syrdb
#+nil
(deflapack syrdb (:single :double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tau (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; herdb
#+nil
(deflapack herdb (:complex-single :complex-double) :void
  (jobz :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (d (:array :precision *) :in-out)
  (e (:array :precision *) :in-out)
  (tau (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; orgtr
(deflapack orgtr (:single :double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormtr
(deflapack ormtr (:single :double) :void
  (side :string)
  (uplo :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; hetrd
(deflapack hetrd (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ungtr
(deflapack ungtr (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmtr
(deflapack unmtr (:complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; sptrd
(deflapack sptrd (:single :double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tau (:array :precision *) :in-out)
  (info blas-int :out))

;;; opgtr
(deflapack opgtr (:single :double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (tau (:array :precision *))
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; opmtr
(deflapack opmtr (:single :double) :void
  (side :string)
  (uplo :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (ap (:array :precision *))
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hptrd
(deflapack hptrd (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (tau (:array :precision *) :in-out)
  (info blas-int :out))

;;; upgtr
(deflapack upgtr (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *))
  (tau (:array :precision *))
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; upmtr
(deflapack upmtr (:complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (ap (:array :precision *))
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; sbtrd
(deflapack sbtrd (:single :double) :void
  (vect :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hbtrd
(deflapack hbtrd (:complex-single :complex-double) :void
  (vect :string)
  (uplo :string)
  (n blas-int)
  (kd blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; sterf
(deflapack sterf (:single :double) :void
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (info blas-int :out))

;;; steqr
(deflapack steqr (:single :double :complex-single :complex-double) :void
  (compz :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (info blas-int :out))

;;; stemr
;;; TODO: test tryrac, it's a boolean parameter
#+nil
(deflapack stemr (:single :double :complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (nzc blas-int)
  (isuppz (:array blas-int *) :in-out)
  (tryrac :boolean :in-out)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; stedc
(deflapack stedc (:single :double) :void
  (compz :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision * *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

(deflapack stedc (:complex-single :complex-double) :void
  (compz :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision * *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (lrwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; stegr
(deflapack stegr (:single :double :complex-single :complex-double) :void
  (jobz :string)
  (range :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (m blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (isuppz (:array blas-int *) :in-out)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; pteqr
(deflapack pteqr (:single :double :complex-single :complex-double) :void
  (compz :string)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (info blas-int :out))

;;; stebz
(deflapack stebz (:single :double) :void
  (range :string)
  (order :string)
  (n blas-int)
  (vl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (vu (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision)))
  (il blas-int)
  (iu blas-int)
  (abstol (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision)))
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (m blas-int :out)
  (nsplit blas-int :out)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (iblock (:array blas-int *) :in-out)
  (isplit (:array blas-int *) :in-out)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (info blas-int :out))

;;; stein
(deflapack stein (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (e (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (m blas-int)
  (w (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (iblock (:array blas-int *))
  (isplit (:array blas-int *))
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (iwork (:array blas-int *))
  (ifailv (:array blas-int *) :in-out)
  (info blas-int :out))

;;; disna
(deflapack disna (:single :double) :void
  (jobz :string)
  (m blas-int)
  (n blas-int)
  (d (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (sep (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (info blas-int :out))


;;;; routines for generalized symmetric-definite eigenvalue problems

;;; sygst
(deflapack sygst (:single :double) :void
  (itype blas-int)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (info blas-int :out))

;;; hegst
(deflapack hegst (:complex-single :complex-double) :void
  (itype blas-int)
  (uplo :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (info blas-int :out))

;;; spgst
(deflapack spgst (:single :double) :void
  (itype blas-int)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *))
  (info blas-int :out))

;;; hpgst
(deflapack hpgst (:complex-single :complex-double) :void
  (itype blas-int)
  (uplo :string)
  (n blas-int)
  (ap (:array :precision *) :in-out)
  (bp (:array :precision *))
  (info blas-int :out))

;;; sbgst
(deflapack sbgst (:single :double) :void
  (vect :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *))
  (ldbb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (work (:array :precision *))
  (info blas-int :out))

;;; hbgst
(deflapack hbgst (:complex-single :complex-double) :void
  (vect :string)
  (uplo :string)
  (n blas-int)
  (ka blas-int)
  (kb blas-int)
  (ab (:array :precision * *) :in-out)
  (ldab blas-int)
  (bb (:array :precision * *))
  (ldbb blas-int)
  (x (:array :precision * *) :in-out)
  (ldx blas-int)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; pbstf
(deflapack pbstf (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (kb blas-int)
  (bb (:array :precision * *) :in-out)
  (ldbb blas-int)
  (info blas-int :out))


;;;; routines for nonsymmetric eigenvalue problems

;;; gehrd
(deflapack gehrd (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *) :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; orghr
(deflapack orghr (:single :double) :void
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; ormhr
(deflapack ormhr (:single :double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unghr
(deflapack unghr (:complex-single :complex-double) :void
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (tau (:array :precision *))
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; unmhr
(deflapack unmhr (:complex-single :complex-double) :void
  (side :string)
  (trans :string)
  (m blas-int)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (tau (:array :precision *))
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; gebal
(deflapack gebal (:single :double :complex-single :complex-double) :void
  (job :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (scale (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (info blas-int :out))

;;; gebak
(deflapack gebak (:single :double :complex-single :complex-double) :void
  (job :string)
  (side :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (scale (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (m blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (info blas-int :out))

;;; hseqr
(deflapack hseqr (:single :double) :void
  (job :string)
  (compz :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (h (:array :precision * *) :in-out)
  (ldh blas-int)
  (wr (:array :precision *) :in-out)
  (wi (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack hseqr (:complex-single :complex-double) :void
  (job :string)
  (compz :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (h (:array :precision * *) :in-out)
  (ldh blas-int)
  (w (:array :precision *) :in-out)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; hsein
;;; TODO: test select: (:array :boolean *)
(deflapack hsein (:single :double) :void
  (job :string)
  (eigsrc :string)
  (initv :string)
  (select (:array :boolean *) :in-out)
  (n blas-int)
  (h (:array :precision * *))
  (ldh blas-int)
  (wr (:array :precision *) :in-out)
  (wi (:array :precision *))
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (ifaill (:array blas-int *) :out)
  (ifailr (:array blas-int *) :out)
  (info blas-int :out))

(deflapack hsein (:complex-single :complex-double) :void
  (job :string)
  (eigsrc :string)
  (initv :string)
  (select (:array :boolean *) :in-out)
  (n blas-int)
  (h (:array :precision * *))
  (ldh blas-int)
  (w (:array :precision *) :in-out)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (ifaill (:array blas-int *) :out)
  (ifailr (:array blas-int *) :out)
  (info blas-int :out))

;;; trevc
(deflapack trevc (:single :double) :void
  (side :string)
  (howmny :string)
  (select (:array :boolean *) :in-out)
  (n blas-int)
  (tt (:array :precision * *))
  (ldt blas-int)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (info blas-int :out))

(deflapack trevc (:complex-single :complex-double) :void
  (side :string)
  (howmny :string)
  (select (:array :boolean *) :in-out)
  (n blas-int)
  (tt (:array :precision * *))
  (ldt blas-int)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; trsna
(deflapack trsna (:single :double :complex-single :complex-double) :void
  (job :string)
  (howmny :string)
  (select (:array :boolean *))
  (n blas-int)
  (tt (:array :precision * *))
  (ldt blas-int)
  (vl (:array :precision * *))
  (ldvl blas-int)
  (vr (:array :precision * *))
  (ldvr blas-int)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (sep (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision * *))
  (ldwork blas-int)
  (i/rwork (case :precision
	     ((:complex-single :complex-double) '(:array :single *)) 
	     (t '(:array blas-int *))))
  (info blas-int :out))

;;; trexc
(deflapack trexc (:single :double) :void
  (compq :string)
  (n blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (ifst blas-int :in-out)
  (ilst blas-int :in-out)
  (work (:array :precision *))
  (info blas-int :out))

(deflapack trexc (:complex-single :complex-double) :void
  (compq :string)
  (n blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (ifst blas-int :in-out)
  (ilst blas-int :in-out)
  (info blas-int :out))

;;; trsen
(deflapack trsen (:single :double) :void
  (job :string)
  (compq :string)
  (select (:array :boolean *))
  (n blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (wr (:array :precision *) :in-out)
  (wi (:array :precision *) :in-out) 
  (m blas-int :out)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (sep (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

(deflapack trsen (:complex-single :complex-double) :void
  (job :string)
  (compq :string)
  (select (:array :boolean *))
  (n blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (w (:array :precision *) :in-out)
  (m blas-int :out)
  (s (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *)))
     :in-out)
  (sep (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

;;; trsyl
(deflapack trsyl (:single :double :complex-single :complex-double) :void
  (trana :string)
  (tranb :string)
  (isgn blas-int)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (scale (case :precision
	    (:complex-single :single)
	    (:complex-double :double)
	    (t :precision))
	 :out)
  (info blas-int :out))


;;;; generalized nonsymmetric eigenvalue problems
(deflapack gghrd (:single :double :complex-single :complex-double) :void
  (compq :string)
  (compz :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (info blas-int :out))

;;; ggbal
(deflapack ggbal (:single :double :complex-single :complex-double) :void
  (job :string)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (ilo blas-int :out)
  (ihi blas-int :out)
  (lscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (rscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *)))
	  :in-out)
  (work (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *))))
  (info blas-int :out))

;;; ggbak
(deflapack ggbak (:single :double :complex-single :complex-double) :void
  (job :string)
  (side :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (lscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *))))
  (rscale (case :precision
	    (:complex-single '(:array :single *))
	    (:complex-double '(:array :double *))
	    (t '(:array :precision *))))
  (m blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (info blas-int :out))

;;; hgeqz
(deflapack hgeqz (:single :double) :void
  (job :string)
  (compq :string)
  (compz :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (h (:array :precision * *) :in-out)
  (ldh blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (alphar (:array :precision *) :in-out)
  (alphai (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (info blas-int :out))

(deflapack hgeqz (:complex-single :complex-double) :void
  (job :string)
  (compq :string)
  (compz :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (h (:array :precision * *) :in-out)
  (ldh blas-int)
  (tt (:array :precision * *) :in-out)
  (ldt blas-int)
  (alpha (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (z (:array :precision * *) :in-out)
  (ldz blas-int)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; tgevc
(deflapack tgevc (:single :double) :void
  (side :string)
  (howmny :string)
  (select (:array :boolean *))
  (n blas-int)
  (s (:array :precision * *))
  (lds blas-int)
  (p (:array :precision * *))
  (ldp blas-int)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (info blas-int :out))

(deflapack tgevc (:complex-single :complex-double) :void
  (side :string)
  (howmny :string)
  (select (:array :boolean *))
  (n blas-int)
  (s (:array :precision * *))
  (lds blas-int)
  (p (:array :precision * *))
  (ldp blas-int)
  (vl (:array :precision * *) :in-out)
  (ldvl blas-int)
  (vr (:array :precision * *) :in-out)
  (ldvr blas-int)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (info blas-int :out))

;;; tgexc
;;; TODO: test wantq and wantz
(deflapack tgexc (:single :double) :void
  (wantq :boolean)
  (wantz :boolean)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (q (:array :precision * *))
  (ldq blas-int)
  (z (:array :precision * *))
  (ldz blas-int)
  (ifst blas-int :in-out)
  (ilst blas-int :in-out)
  (work (:array :precision *))
  (lwork blas-int)
  (info blas-int :out))

(deflapack tgexc (:complex-single :complex-double) :void
  (wantq :boolean)
  (wantz :boolean)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (q (:array :precision * *))
  (ldq blas-int)
  (z (:array :precision * *))
  (ldz blas-int)
  (ifst blas-int :in-out)
  (ilst blas-int :in-out)
  (info blas-int :out))

;;; tgsen
(deflapack tgsen (:single :double) :void
  (ijob blas-int)
  (wantq :boolean)
  (wantz :boolean)
  (select (:array :boolean *))
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alphar (:array :precision *) :in-out)
  (alphai (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (z (:array :precision * *) :in-out)
  (m blas-int :out)
  (pl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision))
      :out)
  (pr (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision))
      :out)
  (dif (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

(deflapack tgsen (:complex-single :complex-double) :void
  (ijob blas-int)
  (wantq :boolean)
  (wantz :boolean)
  (select (:array :boolean *))
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (alpha (:array :precision *) :in-out)
  (beta (:array :precision *) :in-out)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (z (:array :precision * *) :in-out)
  (m blas-int :out)
  (pl (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision))
      :out)
  (pr (case :precision
	(:complex-single :single)
	(:complex-double :double)
	(t :precision))
      :out)
  (dif (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *) :in-out)
  (liwork blas-int)
  (info blas-int :out))

;;; tgsyl
(deflapack tgsyl (:single :double :complex-single :complex-double) :void
  (trans :string)
  (ijob blas-int)
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (d (:array :precision * *))
  (ldd blas-int)
  (e (:array :precision * *))
  (lde blas-int)
  (f (:array :precision * *))
  (ldf blas-int)
  (scale (case :precision
	   (:complex-single :single)
	   (:complex-double :double)
	   (t :precision))
	 :out)
  (dif (case :precision
	 (:complex-single :single)
	 (:complex-double :double)
	 (t :precision))
       :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (info blas-int :out))
  
;;; tgsna
(deflapack tgsna (:single :double :complex-single :complex-double) :void
  (job :string)
  (howmny :string)
  (select (:array :boolean *))
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (vl (:array :precision * *))
  (ldvl blas-int)
  (vr (:array :precision * *))
  (ldvr blas-int)
  (s (:array :precision * *) :in-out)
  (dif (case :precision
	 (:complex-single '(:array :single *))
	 (:complex-double '(:array :double *))
	 (t '(:array :precision *)))
       :in-out)
  (mm blas-int)
  (m blas-int :out)
  (work (:array :precision *) :in-out)
  (lwork blas-int)
  (iwork (:array blas-int *))
  (info blas-int :out))

;;; ggsvp
(deflapack ggsvp (:single :double) :void
  (jobu :string)
  (jobv :string)
  (jobq :string)
  (m blas-int)
  (p blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (tola (case :precision
	 (:complex-single :single)
	 (:complex-double :double)
	 (t :precision)))
  (tolb (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision)))
  (k blas-int :out)
  (l blas-int :out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (iwork (:array blas-int *))
  (tau (:array :precision *))
  (work (:array :precision *))
  (info blas-int :out))

(deflapack ggsvp (:complex-single :complex-double) :void
  (jobu :string)
  (jobv :string)
  (jobq :string)
  (m blas-int)
  (p blas-int)
  (n blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (tola (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision)))
  (tolb (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision)))
  (k blas-int :out)
  (l blas-int :out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (iwork (:array blas-int *))
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *))))
  (tau (:array :precision *))
  (work (:array :precision *))
  (info blas-int :out))

;;; tgsja
(deflapack tgsja (:single :double :complex-single :complex-double) :void
  (jobu :string)
  (jobv :string)
  (jobq :string)
  (m blas-int)
  (p blas-int)
  (n blas-int)
  (k blas-int)
  (l blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int)
  (tola (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision)))
  (tolb (case :precision
	  (:complex-single :single)
	  (:complex-double :double)
	  (t :precision)))
  (alpha (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))
	 :in-out)
  (beta (case :precision
	  (:complex-single '(:array :single *))
	  (:complex-double '(:array :double *))
	  (t '(:array :precision *)))
	:in-out)
  (u (:array :precision * *) :in-out)
  (ldu blas-int)
  (v (:array :precision * *) :in-out)
  (ldv blas-int)
  (q (:array :precision * *) :in-out)
  (ldq blas-int)
  (work (:array :precision *))
  (ncycle blas-int :out)
  (info blas-int :out))




;;;;;; LAPACK auxiliary and utility routines

;;;; auxiliary routines

;;; lacgv
(deflapack lacgv (:complex-single :complex-double) :void
  (n blas-int)
  (x (:array :precision * *) :in-out)
  (incx blas-int))

;;; lacrm
(deflapack lacrm (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (b (case :precision
       (:complex-single '(:array :single *))
       (:complex-double '(:array :double *))
       (t '(:array :precision *))))
  (ldb blas-int)
  (c (:array :precision * *) :in-out)
  (ldc blas-int)
  (rwork (case :precision
	   (:complex-single '(:array :single *))
	   (:complex-double '(:array :double *))
	   (t '(:array :precision *)))))

;;; lacrt
(deflapack lacrt (:complex-single :complex-double) :void
  (n blas-int)
  (cx (:array :precision *) :in-out)
  (incx blas-int)
  (cy (:array :precision *) :in-out)
  (incy blas-int)
  (c :precision)
  (s :precision))

;;; laesy
(deflapack laesy (:complex-single :complex-double) :void
  (a :precision)
  (b :precision)
  (c :precision)
  (rt1 :precision :out)
  (rt2 :precision :out)
  (evscal :precision :out)
  (cs1 :precision :out)
  (sn1 :precision :out))

;;; rot
(deflapack rot (:complex-single :complex-double) :void
  (n blas-int)
  (cx (:array :precision *) :in-out)
  (incx blas-int)
  (cy (:array :precision *) :in-out)
  (incy blas-int)
  (c (case :precision
       (:complex-single :single)
       (:complex-double :double)
       (t :precision)))
  (s :precision))

;;; spmv
(deflapack spmv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (ap (:array :precision *))
  (x (:array :precision * *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; TODO: more auxiliary routines


;;;; utility functions and routines

;;; ilaver
(defffun ilaver :void
  (vers-major blas-int :out)
  (vers-minor blas-int :out)
  (vers-patch blas-int :out))
(export 'ilaver)

;;; ilaenv
(defffun ilaenv blas-int
  (ispec blas-int)
  (name :string)
  (opts :string)
  (n1 blas-int)
  (n2 blas-int)
  (n3 blas-int)
  (n4 blas-int))
(export 'ilaenv)

;;; iparmq
(defffun iparmq blas-int
  (ispec blas-int)
  (name :string)
  (opts :string)
  (n blas-int)
  (ilo blas-int)
  (ihi blas-int)
  (lwork blas-int))
(export 'iparmq)

;;; ieeeck
(defffun ieeeck blas-int 
  (ispec blas-int)
  (zero :float)
  (one :float))
(export 'ieeeck)

;;; lsamen
(defffun lsamen :boolean
  (n blas-int)
  (ca :string)
  (cb :string))
(export 'lsamen)

;;; labad
(deflapack labad (:single :double) :void
  (small :precision :in-out)
  (large :precision :in-out))

;;; lamch
(deflapack lamch (:single :double) :precision
  (cmach :string))

;;; lamc1
#+nil
(deflapack lamc1 (:single :double) :void
  (beta blas-int :out)
  (tt blas-int :out)
  (rnd :boolean :out)
  (ieee1 :boolean :out))

;;; lamc2
#+nil
(deflapack lamc2 (:single :double) :void
  (beta blas-int :out)
  (tt blas-int :out)
  (rnd :boolean :out)
  (eps :precision :out)
  (emin blas-int :out)
  (rmin :precision :out)
  (emax blas-int :out)
  (rmax :precision :out))

;;; lamc3
#+nil
(deflapack lamc3 (:single :double) :precision
  (a :precision)
  (b :precision))

;;; lamc4
#+nil
(deflapack lamc4 (:single :double) :void
  (emin blas-int :out)
  (start :precision)
  (base blas-int))

;;; lamc5
#+nil
(deflapack lamc5 (:single :double) :void
  (beta blas-int)
  (p blas-int)
  (emin blas-int)
  (ieee :boolean)
  (emax blas-int :out)
  (rmax :precision :out))

;;; second
;;; NOTE: second is CL built-in function so I changed it to ssecnd
(defffun (ssecnd "second") :float)
(defffun dsecnd :double)
(export 'ssecond)
(export 'dsecond)



;;;; progress routine (for intel mkl)

;;; mkl-progress
#+intel-mkl
(progn
  (defffun mkl-progress blas-int
    (thread blas-int)
    (step blas-int)
    (stage :string))
  (export 'mkl-progress))

