#include <stdio.h>
#include <stdlib.h>         /* malloc, calloc, srand, RAND_MAX, posix_memalign */
#include <string.h>         /* memcpy, memset */

#include <time.h>           /* seed for random */
#include <math.h>           /* log10,exp */

/* caml specific headers */
#include <caml/alloc.h>     /* copy_double, et cetera */
#include <caml/memory.h>    /* caml_param, et cetera */
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/fail.h>      /* failwith('') */
#include <caml/custom.h>    /* abstract types */

/* CONSTANTS */
#define EPSILON      1e-10   /* error for numerical calculations */

/* double symmetric eigenvalue/vector routine */
int dsyev_( char *jobz, char *uplo, int *n, double *a, int *lda, double *w,
            double *work, int *lwork, int *info);

/* double general eigenvalue/vector routine */
int dgeev_( char *jobvl, char *jobvr, int *n, double * a, int *lda, double *wr,
            double *wi, double *vl, int *ldvl, double *vr, int *ldvr,
            double *work, int *lwork, int *info);

/* double general real LU-factorization */
int dgetrf_(int *m, int *n, double *a, int * lda, int *ipiv, int *info);

/* double general real matrix inverse using LU */
int dgetri_(int *n, double *a, int *lda, int *ipiv, double *work,
            int *lwork, int *info);

int dgemm_( char *transa, char *transb, const int *m, const int *n, const int *k,
            double *alpha, const double *a, const int *lda, const double *b,
            const int *ldb, double *beta, double *c, const int *ldc);

value likelihood_CAML_compose_gtr(value U, value D, value Ui, value t);
value likelihood_CAML_compose_sym(value U, value D, value t);

value likelihood_CAML_diagonalize_gtr( value Q, value D, value Qi);
value likelihood_CAML_diagonalize_sym( value Q, value D);
