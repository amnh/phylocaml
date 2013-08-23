/**
 * CONVENTIONS:
 *
 * variables that start with a:
 *      Uppercase letter are matrices
 *      lowercase letter are vectors
 *
 * variables that end with a,
 *      t is the transpose (thus, pi is a column vector)
 *      i is the inverse
 *
 * D is a diagonal matrix (usually of eigenvalues)
 * U are eigenvectors (column major, Ut is rowmajor)
 * p are the priors as a vector
 * Dp are the priors as a diagonal matrix
 *
 * variable:
 *      n/m is the number of rows/columns or size of the alphabet
 *      t is the branch length
 */
#include "phyloc.h"
#include "mlmodel.h"

/* prints a matrix (block format) */
void printmatrixf( const double* Z, const int s, const int n, const int m)
{
  int i,j;
  for (i=0; i<m; ++i) {
    putchar('\t');
    for (j=0; j<n; ++j)
      printf("[%11.10f] ", Z[i*s+j]);
    putchar('\n');
  }
}

/* prints an array horizontally */
void printarrayf( const double* a, const int n )
{
  int i;
  for(i=0;i<n;++i)
    printf("[%6.5f] ", a[i]);
  putchar('\n');
}

/** transpose of matrix [A] of [n]x[n], must be square */
void transpose( double *A, const int n, const int s )
{
  double tmp;
  int i,j;
  for(i=0;i<n;++i){
    for(j=i+1;j<n;++j){
      tmp = A[i*s+j];
      A[i*s+j] = A[j*s+i];
      A[j*s+i] = tmp;
    }
  }
}

/** creates a random symmetric sub-rate matrix
 * each row sums to 0 on diagonal elements */
void rand_sub_mat_sym( double* A, const int n, const int s)
{
  int i,j;
  double temp,diag;
  srand( time(NULL) );
  for(i=0;i<n;++i){
    diag = 0;
    for(j=0;j<n;++j){
      if( i <= j ) { continue; }
      temp = ((double)rand()/(double)RAND_MAX);
      A[i*s+j] = temp;
      A[j*s+i] = temp;
      diag = diag + temp + temp;
    }
    A[i*s+i] = -diag;
  }
}

/**  [diag diag mat n] ~ makes a diagonal square matrix from the first [n]
 * elements of the matrix. Put first [n] elements of [M] along diagonal of [M]. */
void diag(double* M, const int n)
{
  int i;
  for(i = 1; i < n; ++i){
    M[i*n+i] = M[i];
    M[i] = 0;
  }
}

/** [inverse D S n] makes the inverse of S through LU decomposition and stores
 * the result in D. We catch and report all LAPACK error codes. */
int inverse(double *D, const double *S, int n)
{
  /* LU factorization and inverse via DGETRF and DGETRI */
  int i,lwork,*pivot;
  double work_size, *work;
  lwork = -1;
  pivot = (int*) malloc(n * sizeof(int));
  memcpy(D, S, n*n*sizeof(double) );
  dgetrf_(&n, &n, D, &n, pivot, &i);
  if( 0 == i ){
    /* calculate optimal work */
    dgetri_(&n, D, &n, pivot, &work_size, &lwork, &i);
    if( 0 == i ){
      lwork = (int)work_size;
      work = (double*) malloc(lwork * sizeof(double));
      dgetri_(&n, D, &n, pivot, work, &lwork, &i);
      free(work);
      if( i < 0 ){
        failwith("dgetri_ argument failed.1");
      } else if( i > 0 ){
        failwith("dgetri_ matrix is singular and inverse cannot be computed.1");
      }
    } else if( i < 0 ){ 
      failwith("dgetri_ argument failed.2");
    } else {
      failwith("dgetri_ matrix is singular and inverse cannot be computed.2");
    }
  } else {
    failwith("dgetrf_ unknown error.");
  }
  free (pivot);
  return i;
}

/**  [apply_exp diag n m t] Multiplies the diagonal of [diag], an [n]x[m]
 * matrix by [t] and applies exp() */
void apply_exp(double* D, const int n, const double t)
{
  int i,s;
  s = n * n;
  for(i=0;i<s;i=i+n+1)
    D[i] = exp( D[i]*t );
}

/** [log_matrix m] Take the log of each value of the matrix */
void log_matrix(double* PA, const int n, const int m)
{
  int i,s;
  s = m * n;
  for( i=0;i<s;++i )
    PA[i] = log( PA[i] );
}

/** [create_identity P n] Fill a square matrix with I; width && height = n */
void create_identity(double* P, const int n)
{
  int i,j;
  for( i=0;i<n;++i ){
    for( j=0;j<n;++j )
      P[i*n+j] = (i == j) ? 1.0 : 0.0;
  }
}

/**  [diagonalize_sym A D N]
 * Diagonalizes [A] that is [N]x[N] and upper symmetric and places
 * the resultant eigenvalues in the matrix [D] along the diagonal.
 * Overwrites [A] with the eigenvectors (rowmajor).
 *
 * @returns =0 on success
 *          <0 ith argument had an issue
 *          >0 failed in convergence */
int diagonalize_sym( double* A, double* D, int n)
{
  char jobz = 'V';
  char uplo = 'U';
  int info = 0,lwork=-1;
  double *work,work_size;
  /* find the optimal work (call func with -1 in lwork)
   * result ends up in work_size */
  dsyev_(&jobz, &uplo, &n, A, &n, D, &work_size, &lwork, &info);
  if( info == 0 ){
    lwork = (int)work_size;
    work = (double*) malloc( lwork * sizeof(double) );
    /** dsyev - calculate eigenvalues and vectors
     *          possibly use PDSYGVX or PDSYEVX --less precision)
     * jobz   - V to return eigenvectors (N otherwise)
     * uplo   - Upper/Lower triangular? (U/L)
     * n      - order of the matrix
     * E_vecs - (in/out) dim(lda,n) --note: lda==n
     * n      - lda
     * e_vals - (out) eigenvalues
     * work   - (workspace/out) = dim(n)
     * lwork  - length of work >= (NB+2)*N
     * info   - retval; see comments above */
    dsyev_(&jobz, &uplo, &n, A, &n, D, work, &lwork, &info);
    free(work);
    if( 0 == info ){
      diag(D,n);
    } else if( info < 0 ){
      failwith("dsyev_ argument failed");
    } else { 
      failwith("dsyev_ diagonalization failed to converge. Singular matrix?");
    }
  }
  return info;
}

/* Diagonlize matrix interfaces: symmetric */
value likelihood_CAML_diagonalize_sym( value Q, value D)
{
  CAMLparam2(Q, D);
  diagonalize_sym(Data_bigarray_val(Q),Data_bigarray_val(D),Bigarray_val(Q)->dim[0]);
  CAMLreturn( Val_unit );
}

/* Diagonalize general rate matrix */
int diagonalize_gtr(double* A, double* D, double* Ui, int n)
{
  char jobv_ = 'V'; /* we went to find left and right eigenvectors */
  double *wi,*U,*work,work_size;
  int lwork,info;
  /* find the optimal work (call func with -1 in lwork) */
  lwork = -1;
  /* D holds the real values eigen values through the computation */
  wi = (double*) malloc( n * sizeof(double) );
  U  = (double*) malloc( n*n * sizeof(double) );
  dgeev_(&jobv_,&jobv_,&n,A,&n,D,wi,U,&n,Ui,&n,&work_size,&lwork,&info);
  if( info == 0 ) {
    lwork = (int)work_size;
    work = (double*) malloc(lwork * sizeof(double));
    /** dgeev   - A * v(j) = lambda(j) * v(j)
     *            u(j)**H * A = lambda(j) * u(j)**H
     *            where:
     *             ` **H is conjugate transpose)
     *             ` u(j) is a left eigenvector
     *             ` v(j) is a right eigenvector
     *             ` lambda(j) is an eigenvalue

     * JOBVL    -calulate left eigenvectors? V/N
     * JOBVR    -calulate right eigenvectors? V/N
     * N        - order(A)
     * A        - matrix of LDAxN           (**MODIFIED**)
     * LDA      - dim(A) 
     * WR       - real parts of eigenvalues (**OUTPUT**) //putting them in D
     * WI       - imaginary parts           (**OUTPUT**) //ignored
     * VL       - left eigenvectors         (**OUTPUT**)
     * LDVL     - dim(VL)
     * VR       - right eigenvectors        (**OUTPUT**)
     * LDVR     - dim(VR)
     * WORK     - workspace
     * LWORK    - dim(work)
     * INFO     - =0 if successful
     *            <0 if the ith argument is illegal
     *            >0 if QR failed; i+1 is first eigenvalue */
    dgeev_(&jobv_,&jobv_,&n,A,&n,D,wi,U,&n,Ui,&n,work,&lwork,&info);
    /* eigenvalues are not imaginary for matrix similar to symmetric matrix (Keilson 1979)) */
    for(lwork=0;lwork<n;lwork++){
      if(wi[lwork]> EPSILON || wi[lwork] < -EPSILON){ failwith("Imaginary eigenvalues");}
    }
    if(0 == info){
      diag(D,n);
      inverse(U, Ui, n);
    } else if ( info < 0 ){
      failwith("dgeev_ argument failed");
    } else {
      failwith("dgeev_ QR failed, possibly singular matrix?");
    }
  }
  memcpy(A,U,n*n*sizeof(double));
  return info;
}

/* Diagonalize matrix interface: general */
value likelihood_CAML_diagonalize_gtr( value Q, value D, value Qi)
{
  CAMLparam3( Q, D, Qi );
  diagonalize_gtr(Data_bigarray_val(Q), Data_bigarray_val(D),
                  Data_bigarray_val(Qi), Bigarray_val(Q)->dim[0]);
  CAMLreturn( Val_unit );
}

/** [compose_*** P U D [Ui] t]
 * Finds the probability matrix based on the diagonalized substitution
 * matrix, [U] and [D], with branch length [t]. returns result in [P].
 *
 * UNCHECKED EXCEPTIONS ::
 *   [n]x[n] == dim([D]) == dim([P]) == dim([U]) == dim([Ui]) == dim([TMP])
 *   t > 0 */  
void compose_sym(double* P,const double* U,const double* D,const float t,int n,double *TMP)
{
  char _tran,ntran;
  double alpha,beta;
  alpha = 1; beta = 0; _tran = 'T'; ntran = 'N';
  memcpy(P, D, n*n*sizeof(double) );
  if(t == -1.0){ /** Instantaneous rate matrix **/
    dgemm_(&ntran,&ntran, &n, &n, &n, &alpha, U, &n, P, &n, &beta, TMP, &n );
    dgemm_(&ntran,&_tran,&n,&n,&n,&alpha,TMP,&n,U,&n,&beta,P,&n);
  } else if(t >= EPSILON){
    apply_exp(P,n,t); /* exp(D*t); along diagonal only */
    /* calculates: C = op(A)*op(B)*a + C*b */
    dgemm_(&ntran,&ntran,       /* format, op(A), op(B) */
           &n, &n, &n, &alpha,  /* rows A, cols B, cols A, multiplier of (A*B) */
            U, &n,              /* MATRIX A, stride for A */
            P, &n,              /* MATRIX B, stride for B */
           &beta, TMP, &n );    /* multiplier for C, MATRIX C, stride for C */
    /* if scalor mult of C == 0, C isn't used to add */
    dgemm_(&ntran,&_tran,&n,&n,&n,&alpha,TMP,&n,U,&n,&beta,P,&n);
  } else {
    create_identity( P , n );
  }
}

value likelihood_CAML_compose_sym(value U, value D, value t)
{
  CAMLparam3(U,D,t);
  CAMLlocal1( res );
  double *c_P, c_t,*c_D,*c_U,*c_T;
  int n;
  long dims[2];

  n = Bigarray_val(U)->dim[0];
  c_t = Double_val( t );
  c_U = (double *) Data_bigarray_val( U );
  c_D = (double *) Data_bigarray_val( D );
  c_P = (double*) malloc( n*n*sizeof(double));
  c_T = (double*) malloc( n*n*sizeof(double));
  compose_sym(c_P,c_U,c_D,c_t,n,c_T);
  dims[0] = n; dims[1] = n;
  res = alloc_bigarray(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT, 2, c_P, dims);
  free(c_T);
  CAMLreturn ( res );
}

void
compose_gtr(double* P, const double* U, const double* D, const double* Ui, 
        const double t, const int n,double *tmp)
{
  double alpha,beta; char ntran;
  alpha = 1; beta = 0; ntran = 'N';
  memcpy(P,D,n*n*sizeof(double));
  if(t == -1.0){
    dgemm_(&ntran,&ntran,&n,&n,&n,&alpha,Ui,&n,P,&n,&beta,tmp,&n);
    dgemm_(&ntran,&ntran,&n,&n,&n,&alpha,tmp,&n,U,&n,&beta,P,&n);
  } else if(t >= EPSILON){
    apply_exp(P,n,t);
    dgemm_(&ntran,&ntran,&n,&n,&n,&alpha,Ui,&n,P,&n,&beta,tmp,&n);
    dgemm_(&ntran,&ntran,&n,&n,&n,&alpha,tmp,&n,U,&n,&beta,P,&n);
  } else { /* identity matrix when t = 0 */
    create_identity( P, n );
  }
}

value likelihood_CAML_compose_gtr(value U, value D, value Ui, value t)
{
  CAMLparam4( U,D,Ui,t );
  CAMLlocal1( res );
  double *c_P, c_t,*c_D,*c_U,*c_Ui,*c_T;
  int n;
  long dims[2];

  n = Bigarray_val( U )->dim[0];
  c_t = Double_val( t );
  c_U = (double*) Data_bigarray_val( U );
  c_D = (double*) Data_bigarray_val( D );
  c_Ui= (double*) Data_bigarray_val( Ui);
  c_P = (double*) malloc( n*n*sizeof(double) );
  c_T = (double*) malloc( n*n*sizeof(double) );

  compose_gtr(c_P,c_U,c_D,c_Ui,c_t,n,c_T);
  dims[0] = n; dims[1] = n;
  res = alloc_bigarray(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT, 2, c_P, dims);
  free( c_T );
  CAMLreturn( res );
}
