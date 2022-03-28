#include <R.h>
#include <Rinternals.h>

SEXP add_(SEXP x_, SEXP y_);
SEXP COMPUTE_CROSS_CORR_(SEXP xx_in, SEXP yy_in, SEXP minoverlap_in);
SEXP NA_TRIM_(SEXP seq_in);
SEXP LOCAL_MAX_(SEXP seq_in, SEXP MAX_MIN_in);
