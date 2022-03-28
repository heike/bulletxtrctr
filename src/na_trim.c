#include <R.h>
#include <Rinternals.h>

#include <math.h>

SEXP NA_TRIM_(SEXP seq_in)
{
        int head_na_count = 0, tail_na_count = 0, protect_cnt = 0;
        int *out_linker;
        double *seq;

        R_len_t len = length(seq_in);

        SEXP out = PROTECT(allocVector(INTSXP, 2));
        protect_cnt += 1;
        out_linker = INTEGER(out);

        if(isInteger(seq_in))
        {
                PROTECT(seq_in = coerceVector(seq_in, REALSXP));
                protect_cnt += 1;
        } else if (!isReal(seq_in))
        {
                error("numeric values are expected for _NA_TRIM.");
        }
        seq = REAL(seq_in);
        int i = 0;

        while(isnan(seq[i]))
        {
                head_na_count += 1;
                i++;
        }

        i = len - 1;
        while(isnan(seq[i]))
        {
                tail_na_count += 1;
                i--;
        }

        out_linker[0] = head_na_count;
        out_linker[1] = tail_na_count;

        UNPROTECT(protect_cnt);

        return(out);

}