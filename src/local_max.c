#include <R.h>
#include <Rinternals.h>

#include <math.h>

SEXP LOCAL_MAX_(SEXP seq_in, SEXP MAX_MIN_in)
{
        int protect_cnt = 0;
        R_len_t len = length(seq_in);
        double *seq, *out_linker, *MAX_MIN;

        if (isInteger(seq_in))
        {
                PROTECT(seq_in = coerceVector(seq_in, REALSXP));
                protect_cnt += 1;
        }
        else if (!isReal(seq_in))
        {
                error("numeric values are expected for _NA_TRIM.");
        }
        seq = REAL(seq_in);

        if (isInteger(MAX_MIN_in))
        {
                PROTECT(MAX_MIN_in = coerceVector(MAX_MIN_in, REALSXP));
                protect_cnt += 1;
        }
        else if (!isReal(MAX_MIN_in))
        {
                error("numeric values are expected for _NA_TRIM.");
        }
        MAX_MIN = REAL(MAX_MIN_in);

        if (len == 1)
        {
                return (ScalarReal(seq[0]));
        }
        else if (len < 1)
        {
                error("input has length 0");
        }

        double *diff_check = calloc(len, sizeof *diff_check);
        if (diff_check == NULL)
        {
                error("No enough memory");
        }
        int out_count = 0;

        if (*MAX_MIN == 0)
        { // find local max
                if (seq[0] - seq[1] >= 0)
                {
                        diff_check[0] = 1;
                        out_count += 1;
                }

                if (seq[len - 1] >= seq[len - 2])
                {
                        diff_check[len - 1] = 1;
                        out_count += 1;
                }
        }
        else
        { // find local min
                if (seq[0] - seq[1] <= 0)
                {
                        diff_check[0] = 1;
                        out_count += 1;
                }

                if (seq[len - 1] <= seq[len - 2])
                {
                        diff_check[len - 1] = 1;
                        out_count += 1;
                }
        }

        for (int i = 1; i < len - 1; i++)
        {
                if (*MAX_MIN == 0)
                {
                        if (seq[i] >= seq[i - 1] && seq[i] >= seq[i + 1])
                        {
                                diff_check[i] = 1;
                                out_count += 1;
                        }
                }
                else
                {
                        if (seq[i] <= seq[i - 1] && seq[i] <= seq[i + 1])
                        {
                                diff_check[i] = 1;
                                out_count += 1;
                        }
                }
        }

        SEXP out = PROTECT(allocVector(REALSXP, out_count));
        protect_cnt += 1;
        out_linker = REAL(out);

        int j = 0;

        for (int i = 0; i < len; i++)
        {
                if (diff_check[i])
                {
                        out_linker[j] = i + 1;
                        j++;
                }
        }

        UNPROTECT(protect_cnt);

        if (diff_check)
                free(diff_check);

        return (out);
}

