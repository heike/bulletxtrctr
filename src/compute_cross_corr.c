#include <R.h>
#include <Rinternals.h>

#include <math.h>

// https://github.com/SurajGupta/r-source/blob/master/src/main/arithmetic.c
// two loop with one operation each
// vs one loop with two operations

SEXP COMPUTE_CROSS_CORR_(SEXP xx_in, SEXP yy_in, SEXP minoverlap_in)
{
        // xx_in is the long vector
        // yy_in is the short vector that will be shifting around

        R_len_t lenyy = length(yy_in);
        R_len_t lenxx = length(xx_in); // includes necessary shifting space with NA

        double *xx, *yy;
        int *xx_na, *yy_na;
        // double *px, *py, *px2, *py2;
        double pxsum, pysum, pxpysum, px2sum, py2sum, corr, denom;
        double pxsum_tr, pysum_tr, pxpysum_tr, px2sum_tr, py2sum_tr;

        pxsum = pysum = pxpysum = px2sum = py2sum = 0;
        pxsum_tr = pysum_tr = pxpysum_tr = px2sum_tr = py2sum_tr = 0;

        int true_count = 0, minoverlap; // overlap_length;

        xx = REAL(xx_in);                           // maybe length of 1800
        yy = REAL(yy_in);                           // maybe length of 50
        minoverlap = (int)(REAL(minoverlap_in)[0]); // min.overlap

        int left_overlap_idx = lenyy - minoverlap;
        // int xx_len_tr = lenxx - 2 * left_overlap_idx;

        xx_na = (int *)calloc(lenxx, sizeof *xx_na);
        yy_na = (int *)calloc(lenyy, sizeof *yy_na);

        SEXP corr_vec = PROTECT(allocVector(REALSXP, lenxx - lenyy + 1));
        double *cor_result;
        cor_result = REAL(corr_vec);
        // SEXP out = PROTECT(allocVector(VECSXP, 3));

        // if(ISNA(yy[0])) {
        //         Rprintf("good %lf check: %d %d %d %lf %d %d\n %d %d \n %d \n", yy[0], isnan(yy[0]), isnan(yy[1]),
        //                                            __inline_isnand(yy[0]), yy[0] + yy[1], 1 && 1, 1 && 0, !1, !0, 1 +
        //                                            5);
        // }

        // if isnan evals true, mark 1; i.e. mark all NA values
        for (R_len_t i = 0; i < lenxx; i++)
        {
                if (isnan(xx[i]))
                        xx_na[i] = 1;
        }

        for (R_len_t i = 0; i < lenyy; i++)
        {
                if (isnan(yy[i]))
                        yy_na[i] = 1;
        }

        // overlap_length = minoverlap;
        // initialize sums for the front

        if (left_overlap_idx < 0)
        {
                error("length of y is less than min.overlap");
        }

        for (int i = 0; i < lenyy; i++)
        {
                // addup all non-NA values in the range
                if (!yy_na[i])
                {
                        pysum += yy[i];
                        py2sum += yy[i] * yy[i];
                }

                // tmpidx = i - left_overlap_idx; // 39 = 49 - 10
                if (!xx_na[i])
                {
                        pxsum += xx[i];
                        px2sum += xx[i] * xx[i];
                }
        }

        // Rprintf("%lf %lf %lf %lf\n %d\n", pysum, py2sum, pxsum, px2sum, left_overlap_idx);

        for (int j = 0; j < lenxx - lenyy + 1; j++)
        {
                true_count = 0;
                pxpysum = 0;

                if (j != 0)
                {
                        if (!xx_na[j - 1])
                        // if(!__inline_isnand(xx[j-1]))
                        {
                                pxsum -= xx[j - 1];
                                px2sum -= xx[j - 1] * xx[j - 1];
                        }
                        if (!xx_na[j - 1 + lenyy])
                        // if(!__inline_isnand(xx[j-1+lenyy]))
                        {
                                pxsum += xx[j - 1 + lenyy];
                                px2sum += xx[j - 1 + lenyy] * xx[j - 1 + lenyy];
                        }
                }

                pxsum_tr = pxsum;
                px2sum_tr = px2sum;
                pysum_tr = pysum;
                py2sum_tr = py2sum;

                for (int i = 0; i < lenyy; i++)
                {

                        if (!xx_na[i + j]) // xx has value
                        {
                                if (!yy_na[i]) // both have values
                                {
                                        true_count += 1;
                                        pxpysum += xx[i + j] * yy[i];
                                }
                                else
                                { // but yy has NA
                                        pxsum_tr -= xx[i + j];
                                        px2sum_tr -= xx[i + j] * xx[i + j];
                                }
                        }
                        else
                        {                      // xx has NA
                                if (!yy_na[i]) // but yy has value
                                {
                                        pysum_tr -= yy[i];
                                        py2sum_tr -= yy[i] * yy[i];
                                } // else, both NA, do nothing
                        }
                }

                if (true_count < minoverlap)
                {
                        cor_result[j] = NA_REAL;
                }
                else
                {
                        denom = sqrt((true_count * px2sum_tr - pxsum_tr * pxsum_tr) * 
                                (true_count * py2sum_tr - pysum_tr * pysum_tr));
                        
                        if(denom == 0){
                                cor_result[j] = NA_REAL;
                        } else {
                                corr = (pxpysum * true_count - pxsum_tr * pysum_tr) / denom;
                                cor_result[j] = corr;
                        }
                        
                        
                               
                        // SET_REAL_ELT(corr_vec, j, corr);
                        
                        // Rprintf("%lf %lf %d %d\n %lf %lf %lf %lf\n %d %d\n%lf %lf\n %;f %lf %lf %lf\n", corr, pxpysum, true_count, minoverlap,
                        //         pxsum_tr, pysum_tr, px2sum_tr, py2sum_tr,
                        //         xx_na[0], yy_na[0],
                        //         pxpysum * true_count - pxsum_tr * pysum_tr,
                        //         ((true_count * px2sum_tr - pxsum_tr * pxsum_tr) * (true_count * py2sum_tr - pysum_tr * pysum_tr)),
                        //         pxsum, px2sum, pysum, py2sum);
                }
        }

        UNPROTECT(1);

        if (xx_na)
                free(xx_na);
        if (yy_na)
                free(yy_na);

        return (corr_vec);
        // return (out);
}


