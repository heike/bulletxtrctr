#include "bulletxtrctr.h"
#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
        {"compute_cross_corr_c",      (DL_FUNC) &COMPUTE_CROSS_CORR_,  3},
        {"trim_na_c",                 (DL_FUNC) &NA_TRIM_,             1},
        {"maxmin_local_c",            (DL_FUNC) &LOCAL_MAX_,           2},
        {NULL,                        NULL,                            0}
};

void R_init_bulletxtrctr(DllInfo *info)
{
//   SymbolShortcuts();
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, TRUE);

  /* used by external packages linking to internal code from C */
  R_RegisterCCallable("bulletxtrctr","compute_cross_corr",  (DL_FUNC) &COMPUTE_CROSS_CORR_);
  R_RegisterCCallable("bulletxtrctr","trim_na",             (DL_FUNC) &NA_TRIM_);
  R_RegisterCCallable("bulletxtrctr","maxmin_local",        (DL_FUNC) &LOCAL_MAX_);

}
