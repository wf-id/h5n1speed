#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void sir_base_rhs_dde(void *);
extern void sir_vaccine_rhs_dde(void *);

/* .Call calls */
extern SEXP sir_base_contents(SEXP);
extern SEXP sir_base_create(SEXP);
extern SEXP sir_base_initial_conditions(SEXP, SEXP);
extern SEXP sir_base_metadata(SEXP);
extern SEXP sir_base_rhs_r(SEXP, SEXP, SEXP);
extern SEXP sir_base_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP sir_base_set_user(SEXP, SEXP);
extern SEXP sir_vaccine_contents(SEXP);
extern SEXP sir_vaccine_create(SEXP);
extern SEXP sir_vaccine_initial_conditions(SEXP, SEXP);
extern SEXP sir_vaccine_metadata(SEXP);
extern SEXP sir_vaccine_rhs_r(SEXP, SEXP, SEXP);
extern SEXP sir_vaccine_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP sir_vaccine_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"sir_base_rhs_dde",    (DL_FUNC) &sir_base_rhs_dde,    1},
    {"sir_vaccine_rhs_dde", (DL_FUNC) &sir_vaccine_rhs_dde, 1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"sir_base_contents",              (DL_FUNC) &sir_base_contents,              1},
    {"sir_base_create",                (DL_FUNC) &sir_base_create,                1},
    {"sir_base_initial_conditions",    (DL_FUNC) &sir_base_initial_conditions,    2},
    {"sir_base_metadata",              (DL_FUNC) &sir_base_metadata,              1},
    {"sir_base_rhs_r",                 (DL_FUNC) &sir_base_rhs_r,                 3},
    {"sir_base_set_initial",           (DL_FUNC) &sir_base_set_initial,           4},
    {"sir_base_set_user",              (DL_FUNC) &sir_base_set_user,              2},
    {"sir_vaccine_contents",           (DL_FUNC) &sir_vaccine_contents,           1},
    {"sir_vaccine_create",             (DL_FUNC) &sir_vaccine_create,             1},
    {"sir_vaccine_initial_conditions", (DL_FUNC) &sir_vaccine_initial_conditions, 2},
    {"sir_vaccine_metadata",           (DL_FUNC) &sir_vaccine_metadata,           1},
    {"sir_vaccine_rhs_r",              (DL_FUNC) &sir_vaccine_rhs_r,              3},
    {"sir_vaccine_set_initial",        (DL_FUNC) &sir_vaccine_set_initial,        4},
    {"sir_vaccine_set_user",           (DL_FUNC) &sir_vaccine_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_h5n1speed(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
