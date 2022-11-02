#include <R_ext/Visibility.h>
#include "SimInf.h"

#define SIMINF_MODEL_RUN SEIR_cm_run
#define SIMINF_R_INIT R_init_movenet
#define SIMINF_FORCE_SYMBOLS TRUE

/* Offset in the integer compartment state vector. */
enum {S, E, I, R, N_COMPARTMENTS};

/**
 * susceptible to exposed: S -> E
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIR_cm_S_to_E(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    /* FIXME */
    return 0.0;
}

/**
 * exposed to infected: E -> I
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIR_cm_E_to_I(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    /* FIXME */
    return 0.0;
}

/**
 *  infected to recovered: I -> R
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIR_cm_I_to_R(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    /* FIXME */
    return 0.0;
}

/**
 * SEIR_cm post time step
 *
 * Update the force of infection from the contact matrix.
 *
 * @param v_new The continuous state vector in the node after the post
 * time step
 * @param u The compartment state vector in the node.
 * @param v The current continuous state vector in the node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param node The node.
 * @param t The current time.
 * @return error code (<0), or 1 if node needs to update the
 * transition rates, or 0 when it doesn't need to update the
 * transition rates.
 */
static int SEIR_cm_post_time_step(
    double *v_new,
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    int node,
    double t)
{
    /* FIXME */
    return 0.0;
}

/**
 * Run a trajectory of the SEIR_cm model.
 *
 * @param model The model.
 * @param solver The name of the numerical solver.
 * @return A model with a trajectory attached to it.
 */
static SEXP SIMINF_MODEL_RUN(SEXP model, SEXP solver)
{
    static SEXP(*SimInf_run)(SEXP, SEXP, TRFun*, PTSFun) = NULL;
    TRFun tr_fun[] = {&SEIR_cm_S_to_E, &SEIR_cm_E_to_I, &SEIR_cm_I_to_R};

    if (!SimInf_run) {
        SimInf_run = (SEXP(*)(SEXP, SEXP, TRFun*, PTSFun))
            R_GetCCallable("SimInf", "SimInf_run");

        if (!SimInf_run) {
            Rf_error("Cannot find function 'SimInf_run'.");
        }
    }

    return SimInf_run(model, solver, tr_fun, &SEIR_cm_post_time_step);
}

/**
 * A NULL-terminated array of routines to register for the .Call
 * interface, see section '5.4 Registering native routines' in
 * the 'Writing R Extensions' manual.
 */
static const R_CallMethodDef callMethods[] =
{
    SIMINF_CALLDEF(SIMINF_MODEL_RUN, 2),
    {NULL, NULL, 0}
};

/**
 * This routine will be invoked when R loads the shared object/DLL,
 * see section '5.4 Registering native routines' in the
 * 'Writing R Extensions' manual.
 */
void SIMINF_R_INIT(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, SIMINF_FORCE_SYMBOLS);
}
