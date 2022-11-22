#include <R_ext/Visibility.h>
#include "SimInf.h"

#define SIMINF_MODEL_RUN SEIRcm_run
#define SIMINF_R_INIT R_init_movenet
#define SIMINF_FORCE_SYMBOLS TRUE

/* Offset in the integer compartment state vector.  The value 0 is
 * associated with S by default, 1 is associated with E, etc. Then
 * N_COMPARTMENTS (not a compartment in the model) is associated with
 * the total number of compartments in the model by
 * default. N_COMPARTMENTS is used to determine the offset in u for
 * each node.
 */
enum {S, E, I, R, N_COMPARTMENTS};

/* Offset in the real-valued continuous state vector. */
enum {LAMBDA_I};

/* Offset in the global data (gdata) vector to parameters in the
 * model */
enum {BETA, EPSILON, GAMMA};

/**
 * Susceptible to Exposed: S -> E
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIRcm_S_to_E(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    return v[LAMBDA_I] * u[S];
}

/**
 * Exposed to Infected: E -> I
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIRcm_E_to_I(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    return gdata[EPSILON] * u[E];
}

/**
 *  Infected to Recovered: I -> R
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIRcm_I_to_R(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    return gdata[GAMMA] * u[I];
}

/**
 * SEIRcm post time step
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
static int SEIRcm_post_time_step(
    double *v_new,
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    int node,
    double t)
{
    /* Determine the pointer to the compartment state vector in the
     * first node. Use this to find the number of infected individuals
     * in the other nodes. Consider we have five nodes and that the
     * post-time-step function is called for the fourth node (node =
     * 3) (the function is called once per node), then the u vector
     * looks as follows:
     *
     * SEIRSEIRSEIRSEIRSEIR
     * ^           ^
     * |           |
     * u_0         u (this is what u is pointing at when node=3)
     *
     */
    const int *u_0 = &u[-N_COMPARTMENTS * node];

    /* Determine the number of nodes in the model. */
    const int n_nodes = (int)ldata[0];

    /* First, clear lambda_i, then iterate over all nodes and add the
     * contributions from infected individuals. Note the offset
     * 'ldata[i + 1]' for the coupling because the first item is the
     * number of nodes. */
    v_new[LAMBDA_I] = 0.0;
    for (int i = 0; i < n_nodes; i++) {
        /* Add the contribution from node i. */
        v_new[LAMBDA_I] += ldata[i + 1] * u_0[i * N_COMPARTMENTS + I];
    }

    /* Error check the new lambda_i value. */
    if (!R_FINITE(v_new[LAMBDA_I]))
        return SIMINF_ERR_V_IS_NOT_FINITE;
    if (v_new[LAMBDA_I] < 0.0)
        return SIMINF_ERR_V_IS_NEGATIVE;

    /*  Finally, if lambda_i has changed compared to the previous
     *  value, return 1 to indicate to the numerical solver that the
     *  transition rates must be updated. */
    return v[LAMBDA_I] != v_new[LAMBDA_I];
}

/**
 * Run a trajectory of the SEIRcm model.
 *
 * @param model The model.
 * @param solver The name of the numerical solver.
 * @return A model with a trajectory attached to it.
 */
static SEXP SIMINF_MODEL_RUN(SEXP model, SEXP solver)
{
    static SEXP(*SimInf_run)(SEXP, SEXP, TRFun*, PTSFun) = NULL;
    TRFun tr_fun[] = {&SEIRcm_S_to_E, &SEIRcm_E_to_I, &SEIRcm_I_to_R};

    if (!SimInf_run) {
        SimInf_run = (SEXP(*)(SEXP, SEXP, TRFun*, PTSFun))
            R_GetCCallable("SimInf", "SimInf_run");

        if (!SimInf_run) {
            Rf_error("Cannot find function 'SimInf_run'.");
        }
    }

    return SimInf_run(model, solver, tr_fun, &SEIRcm_post_time_step);
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
