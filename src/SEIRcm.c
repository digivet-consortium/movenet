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

/* Offset in the real-valued continuous state vector (v). */
enum {LAMBDA_I};

/* Offset in the local data (ldata) vector to parameters in the
 * model */
enum {EPSILON_RATE, EPSILON_SHAPE, GAMMA_RATE, GAMMA_SHAPE, N_CONTACTS, CONTACTS};

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
    SIMINF_UNUSED(ldata);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (u[S] == 1 &&
        u[E] == 0 &&
        u[I] == 0 &&
        u[R] == 0)
    {
        return v[LAMBDA_I];
    }

    return 0;
}

/**
 * Exposed stages: E_{n} -> E_{n+1}
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIRcm_E_stages(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (u[S] == 1 &&
        u[E]  > 0 && u[E] < (int)ldata[EPSILON_SHAPE] &&
        u[I] == 0 &&
        u[R] == 0)
    {
        return ldata[EPSILON_RATE];
    }

    return 0;
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
    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (u[S] == 1 &&
        u[E]  > 0 && u[E] == (int)ldata[EPSILON_SHAPE] &&
        u[I] == 0 &&
        u[R] == 0)
    {
        return ldata[EPSILON_RATE];
    }

    return 0;
}

/**
 *  Infected stages: I_{n} -> I_{n+1}
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SEIRcm_I_stages(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (u[S] == 1 &&
        u[E]  > 0 && u[E] > (int)ldata[EPSILON_SHAPE] &&
        u[I]  > 0 && u[I] < (int)ldata[GAMMA_SHAPE]   &&
        u[R] == 0)
    {
        return ldata[GAMMA_RATE];
    }

    return 0;
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
    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (u[S] == 1 &&
        u[E]  > 0 && u[E]  > (int)ldata[EPSILON_SHAPE] &&
        u[I]  > 0 && u[I] == (int)ldata[GAMMA_SHAPE]   &&
        u[R] == 0)
    {
        return ldata[GAMMA_RATE];
    }

    return 0;
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

    /* Determine the number of contacts in the contact matrix. */
    const int n_contacts = (int)ldata[N_CONTACTS];

    /* First, clear lambda_i, then iterate over all nodes in the
     * contact matrix and add the contributions from infected
     * nodes. */
    v_new[LAMBDA_I] = 0.0;
    for (int i = 0, j = 0; i < n_contacts; i++, j += N_COMPARTMENTS) {
        /* Check if node i is in infected stage. */
        if (u_0[j + S] == 1 &&
            u_0[j + E]  > 0 && u_0[j + E]  > (int)ldata[EPSILON_SHAPE] &&
            u_0[j + I]  > 0 && u_0[j + I] <= (int)ldata[GAMMA_SHAPE]   &&
            u_0[j + R] == 0)
        {
            /* Add the contribution from node i. */
            v_new[LAMBDA_I] += ldata[i + CONTACTS];
        }
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
    TRFun tr_fun[] = {
        &SEIRcm_S_to_E,
        &SEIRcm_E_stages,
        &SEIRcm_E_to_I,
        &SEIRcm_I_stages,
        &SEIRcm_I_to_R};

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
