#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>
#include <stdio.h>


/* 
 ALTREP objects which provide a "window" into an existing
 (standard) vector without
 
 a) duplicating data
 b) violating Copy-On-Write
 */


static R_altrep_class_t window_real_class;

/* windows are ALTREPS with data fields
 
 data1: VECSXP (list) length 2
     1: (ExternalPtr canary (parent in Protected slot)
     2: REALSXP (start, length))
 data2: Expanded data SEXP (initialized to R_NilValue)
 

 If data2 (expanded SEXP) is ever not R_NilValue (R's NULL), all methods
 must hit that, as it means a writeable dataptr has been given out.

 The canary lets us ensure the reference to parent gets decremented on destruction
 of the altrep IF THIS STILL NEEDS TO OCCUR. 

 If a writable dataptr is retrieved, we populate data2 (expanded SEXP), then 
 we set the reference to parent in canary ('protected' field) to R_NilValue, 
 THEN clear the canary external pointer. 

 If the canary is still uncleared upon finalization, we do the cleanup then
 which takes care of recovering the reference count automatically.

 
 */


#define VWINDOW_PARENT(x) R_ExternalPtrProtected(VECTOR_ELT(R_altrep_data1(x), 0))
/* We always want to do this as a unit! */
#define FULL_CLEAR_EXTPTR(x) do {			\
	R_SetExternalPtrProtected(x, R_NilValue);	\
	R_ClearExternalPtr(x);				\
    } while(0)

/* this decrements the reference count for parent and then */	
/* clear's the canary */					
#define VWINDOW_UNSET_PARENT(x) FULL_CLEAR_EXTPTR(VECTOR_ELT(R_altrep_data1(x), 0))
#define VWINDOW_START(x) ((R_xlen_t) REAL_ELT(VECTOR_ELT(R_altrep_data1(x), 1), 0))
#define VWINDOW_LENGTH(x) ((R_xlen_t) REAL_ELT(VECTOR_ELT(R_altrep_data1(x), 1), 1))
#define VWINDOW_EXPANDED(x) R_altrep_data2(x)
#define VWINDOW_SET_EXPANDED(x, v) R_set_altrep_data2(x, v)

void canary_finalizer(SEXP x) {
    int *canary = (int *) R_ExternalPtrAddr(x);
    /* check if our canary is still tweeting and hopping about */
    if(canary) {
	FULL_CLEAR_EXTPTR(x);
    }
}

SEXP make_window_real(SEXP parent, SEXP start_len) {
    /* carry around a pointer to parent that we can put a finalizer on
       so we're not accumulating reference count that cant be 
       decremented.
    */

#ifndef SWITCH_TO_REFCNT
     /* NAMED wouldn't be decremented so in NAMED world just mark parent
	not mutable for safety */
     MARK_NOT_MUTABLE(parent);
#endif
    int *canarydata = malloc(sizeof(int));
    /* there was a bug in R_MakeExternalPtr which didn't increment ref counts of Prot,
       but setter did. Fixed already in R-devel and R-patched but initializing to
       R_NilValue then setting it works backwards-compatibly. */
    SEXP canary = R_MakeExternalPtr(canarydata, R_NilValue, R_NilValue);
    R_SetExternalPtrProtected(canary, parent);
    R_RegisterCFinalizerEx(canary, canary_finalizer, TRUE);
    SEXP mdata = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(mdata, 0, canary);
    SET_VECTOR_ELT(mdata, 1, start_len);
    R_altrep_class_t cls = window_real_class;
    SEXP ans = R_new_altrep(cls, mdata, R_NilValue);
    UNPROTECT(1); /* mdata */
    return ans;
}


static SEXP vwindow_Serialized_state(SEXP x) {
    /* 
     * no serializing windows as altreps, 
     * will be converted to std vec
     */
    warning("Not serializing window vector as ALTREP, duplicating data");
    return NULL;
}



Rboolean vwindow_Inspect(SEXP x, int pre, int deep, int pvec,
                         void (*inspect_subtree)(SEXP, int, int, int))
{
    Rprintf(" window %s", type2char(TYPEOF(x)));
    if(VWINDOW_EXPANDED(x) != R_NilValue)
	Rprintf(" [ expanded ]\n");
    else 
	Rprintf(" [par %p strt: %ld len: %ld]\n", 
            (void *) VWINDOW_PARENT(x), 
		VWINDOW_START(x) + 1, /* so that it is in R indexing */ 
		VWINDOW_LENGTH(x));
    return TRUE;
}



static R_xlen_t vwindow_Length(SEXP x)
{
    return VWINDOW_LENGTH(x);
}

static void *vwindow_Dataptr(SEXP x, Rboolean writeable)
{
    SEXP exp = VWINDOW_EXPANDED(x);
    if(exp != R_NilValue) {
	/* 
	 * we already lost our ALTREPness, no sense in pretending
	 * otherwise now, just operate on the expanded version
	 */
	return REAL(exp);
    }
    R_xlen_t len = VWINDOW_LENGTH(x),
	start = VWINDOW_START(x),
	ncopy;
    if(!writeable) {
	/* 
	 * no reason to expand things and get all upset about it,
	 * for read-only case shifted pointer to parent is ok
	 */
	return REAL_RO(VWINDOW_PARENT(x)) + start;
    }
    
    /* 
     * here they want a writing pointer, that means expanding
     * the altrep, copying the data, the whole shebang
     */
    SEXP ans = PROTECT(allocVector(REALSXP, len));
    double *buff = REAL(ans);
    
    ncopy = REAL_GET_REGION(VWINDOW_PARENT(x), start, len, buff);
    if(ncopy != len)
	error("Retrieving data pointer appears to have failed");
    VWINDOW_SET_EXPANDED(x, ans);
    VWINDOW_UNSET_PARENT(x);
    UNPROTECT(1);
    return REAL(ans);
}

static const void *vwindow_Dataptr_or_null(SEXP x)
{
    /* already expanded, so just do that */
    SEXP exp = VWINDOW_EXPANDED(x);
    if(exp != R_NilValue) {
	return REAL_RO(exp);
    }
    
    /* no thanks I like being an ALTREP */
    return NULL;
}

static double vwindow_real_Elt(SEXP x, R_xlen_t i) {
    
    SEXP vec;
    SEXP exp = VWINDOW_EXPANDED(x);
    if(exp != R_NilValue) {
	/* super bummer I'm not special anymore */
	vec = exp;
    } else {
	vec = VWINDOW_PARENT(x);
	i = i + VWINDOW_START(x);
    }
    return REAL_ELT(vec, i);
}

static
R_xlen_t vwindow_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buff) {
    SEXP exp = VWINDOW_EXPANDED(sx);
    if(exp != R_NilValue) {
	/* I miss being a window :( */
	return REAL_GET_REGION(exp, i, n, buff);
    }
    /* this could be VWINDOW_LENGTH but general XLENGTH is better */
    R_xlen_t xlen = XLENGTH(sx); 
    R_xlen_t ncopy = xlen - i > n ? n : xlen - i;
    R_xlen_t offset = VWINDOW_START(sx) + i;
    double *parentptr = REAL(VWINDOW_PARENT(sx));
    for(R_xlen_t j = 0; j < ncopy; j++)
	buff[j] = parentptr[offset + j];
    return ncopy;
}

static int vwindow_real_Is_sorted(SEXP x) {
    int ans;
    if(VWINDOW_EXPANDED(x) != R_NilValue)
	ans = UNKNOWN_SORTEDNESS;
    else
	ans = REAL_IS_SORTED(VWINDOW_PARENT(x));
    
    return ans;
}

static int vwindow_real_No_NA(SEXP x) {
    int ans;
    if(VWINDOW_EXPANDED(x) != R_NilValue) {
	ans = 0;
    } else {
	ans = REAL_NO_NA(x);
    }
    return ans;
}

static void InitVWindowRealClass(DllInfo *dll)
{
    R_altrep_class_t cls = 
	R_make_altreal_class("vwindow_real", "vectorwindow", dll);
    
    window_real_class = cls;
    
    /* note the differences after R_set_ below */
    
    /* ALTREP methods */
    R_set_altrep_Inspect_method(cls, vwindow_Inspect);
    R_set_altrep_Length_method(cls, vwindow_Length);
    R_set_altrep_Serialized_state_method(cls, vwindow_Serialized_state);
    
    /* ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, vwindow_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, vwindow_Dataptr_or_null);
    
    /* ALTREAL methods */
    R_set_altreal_Elt_method(cls, vwindow_real_Elt);
    R_set_altreal_Get_region_method(cls, vwindow_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, vwindow_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, vwindow_real_No_NA);
}

/*
 * Shared Library Initialization and Finalization
 */

static const R_ExternalMethodDef ExtEntries[] = {
    {"make_window_real", (DL_FUNC) &make_window_real, -1},
    {NULL, NULL, 0}
};

void R_init_vectorwindow(DllInfo *dll)
{
    InitVWindowRealClass(dll);
    
    R_registerRoutines(dll, NULL, NULL, NULL, ExtEntries);
}



