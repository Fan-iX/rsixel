#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>
#include <cstring>

// Structure to track sixel devices
typedef struct SixelDeviceInfo {
    int device_num;
    char *filename;
    int max_colors;
    int iter_max;
    char *background;
    char *output;
    struct SixelDeviceInfo *next;
} SixelDeviceInfo;

static SixelDeviceInfo *sixel_device_list = NULL;

// Add a device to track
static void add_sixel_device(int dev_num, const char *filename, int max_colors,
                             int iter_max, const char *background, 
                             const char *output) {
    SixelDeviceInfo *info = (SixelDeviceInfo *)malloc(sizeof(SixelDeviceInfo));
    info->device_num = dev_num;
    info->filename = strdup(filename);
    info->max_colors = max_colors;
    info->iter_max = iter_max;
    info->background = strdup(background);
    info->output = strdup(output);
    info->next = sixel_device_list;
    sixel_device_list = info;
}

// Free device info
static void free_sixel_device_info(SixelDeviceInfo *info) {
    if (info != NULL) {
        if (info->filename != NULL) free(info->filename);
        if (info->background != NULL) free(info->background);
        if (info->output != NULL) free(info->output);
        free(info);
    }
}

// Register a sixel device for tracking
extern "C" SEXP C_sixel_register(SEXP dev_num, SEXP filename, SEXP max_colors, 
                                  SEXP iter_max, SEXP background, SEXP output) {
    add_sixel_device(
        Rf_asInteger(dev_num),
        CHAR(STRING_ELT(filename, 0)),
        Rf_asInteger(max_colors),
        Rf_asInteger(iter_max),
        CHAR(STRING_ELT(background, 0)),
        CHAR(STRING_ELT(output, 0))
    );
    return R_NilValue;
}

// Get list of registered device numbers
extern "C" SEXP C_sixel_get_devices() {
    // Count devices
    int n = 0;
    SixelDeviceInfo *info = sixel_device_list;
    while (info != NULL) {
        n++;
        info = info->next;
    }
    
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n));
    int *p = INTEGER(result);
    info = sixel_device_list;
    for (int i = 0; i < n; i++) {
        p[i] = info->device_num;
        info = info->next;
    }
    
    UNPROTECT(1);
    return result;
}

// Get info for a specific device and remove it from the list
extern "C" SEXP C_sixel_pop_device(SEXP dev_num) {
    int target = Rf_asInteger(dev_num);
    
    SixelDeviceInfo **pp = &sixel_device_list;
    while (*pp != NULL) {
        if ((*pp)->device_num == target) {
            SixelDeviceInfo *found = *pp;
            *pp = found->next;
            
            // Return as a list
            SEXP result = PROTECT(Rf_allocVector(VECSXP, 5));
            SET_VECTOR_ELT(result, 0, Rf_mkString(found->filename));
            SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(found->max_colors));
            SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(found->iter_max));
            SET_VECTOR_ELT(result, 3, Rf_mkString(found->background));
            SET_VECTOR_ELT(result, 4, Rf_mkString(found->output));
            
            // Set names
            SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
            SET_STRING_ELT(names, 0, Rf_mkChar("filename"));
            SET_STRING_ELT(names, 1, Rf_mkChar("max_colors"));
            SET_STRING_ELT(names, 2, Rf_mkChar("iter_max"));
            SET_STRING_ELT(names, 3, Rf_mkChar("background"));
            SET_STRING_ELT(names, 4, Rf_mkChar("output"));
            Rf_setAttrib(result, R_NamesSymbol, names);
            
            free_sixel_device_info(found);
            UNPROTECT(2);
            return result;
        }
        pp = &(*pp)->next;
    }
    
    return R_NilValue;
}

// Check if we have any registered devices
extern "C" SEXP C_sixel_has_devices() {
    return Rf_ScalarLogical(sixel_device_list != NULL);
}

// Register routines
static const R_CallMethodDef CallEntries[] = {
    {"C_sixel_register", (DL_FUNC) &C_sixel_register, 6},
    {"C_sixel_get_devices", (DL_FUNC) &C_sixel_get_devices, 0},
    {"C_sixel_pop_device", (DL_FUNC) &C_sixel_pop_device, 1},
    {"C_sixel_has_devices", (DL_FUNC) &C_sixel_has_devices, 0},
    {NULL, NULL, 0}
};

extern "C" void R_init_rsixel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
