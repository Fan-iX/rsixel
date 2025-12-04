/*
 * rsixel - SIXEL graphics device for R
 * 
 * This module provides C++ functions to track sixel graphics devices
 * and their associated parameters. When a sixel device is closed,
 * the stored parameters are used to encode the output as SIXEL.
 */

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
// Returns 0 on success, -1 on memory allocation failure
static int add_sixel_device(int dev_num, const char *filename, int max_colors,
                            int iter_max, const char *background, 
                            const char *output) {
    SixelDeviceInfo *info = (SixelDeviceInfo *)malloc(sizeof(SixelDeviceInfo));
    if (info == NULL) {
        return -1;
    }
    
    info->device_num = dev_num;
    info->filename = strdup(filename);
    info->background = strdup(background);
    info->output = strdup(output);
    
    // Check for strdup failures
    if (info->filename == NULL || info->background == NULL || info->output == NULL) {
        if (info->filename != NULL) free(info->filename);
        if (info->background != NULL) free(info->background);
        if (info->output != NULL) free(info->output);
        free(info);
        return -1;
    }
    
    info->max_colors = max_colors;
    info->iter_max = iter_max;
    info->next = sixel_device_list;
    sixel_device_list = info;
    return 0;
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

/*
 * Register a sixel device for tracking.
 * 
 * Parameters:
 *   dev_num: Device number (integer)
 *   filename: Path to temporary PNG file (character)
 *   max_colors: Maximum colors for SIXEL palette (integer)
 *   iter_max: Maximum iterations for k-means (integer)
 *   background: Background color for alpha blending (character)
 *   output: Output file path, "" for stdout (character)
 * 
 * Returns R_NilValue on success, throws an error on failure.
 */
extern "C" SEXP C_sixel_register(SEXP dev_num, SEXP filename, SEXP max_colors, 
                                  SEXP iter_max, SEXP background, SEXP output) {
    // Validate inputs
    if (Rf_length(filename) < 1 || Rf_length(background) < 1 || Rf_length(output) < 1) {
        Rf_error("Invalid input: string parameters must have length >= 1");
    }
    
    int result = add_sixel_device(
        Rf_asInteger(dev_num),
        CHAR(STRING_ELT(filename, 0)),
        Rf_asInteger(max_colors),
        Rf_asInteger(iter_max),
        CHAR(STRING_ELT(background, 0)),
        CHAR(STRING_ELT(output, 0))
    );
    
    if (result != 0) {
        Rf_error("Failed to allocate memory for sixel device");
    }
    
    return R_NilValue;
}

/*
 * Get list of registered device numbers.
 * Returns an integer vector of device numbers.
 */
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

/*
 * Get info for a specific device and remove it from the list.
 * 
 * Parameters:
 *   dev_num: Device number to look up (integer)
 * 
 * Returns a named list with device parameters, or R_NilValue if not found.
 */
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

/*
 * Check if we have any registered devices.
 * Returns TRUE if there are registered devices, FALSE otherwise.
 */
extern "C" SEXP C_sixel_has_devices() {
    return Rf_ScalarLogical(sixel_device_list != NULL);
}

/*
 * R package initialization function.
 * Registers the C callable functions with R.
 */
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
