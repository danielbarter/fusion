#include <fftw3.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

int32_t conjugateIndex (int32_t index,int32_t num_rows, int32_t num_columns);


// amplitudes should be a pointer to an array of length
// row_cutoff * column_cutoff + (row_cutoff - 1) * (column_cutoff - 1)
// the first complex number should be 0

uint8_t *generateTeaLeaf( fftw_complex *amplitudes,
                          int32_t num_rows,
                          int32_t num_columns,
                          int32_t row_cutoff,
                          int32_t column_cutoff );



void freeTeaLeaf(bool *ptr);
