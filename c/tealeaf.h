#include <fftw3.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

bool masked(uint32_t row,
            uint32_t column,
            uint32_t num_rows,
            uint32_t num_columns,
            uint32_t cutoff);

bool *generateTeaLeaf(uint32_t seed,
                      uint32_t num_rows,
                      uint32_t num_columns,
                      uint32_t cutoff);
