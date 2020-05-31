#include <fftw3.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h> // delete me later

#define NUM_PIXELS 420
#define FREQUENCY_CUTOFF 5

bool masked(uint32_t row, uint32_t column);
bool *generateTeaLeaf(uint32_t seed);
