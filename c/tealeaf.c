#include "tealeaf.h"

bool masked(uint32_t row,
            uint32_t column,
            uint32_t num_rows,
            uint32_t num_columns,
            uint32_t cutoff)
{
  return (cutoff <= row && row <= num_rows - cutoff)
    || (cutoff <= column && column <= num_columns - cutoff);
}

bool *generateTeaLeaf(uint32_t seed,
                      uint32_t num_rows,
                      uint32_t num_columns,
                      uint32_t cutoff)
{
  fftw_complex *in, *middle, *out;
  bool *image;
  fftw_plan plan1;
  fftw_plan plan2;
  uint32_t i;
  uint32_t row, column;
  uint32_t array_size = num_rows * num_columns;

  srand(seed);


  // it would be nice to statically allocate these arrays, but the
  // fftw malloc makes sure everything is alligned right for simd
  in     = fftw_alloc_complex(array_size);
  middle = fftw_alloc_complex(array_size);
  out    = fftw_alloc_complex(array_size);

  // plans need to be created before initialization
  plan1 = fftw_plan_dft_2d(num_rows,
                           num_columns,
                           in, middle,
                           FFTW_FORWARD,
                           FFTW_ESTIMATE);

  plan2 = fftw_plan_dft_2d(num_rows,
                           num_columns,
                           middle, out,
                           FFTW_BACKWARD,
                           FFTW_ESTIMATE);

  for (i = 0; i < array_size; ++i)
    {
      in[i][0] = (double) (rand() % 2); // initialize real part
      in[i][1] = 0.0; // initialize complex part
    }

  fftw_execute(plan1);

  for (i = 0; i < array_size; ++i)
    {
      row = i / num_columns;
      column = i % num_columns;
      if ( masked(row,column,num_rows,num_columns,cutoff) )
        {
        middle[i][0] = 0.0;
        middle[i][1] = 0.0;
        }
    }

  fftw_execute(plan2);

  image = malloc(array_size * sizeof(bool));

  for (i = 0; i < array_size; ++i)
    {
      if ( out[i][0] > array_size / 2 )
        {
          image[i] = true;
        } else
        {
          image[i] = false;
        }
    }


  fftw_destroy_plan(plan1);
  fftw_destroy_plan(plan2);
  fftw_free(in);
  fftw_free(middle);
  fftw_free(out);

  return image;
}
