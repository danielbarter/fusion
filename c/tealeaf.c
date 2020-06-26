#include "tealeaf.h"

int32_t conjugateIndex (int32_t index,int32_t num_rows, int32_t num_columns)
{
  int32_t row = index / num_columns;
  int32_t column = index % num_columns;
  int32_t conjugate_row = ( num_rows - row ) % num_rows;
  int32_t conjugate_col = ( num_columns - column ) % num_columns;
  return conjugate_row * num_columns + conjugate_col;
}

uint8_t *generateTeaLeaf( fftw_complex *amplitudes,
                          int32_t num_rows,
                          int32_t num_columns,
                          int32_t row_cutoff,
                          int32_t column_cutoff )
{
  fftw_complex *in, *out;
  uint8_t *image;
  fftw_plan plan;
  int32_t array_size = num_rows * num_columns;
  int32_t amplitude_array_size = row_cutoff * column_cutoff + (row_cutoff -1) * (column_cutoff - 1);
  int32_t i;
  int32_t amplitude_index = 0;
  int32_t *fundamental_domain;
  int32_t row,column;
  int32_t index;
  int32_t conjugate_index;

  in = fftw_alloc_complex(array_size);
  out = fftw_alloc_complex(array_size);

  plan = fftw_plan_dft_2d(num_rows,num_columns,in,out,FFTW_BACKWARD,FFTW_ESTIMATE);

  // initialize fundamental domain
  fundamental_domain = malloc(amplitude_array_size * sizeof(int32_t));
  for (row = 0; row < row_cutoff; row++)
    {
      for (column = 0; column < column_cutoff; column++)
        {
          fundamental_domain[amplitude_index] = row * num_columns + column;
          amplitude_index++;
        }
    }

  for (row = 1; row < row_cutoff; row++)
    {
      for (column = 1; column < column_cutoff; column++)
        {
          fundamental_domain[amplitude_index] = row * num_columns + (num_columns - column);
          amplitude_index++;
        }
    }

  // initializing input array
  for (i = 0; i < array_size; ++i)
    {
      in[i][0] = 0.0;
      in[i][1] = 0.0;
    }

  // setting amplitudes
  for (amplitude_index = 0; amplitude_index < amplitude_array_size; amplitude_index++)
    {
      index =  fundamental_domain[amplitude_index];
      conjugate_index = conjugateIndex( index, num_rows, num_columns );
      in[index][0] = amplitudes[amplitude_index][0];
      in[index][1] = amplitudes[amplitude_index][1];
      in[conjugate_index][0] = amplitudes[amplitude_index][0];
      in[conjugate_index][1] = - amplitudes[amplitude_index][1];
    }

  fftw_execute(plan);

  image = malloc(array_size * sizeof(bool));

  for (i = 0; i < array_size; ++i)
    {
      if ( out[i][0] > 0 )
        {
          image[i] = 255;
        } else
        {
          image[i] = 0;
        }
    }

  fftw_destroy_plan(plan);
  fftw_free(in);
  fftw_free(out);
  free(fundamental_domain);

  return image;

}

void freeTeaLeaf(bool *ptr)
{
  free(ptr);
}
