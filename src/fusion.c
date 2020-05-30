#include "fusion.h"


int32_t main(int32_t argc, char **argv)
{
  if (argc != 3)
    {
      puts("usage: fusion.elf tileFolder seed");
      puts("expected two arguments");
      return 1;
    }

  return 0;
}
