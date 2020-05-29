#include <stdint.h>

typedef uint32_t boundaryLabel;

/* boundary pattern for tiles
 *    1
 *   2 0
 *    3
 */

typedef struct tile_s {
  char *svg;                  // file path for tile svg
  boundaryLabel boundary[4];  // boundary labels counterclockwise starting from right
} tile;


// we refer to tiles by their index in the tileStore
typedef struct tileStore_s {
  tile *tiles;                // array of tiles
  uint32_t numberOfTiles;     // number of tile (0 indicates no tiles)
  uint32_t vacuumTile;        // index of the vacuum tile
} tileStore;

/* boundary pattern for local relations
 *
 *   32
 *  4  1
 *  5  0
 *   67
 *
 */

typedef struct localRelation_s {
  uint32_t boundary[8];       // boundary labels counterclockwise starting 
} localRelation;
