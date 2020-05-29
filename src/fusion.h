#include <stdint.h>

typedef uint32_t boundaryLabel;
typedef uint32_t tileIndex;

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
  tile *tiles;                 // array of tiles
  tileIndex numberOfTiles;     // number of tile (0 indicates no tiles)
  tileIndex vacuumTile;        // index of the vacuum tile
} tileStore;

/* boundary pattern for local relations
 *
 *   3 2
 *  4   1
 *  5   0
 *   6 7
 *
 * tile pattern for local relations
 *
 *   2 1
 *   3 0
 *
 */

// local relations are stored in a hash table
typedef struct localRelation_s {
  boundaryLabel boundary[8];       // boundary labels counterclockwise
  tileIndex tiles[4];              // tile indices counterclockwise
  struct localRelation_s *next;    // next local relation
} localRelation;

typedef struct localRelationStore_s {
  uint32_t numberOfBins;
  localRelation **table;
} localRelationStore;
