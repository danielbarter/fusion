#include <stdint.h>

typedef uint32_t boundaryLabel;
typedef uint32_t tileIndex;


/*
 * the input for funsion.elf is a folder containing:
 * 1. the tile svg files
 * 2. a manifest.csv which contains a list of tiles and their boundary labels
 * and an output file name.
 * the tile names shouldn't have any spaces
 */


/* boundary pattern for tiles
 *    1
 *   2 0
 *    3
 * this is the order of the boundary labels in manifest.csv
 */

typedef struct tile_s {
  char *svg;                  // file path for tile relative to input directory
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
