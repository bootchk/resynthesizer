/* 
Constants for the resynthesizer and its plugins.
These are shared by the resynthesizer engine and its GUI control panel plugin.

TODO
Default parameters should be constants.
Other magic numbers should be constants.
*/

/* Constraints on parameters to plugin. */
// Max size of neighborhood (patch)
// Allocated on stack in reentrant version
#define RESYNTH_MAX_NEIGHBORS 64	
#define RESYNTH_MAX_TRYS_PER_PIXEL 10000	/* Max probes of corpus per pixel. */

/* Parameters to engine and controls, same for both. */
#define RESYNTH_PARAMETER_COUNT 13

/* Names used in the Gimp PDB */
#define RESYNTH_ENGINE_PDB_NAME "plug-in-resynthesizer"
#define RESYNTH_CONTROLS_PDB_NAME "plug-in-resynth-controls"
/* Domain for i18n */
#define RESYNTH_DOMAIN_NAME "resynthesizer"


/*
Gimp constants??
!!! Partially selected and partially transparent.
Be sure resynthesizer treats partial selection and transparency sensibly.
*/
#define MASK_TOTALLY_SELECTED 0xFF
#define MASK_UNSELECTED 0

#define ALPHA_TOTAL_TRANSPARENCY 0

#define PIXELEL_BLACK 0


/* 
These specify the layout of our Pixel, not the same as Gimp !!!
e.g. MRGBW
These are the only constants: other counts and indexes are dynamic:
  index of the first and last color and map pixelels
  index of the alpha pixelel
  count of color and map pixelel
*/
#define MASK_PIXELEL_INDEX  0
#define FIRST_PIXELEL_INDEX 1	/* Starting color pixelel (usually Red) */
/* 
The most pixelels resynth will store and use to match. 
1 mask + 3 colors + 1 alpha + 3 map colors
*/
#define MAX_RESYNTH_BPP 8


/*
Constants of the resynthesis algorithm.
TODO in future versions, these might be parameters of the plugin.
*/

/*
Maximum weight for pixel difference.
Was diff_table[0] which was typically 65418, for the setting that affects weighting (autism.)
65535 is max number representable in 16-bit unsigned int.
!!! Note map_diff_table is uses map_diff_table[0] for the max value.
The map_diff_table is not gushort and values depend on a user given parameter.
*/
#define MAX_WEIGHT G_MAXUSHORT
#define MAP_MULTIPLIER 4.0

/*
The fraction of target points that must be bettered on a pass
else terminate repeated passes over the target.
*/
#define RESYNTH_TERMINATE_FRACTION 0.1

/*
The fraction ( count of points in a band/ total target points) 
for banded randomization of target points.
*/
#define RESYNTH_BAND_FRACTION 0.1
