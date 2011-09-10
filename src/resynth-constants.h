/* 
Constants for the resynthesizer and its plugins.
These are shared by the resynthesizer engine plugin and its GUI control panel plugin.
Constants for the inner engine are separate.
*/

/* Parameters to engine and controls, same for both. */
#define RESYNTH_PARAMETER_COUNT 13

/* Names used in the Gimp PDB */
#define RESYNTH_ENGINE_PDB_NAME "plug-in-resynthesizer"
#define RESYNTH_CONTROLS_PDB_NAME "plug-in-resynth-controls"

/* Domain for i18n */
#define RESYNTH_DOMAIN_NAME "resynthesizer"

// In the GUI, the upper limit we allow user to choose from
// The engine limit is MAX_UINT
#define RESYNTH_MAX_TRYS_PER_PIXEL 10000	/* Max probes of corpus per pixel. */

// In the GUI, the upper limit we allow user to choose from
// SHOULD be no more than the engine allows: IMAGE_SYNTH_MAX_NEIGHBORS
// Engine returns error its actual parameter is greater.
// Max size of neighborhood (patch)
#define RESYNTH_MAX_NEIGHBORS 64

