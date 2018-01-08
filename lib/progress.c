


// Compiling switch #defines
#include "buildSwitches.h"

#ifdef SYNTH_USE_GLIB
  #include "../config.h" // GNU buildtools local configuration
  // Use glib via gimp.h
  // #include <libgimp/gimp.h>
  #include <glib.h>
#endif

#include "imageSynthConstants.h"
#include "progress.h"

/*
 * Intermediate between deeper engine and calling app's progress callback.
 * Called from inside synthesis() every 4k target pixels i.e. with raw progress increments.
 * Knows how to convert accumlated raw progress to a percent progress.
 * Percent progress is ratio of accumulate raw progress to estimated total pixels to synthesis.
 * Callback's to invoking process every 1 percent.

 * Note synthesis may quit early: then progress makes a large jump.
 * 
 * An earlier implementation used a nested function (a gcc extension)
 * which many people consider unsafe.
 * Here the same result (the context of the call is known to the progress callback)
 * is implemented by putting all the progress data (including the app's callback and parameters (context) for that call)
 * into a ProgressRecord
 * and passing both a callback function and its parameters to synthesize()
 */
void
deepProgressCallback(ProgressRecordT * progressRecord)
{
 // !!! Note if estimatedPixelCountToCompletion is small
 // this calls back once for each pass with a percentComplete greater than 100.
 progressRecord->completedPixelCount += IMAGE_SYNTH_CALLBACK_COUNT;
 guint percentComplete = ((float)progressRecord->completedPixelCount/progressRecord->estimatedPixelCountToCompletion)*100;
 if ( percentComplete > progressRecord->priorReportedPercentComplete )
 {
   progressRecord->progressCallback((int) percentComplete, progressRecord->context);  // Forward callback to calling process
   progressRecord->priorReportedPercentComplete = percentComplete;
 }
}


