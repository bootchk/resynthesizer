


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
 * and passing both a callback function and its parameters (the ProgressRecord) to synthesize()
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


#ifdef SYNTH_THREADED
/*
  Threaded version.
   
  This function has local variables that are threadsafe, 
  but progressRecord is in the parent thread and must be synchronized.
  */
  void
  deepProgressCallbackThreaded(ProgressRecordT * progressRecord)
  {
    guint percentComplete;
    
    // Thread-safe increment completedPixelCount
    (void)__sync_add_and_fetch(&progressRecord->completedPixelCount, IMAGE_SYNTH_CALLBACK_COUNT);

    percentComplete = ((float)progressRecord->completedPixelCount/progressRecord->estimatedPixelCountToCompletion)*100;
    if ( percentComplete > progressRecord->priorReportedPercentComplete )
    {
      // mutex lock for two reasons:
      // 1) calls (in progressCallback()) to libgmp, gdk, gtk which are thread aware but not thread safe
      // 2) thread-safe incrementing global variable priorReportedPercentComplete
      // Note threads can still underreport percent complete but it is inconsequential.

      // pass pointer to mutex
      g_mutex_lock(progressRecord->mutexProgress);       
      // Alternatively, use gdk_thread_enter()

      // Forward deep progress callback to calling process
      progressRecord->progressCallback(
        (int) percentComplete, 
        progressRecord->context);

      progressRecord->priorReportedPercentComplete = percentComplete;
      g_mutex_unlock(progressRecord->mutexProgress);
    }
  }
#endif


void 
initializeProgressRecord(
     ProgressRecordT* progressRecord,
     TRepetionParameters repetitionParams,
     void (*progressCallback)(int, void*),
     void * contextInfo)
{
  progressRecord->completedPixelCount = 0;
  progressRecord->priorReportedPercentComplete = 0;
  progressRecord->estimatedPixelCountToCompletion = estimatePixelsToSynth(repetitionParams);
  progressRecord->progressCallback = progressCallback;
  progressRecord->context = contextInfo;
}

void 
initializeThreadedProgressRecord(
     ProgressRecordT* progressRecord,
     TRepetionParameters repetitionParams,
     void (*progressCallback)(int, void*),
     void * contextInfo,
     GMutex *mutexProgress)
{
    // init common fields
    initializeProgressRecord(progressRecord, repetitionParams, progressCallback, contextInfo);

    // also init additional field for threading
    progressRecord->mutexProgress = mutexProgress;
}
