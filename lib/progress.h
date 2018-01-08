
/*
Types for GUI progress callbacks.
*/

#include "passes.h"


struct ProgressRecord {
  guint estimatedPixelCountToCompletion;
  guint completedPixelCount;
  guint priorReportedPercentComplete;

  void (*progressCallback)(int, void*);    // callback upstream to caller
  void * context;                          // opaque data params to caller

#ifdef SYNTH_THREADED
  // mutually exclude threads over certain other fields of struct
  GMutex *mutexProgress;
#endif
};

typedef struct ProgressRecord ProgressRecordT;

void deepProgressCallback(ProgressRecordT*);
void deepProgressCallbackThreaded(ProgressRecordT*);

void initializeProgressRecord(
     ProgressRecordT* progressRecord,
     TRepetionParameters repetitionParams,
     void (*progressCallback)(int, void*),
     void * contextInfo);

void initializeThreadedProgressRecord(
     ProgressRecordT* progressRecord,
     TRepetionParameters repetitionParams,
     void (*progressCallback)(int, void*),
     void * contextInfo,
     GMutex *mutexProgress
);
     

