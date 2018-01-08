
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
};

typedef struct ProgressRecord ProgressRecordT;

void deepProgressCallback(ProgressRecordT*);
void deepProgressCallbackThreaded(ProgressRecordT*);
void initializeProgressRecord(
     ProgressRecordT* progressRecord,
     TRepetionParameters repetitionParams,
     void (*progressCallback)(int, void*),
     void * contextInfo);
     

