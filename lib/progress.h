
/*
Types for GUI progress callbacks.
*/


struct ProgressRecord {
  guint estimatedPixelCountToCompletion;
  guint completedPixelCount;
  guint priorReportedPercentComplete;

  void (*progressCallback)(int, void*);    // callback upstream to caller
  void * context;                          // opaque data params to caller
};

typedef struct ProgressRecord ProgressRecordT;

void deepProgressCallback(ProgressRecordT*);


