

extern int
engine(
  TImageSynthParameters parameters,
  TFormatIndices* indices,
  Map* targetMap,
  Map* corpusMap,
  void (*progressCallback)(int, void*),   // int percentDone, void *contextInfo
  void *contextInfo
  );
