/*
Parameters of the innermost image synthesis engine.
Same as engine parameters to be passed in the SimpleAPI.

Also error return values of engine.
*/

#ifndef FALSE
  #define FALSE 0
#endif


typedef enum  ImageSynthError 
{
  IMAGE_SYNTH_SUCCESS,
  // Programmer error
  IMAGE_SYNTH_ERROR_INVALID_IMAGE_FORMAT, // Returned by SimpleAPI adapter
  IMAGE_SYNTH_ERROR_IMAGE_MASK_MISMATCH,  // "
  // Programmer error, parameter errors returned by inner engine
  IMAGE_SYNTH_ERROR_PATCH_SIZE_EXCEEDED,  
  IMAGE_SYNTH_ERROR_MATCH_CONTEXT_TYPE_RANGE,
  // IN data errors, user error in making selection? returned by inner engine
  IMAGE_SYNTH_ERROR_EMPTY_TARGET,
  IMAGE_SYNTH_ERROR_EMPTY_CORPUS,
  // There are more errors returned by the GIMP adapter
  // There will be more errors returned by a future FullAPI adapter, similar to GIMP adapter errors
  // These are only pertinent for the FullAPI, when more than one image is passed
} TImageSynthError;


typedef struct ImageSynthParametersStruct {
  
  /*
  Boolean.  Whether to synthesize the target so it is subsequently seamlessly tileable.
  This is only pertinenent if isMatchContext is False (when there is no context of the target.)
  */
  int isMakeSeamlesslyTileableHorizontally;
  int isMakeSeamlesslyTileableVertically;
  /*
  Whether to synthesize the target so it matches the context of the target, if there is any.
  For the SimpleAPI, there should always be a context, otherwise, the corpus (which is the context) is empty,
  and this should be TRUE.
  For the AdvancedAPI, the target might not have a context.
  If there is no context, this is moot.
  If there is a context, set it according to whether you want the synthesize target to blend into the context.
  0 Don't match context
  1 Match context but choose corpus entirely at random
  2 Match context and synthesize randomly but in bands inward (from surrounding context.)
  3 etc. see ...orderTarget()
  */
  int matchContextType;   

  /*
  For the advanced API, when maps are passed tothe engine,
  the weight to give to the matching of the maps for the target and corpus,
  as opposed to the weight  given to the matching of the target and the corpus themselves.
  Multiplication factor the the map metric.
  Scales the map metric function to return greater or lesser values
  in relation to the target/corpus metric funtion.
  */
  double mapWeight;
  
  /*
  A parameter of the statistical function for weighting pixel differences.
  AKA autism
  */
  double sensitivityToOutliers;
  
  /*
  Size of the patch matched, in pixels.
  Formerly called neighbors (but it includes the pixel being synthesized, which is not strictly a neighbor.)
  A factor in the complexity of the algorithm.
  Typically a square: 9, 16, 25, 36, 49, 64.
  But patches need not be square, indeed are NOT rectangular early in the algorithm.
  */
  unsigned int patchSize;
  
  /*
  The maximum count of probes per pixel per pass.
  Generally, this count of probes is done per pixel per pass,
  except if an exact match is found, which ends probing.
  A factor in the complexity of the algorithm.
  Typically in the hundreds.
  */
  unsigned int maxProbeCount;
} TImageSynthParameters;




extern void
setDefaultParams(
  TImageSynthParameters* param
  );
  
