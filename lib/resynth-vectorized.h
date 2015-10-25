/*
MMX code for SIMD vectorized innermost loop of resynthesizer
*/

#ifdef MMX_INTRINSICS_RESYNTH

  static __m64 temp3;   // automatically aligned 8
  
  /*
  Compute difference of target and corpus pixels in parallel 
  Eight bytes at once.
  
  There must be padding after the last pixel if it is not eight bytes.
  
  Unsigned 8-bit difference is (saturated subtract) xor (saturated subtract)
  */
  temp3 = _mm_xor_si64(
                _mm_subs_pu8 ( *((__m64*) corpus_pixel), *((__m64*) image_pixel)),
                _mm_subs_pu8 ( *((__m64*) image_pixel), *((__m64*) corpus_pixel))
              );
  
  /* Using the lookup tables is not vectorizeable. */
  /* OR... use PEXTRW and PINSRW to unpack into indexes,
  lookup in a loop
  pack back into a vector,
  and sum in a vector.
  _mm_extract_pi16 
  */
  TPixelelIndex j;
  if (i)  // If not the target point (its own 0th neighbor)
  {
    
    for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
      sum += corpusTargetMetric[((unsigned char *)(&temp3))[j]];
  }
  if (indices->map_match_bpp > 0)
    for(j=indices->map_start_bip; j<indices->map_end_bip; j++)  // also sum mapped difference
      sum += mapsMetric[((unsigned char *)(&temp3))[j]];
  
#endif
