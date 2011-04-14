/*
MMX code for SIMD vectorized innermost loop of resynthesizer
*/

#ifdef MMX_INTRINSICS_RESYNTH

  static __m64 temp3;   // automatically aligned 8
  
  /*
  Compute difference of target and corpus pixels in parallel 
  Eight bytes at once.
  
  There must be padding after the last pixel if it is not eight bytes.
  */
  temp3 = _mm_xor_si64(
                _mm_subs_pu8 ( *((__m64*) corpus_pixel), *((__m64*) image_pixel)),
                _mm_subs_pu8 ( *((__m64*) image_pixel), *((__m64*) corpus_pixel))
              );
  
  /* Using the lookup tables is not vectorizeable. */
  if (i)  // If not the target point (its own 0th neighbor)
  {
    for(BppType j=FIRST_PIXELEL_INDEX; j<color_end_bip; j++)
      sum += diff_table2[((unsigned char *)(&temp3))[j]];
  }
  if (map_bpp > 0)
    for(BppType j=map_start_bip; j<map_end_bip; j++)  // also sum mapped difference
      sum += map_diff_table2[((unsigned char *)(&temp3))[j]];
  
#endif
