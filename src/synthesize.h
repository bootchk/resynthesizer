/*
The heart of the algorithm.
Called repeatedly: many passes over the data.
*/
static guint
synthesize(
  guint pass,
  Parameters *parameters, // IN,
  // GimpDrawable *drawable,  // IN for ANIMATE
  TRepetionParameters repetition_params,
  TFormatIndices* indices,
  Map * targetMap
  )
{
  guint target_index;
  Coordinates position;
  gboolean is_perfect_match;
  guint repeatCountBetters = 0;
  
  
  /* ALT: count progress once at start of pass countTargetTries += repetition_params[pass][1]; */
  reset_color_change();
  
  for(target_index=0; target_index<repetition_params[pass][1]; target_index++) 
  {
    countTargetTries += 1;  /* ALT count progress at each target point. */
    
    #ifdef PROGRESS
    if ((target_index&4095) == 0) 
    {
      /* Progress over all passes, not just within this pass.
      Towards the maximum expected tries, but we might omit latter passes.
      */
      gimp_progress_update((float)countTargetTries/total_targets);
      #ifdef ANIMATE
        post_results_to_gimp(drawable);
      #endif
    }
    #endif
    
    position = g_array_index(target_points, Coordinates, target_index);
     
    /*
    This means we are about to give it a value (and a source),
    but also means that below, we put offset (0,0) in vector of neighbors !!!
    i.e. this makes a target point it's own neighbor (with a source in later passes.)
    */
    set_has_value(&position, TRUE);  
    
    prepare_neighbors(position, parameters, indices, targetMap);
    
    /*
    Repeat a pixel even if found an exact match last pass, because neighbors might have changed.
    
    On passes after the first, we don't explicitly start with best of the previous match,
    but since a pixel is it's own first neighbor, the first best calculated will a be
    from the source that gave the previous best, and should be a good starting best.
    */
    best = G_MAXUINT; /* A very large positive number.  Was: 1<<30 */       
    is_perfect_match = FALSE;
    latestBettermentKind = NO_BETTERMENT;
    
    /*
    Heuristic 1, try neighbors of sources of neighbors of target pixel.
    
    Subtle: The target pixel is its own first neighbor (offset 0,0).
    It also has_value (since we set_has_value() above, but it really doesn't have color on the first pass.)
    On the first pass, it has no source.
    On subsequent passes, it has a source and thus its source is the first corpus point to be tried again,
    and that will set best to a low value!!!
    */
    {
    guint neighbor_index;
    
    for(neighbor_index=0; neighbor_index<n_neighbours && best != 0; neighbor_index++)
      // If the neighbor is in the target (not the context) and has a source in the corpus
      if ( has_source_neighbor(neighbor_index) ) {
        /*
        Coord arithmetic: corpus source minus neighbor offset.
        corpus_point is a pixel in the corpus with opposite offset to corpus source of neighbor
        as target position has to this target neighbor.
        !!! Note corpus_point is raw coordinate into corpus: might be masked.
        !!! It is not an index into unmasked corpus_points.
        */
        Coordinates corpus_point = subtract_points(neighbour_source[neighbor_index], neighbours[neighbor_index]);
        
        /* !!! Must clip corpus_point before further use, its only potentially in the corpus. */
        if (clippedOrMaskedCorpus(corpus_point)) continue;
        if (*intmap_index(&tried, corpus_point) == target_index) continue;  /* Heuristic 2 */
        is_perfect_match = try_point(corpus_point, NEIGHBORS_SOURCE, indices);
        if ( is_perfect_match ) break;  // Break neighbors loop
        
        /*
        !!! Remember we tried corpus pixel point for target point target_index.
        Heuristic 2: all target neighbors with values might come from the same corpus locus.
        */
        *intmap_index(&tried, corpus_point) = target_index;
      }
      // Else the neighbor is not in the target (has no source) so we can't use the heuristic 1.
    }
      
    if ( ! is_perfect_match )
    {
      /* Try random source points from the corpus */
      gint j;
      for(j=0; j<parameters->trys; j++)
      {
        is_perfect_match = try_point(random_corpus_point(), RANDOM_CORPUS, indices);
        if ( is_perfect_match ) break;  /* Break loop over random corpus points */
        /* Not set tried(point) because that heuristic rarely works for random source. */
      }
    }
    
    store_betterment_stats(latestBettermentKind);
    /* DEBUG dump_target_resynthesis(position); */
    
    /*
    Store best match.
    Compared to match from a previous pass:
     The best match may be no better.
     The best match may be the same source point.
     The best match may be the same color from a different source point.
     The best match may be the same source but a better match because the patch changed.
    These are all independent.
    We distinguish some of these cases: only store a better matching, new source.
    */
    if (latestBettermentKind != NO_BETTERMENT )
    {
      /* if source different from previous pass */
      if ( ! equal_points(get_source_of(position), best_point) ) 
      {
        repeatCountBetters++;   /* feedback for termination. */
        integrate_color_change(position); /* Stats. Must be before we store the new color values. */
        /* Save the new color values (!!! not the alpha) for this target point */
        {
          TPixelelIndex j;
          
          // For all color pixelels (channels)
          for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
            // Overwrite prior with new color
            pixmap_index(targetMap, position)[j] = pixmap_index(&corpus, best_point)[j];  
        }
        set_source(position, best_point); /* Remember new source */
      } /* else same source for target */
    } /* else match is same or worse */
  } /* end for each target pixel */
  return repeatCountBetters;
}

