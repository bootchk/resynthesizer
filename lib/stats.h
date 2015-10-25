
/*
Routines for debugging and statistics.
All are compiled away unless DEBUG defined.
But don't delete them, they are useful for debugging and studying the algorithm.
These use global variables.
*/


#ifdef DEBUG

static inline void
store_betterment_stats(tBettermentKind betterment_kind)
{
  bettermentStats[betterment_kind]+=1;
}


guint integralColorChange = 0;

static void
reset_color_change()
{
  integralColorChange = 0;
}

static inline void
integrate_color_change(Coordinates position)
{
  BppType j;
  
  for(j=FIRST_PIXELEL_INDEX; j<color_end_bip; j++)  // For all color pixelels (channels)
    integralColorChange += abs(pixmap_index(&image, position)[j] - pixmap_index(&corpus, best_point)[j]);
}
          

static void 
dump_parameters(const Parameters *parameters)
{
  g_printf("Parameter: Corpus ID: %d\n", parameters->corpus_id);
  g_printf("Parameter: Neighbor count: %d\n", parameters->neighbours);
  g_printf("Parameter: Max tries: %d\n", parameters->trys);
  g_printf("Parameter: Use border: %d\n", parameters->use_border);
  g_printf("Parameter: Horiz tile: %d\n",parameters->h_tile);
}

/* Stats for this pass */
static void
print_pass_stats(
  gint repeat,
  gint target_count,
  gint repeatCountBetters
  ) 
{
  g_printf("Repetition # %d targets %d bettered %d color change %d\n",
    repeat, target_count, repeatCountBetters, integralColorChange);
  g_printf("Processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}


static void
print_processor_time()
{
  g_printf("Processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}


/* 
Clear target pixels. So see them get resynthesized when animated debugging. 
Note the initial values of the target are never used, but totally resynthesized.
*/
static void
clear_target_pixels(guint bpp)
{
  guint x;
  guint y;
  
  for(y=0;y<image.height;y++)
    for(x=0;x<image.width;x++)
    {
      Coordinates coords = {x,y};
      if (is_selected_image(coords)) 
      {
        guint pixelel;
        Pixelel * pixel = pixmap_index(&image, coords);
        for (pixelel = FIRST_PIXELEL_INDEX; pixelel < bpp; pixelel++) // Color channels only
          pixel[pixelel] = PIXELEL_BLACK;
      }
    }
}


/* Total, ending stats. */
static void
print_post_stats()
{
  g_printf("Target pixels %d\n", target_points_size);
  g_printf("Corpus pixels %d\n", corpus_points_size);
  g_printf("Target pixels tried %d\n", countTargetTries);
  g_printf("Corpus pixels tried %d\n", countSourceTries);
  /* Which part of the algorithm or heuristic found the source. */
  g_printf("Bettered by random %d\n", bettermentStats[RANDOM_CORPUS]);
  g_printf("Bettered by neighbor's source %d\n", bettermentStats[NEIGHBORS_SOURCE]);
  // g_printf("Bettered by neighbor itself %d\n", bettermentStats[NEIGHBOR_ITSELF]);
  // g_printf("Bettered by prior source %d\n", bettermentStats[PRIOR_REP_SOURCE]);
  g_printf("Not bettered %d\n", bettermentStats[NO_BETTERMENT]);
  
  g_printf("Perfect matches %d\n", bettermentStats[PERFECT_MATCH]);
  g_printf("Processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}


static void
dump_target_points()
{
  /* dump source for each target, in order in which synthed  */
  guint i;
  
  for (i=0; i<target_points_size; i++) 
  {
    // c++ Coordinates position = target_points[i];
    Coordinates position = g_array_index(target_points, Coordinates, i);
    Coordinates position2 = get_source_of(position);
    g_printf("Target, source Coords: %d %d , %d %d\n", position.x, position.y, position2.x, position2.y);
  }
}

static void
dump_max_grad()
{
  guint i;
  for(i=0; i<401; i++)
    g_printf("Grad %d max %d\n", i, max_cartesian_along_ray[i]);
}



/* Print the results for one attempt at resynthesizing a target point. For debug, study. */
static void
dump_target_resynthesis(Coordinates position)
{
  if ( latestBettermentKind != NO_BETTERMENT) {
      g_printf("Point %d %d source %d %d best %d betterment %d \n", position.x, position.y, best_point.x, best_point.y, best, latestBettermentKind);
      /* myDebug4(position, target_points, "Delta color", image.at(position)[0] - corpus.at(best_point)[0] ); // One channel */
      gint distance = neighbours[n_neighbours-1].x*neighbours[n_neighbours-1].x + neighbours[n_neighbours-1].y*neighbours[n_neighbours-1].y;
      g_printf("Count neighbors %d max distance %d\n", n_neighbours, distance);
  }
}


#else

#define store_betterment_stats(a)
#define reset_color_change()
#define integrate_color_change(a)
#define dump_parameters(a)
#define print_pass_stats(a,b,c)
#define print_processor_time()
#define clear_target_pixels(a)
#define print_post_stats()
#define dump_target_points()
#define dump_target_resynthesis(a)
#endif

