

static void
adaptPluginToLibraryParameters(
  TGimpAdapterParameters* p1,
  TImageSynthParameters* p2
  )
{
  p2->isMakeSeamlesslyTileableHorizontally = p1->h_tile;
  p2->isMakeSeamlesslyTileableVertically   = p1->v_tile;
  p2->matchContextType                     = p1->use_border;
  p2->mapWeight                            = p1->map_weight;
  p2->sensitivityToOutliers                = p1->autism;
  p2->patchSize                            = p1->neighbours;
  p2->maxProbeCount                        = p1->trys;
}
