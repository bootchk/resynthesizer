/*
Prepare parameters of repetition (passes over the target.)
The algorithm repeats synthesis because very early synthesized points
might be wild guesses, more or less random colors,
especially if the context is far away.

Repeat over full target, then repeat over smaller prefix of target, etc.
If synthesizing directionally (targetPoints ordered instead of random)
then later passes do not cover the target uniformly.
Instead, later passes repeat a prefix, which when directionally ordered,
means repeating target points near the context.
It could be argued that this is good, since those points
are often a transition and need the most work to produce a good result.

The original code just lengthened targetPoints vector by duplicating a prefix:
  for(int n=targetPoints->len;n>0;) {
    n = n*3/4; // <- note magic number... the more passes, the higher the quality, maybe
    for(int i=0;i<n;i++)
      targetPoints.push_back(targetPoints[i]);
  }
and just iterated over the target_point vector, either forward or back:
for(int i=targetPoints->len-1;i>=0;i--) { // do a fraction, then more, etc. then all
for(int i=0; i<=int(targetPoints->len-1); i++) {  // do all, then repeat a fraction, etc.

The new code is nearly the same as the original except:
-the second pass resynthesizes all points.
-the number of passes is limited to 6

TODO experiments on other ways of repeating synthesis.
*/

#define MAX_PASSES 6
typedef guint TRepetionParameters[MAX_PASSES][2] ;

static guint
prepare_repetition_parameters(
  TRepetionParameters repetition_params,
  guint countTargetPoints
  )
{ 
  guint i;
  guint n = countTargetPoints;
  guint total_targets;
  
  /* First pass over all points  */
  repetition_params[0][0] = 0;  
  repetition_params[0][1] = n;
  total_targets = n;
  
  /* 
  Second pass over all points, subsequent passes over declining numbers at an exponential rate. 
  */
  for (i=1; i<MAX_PASSES; i++) 
  {
    repetition_params[i][0] = 0;    /* Start index of iteration. Not used, starts at 0 always, see the loop. */
    repetition_params[i][1] = n;    /* End index of iteration. */
    total_targets += n;
    n = (guint) n*3/4;
  }
  return total_targets;
}

