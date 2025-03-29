#pragma once


/*
Routines for debugging and statistics.
All are compiled away unless DEBUG defined.
But don't delete them, they are useful for debugging and studying the algorithm.
These use global variables.
*/

void store_betterment_stats(tBettermentKind betterment_kind);
void reset_color_change();
void integrate_color_change(Coordinates position, TFormatIndices* indicies, Map *image, Map* corpus, Coordinates bestMatchCoords);
// void dump_parameters(const Parameters *parameters);
void print_pass_stats(gint repeat, gint target_count, gint repeatCountBetters);
void print_processor_time();
void clear_target_pixels(guint bpp, Map targetMap);
void print_final_stats();
void dump_target_points();
void dump_max_grad();
void dump_target_resynthesis(Coordinates position);




