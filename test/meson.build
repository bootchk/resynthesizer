
# test harness for libresynth

# installs to usual place in the usual path

# It encodes its own test data

# run it, expect print to console

# To interpret the results, read the code.
# Note latter tests test calling errors, expect an error returned.



executable('testResynthLib',
  'testSynth.c',
  c_args: [ '-DG_LOG_DOMAIN="Resynthesizer"', '-DGIMP_COMPILATION', ],
  link_with : [
    resynth_lib,
  ],
  include_directories: resynthesizer_inc,
  install: true,
)