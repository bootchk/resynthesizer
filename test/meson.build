
# test harness for libheal

# installs to usual place in the usual path

# It encodes its own test data

# run it, expect print to console

# To interpret the results, read the code.
# Note latter tests test calling errors, expect an error returned.


if get_option('build-libheal')
  executable('testHealLib',
    'testSynth.c',
    c_args: [ '-DG_LOG_DOMAIN="Resynthesizer"', '-DGIMP_COMPILATION', ],
    link_with : [
      heal_lib,
    ],
    include_directories: resynthesizer_inc,
    install: true,
  )
endif