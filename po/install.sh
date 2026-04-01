#!/bin/sh

to_dir=$1
from_file=$2
to_file=$3

mkdir -p "${MESON_INSTALL_DESTDIR_PREFIX}/${to_dir}"
cp -vf "${from_file}" "${MESON_INSTALL_DESTDIR_PREFIX}/${to_file}"
