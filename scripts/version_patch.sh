#!/bin/sh

# Copyright © 2022 Michael Thompson
# SPDX-License-Identifier: GPL-2.0-or-later

fatal () {
        echo 'version_patch: ' $@ >&2
        exit 1
}

version="$1"
srcdir="$2"
targdir="$3"

num_patches=0

patchfile () {
        local name="$1"
        local fromline
        local toline
        local cmd
        local args
        shift
        infile="$srcdir/$name"
        outfile="$targdir/$name"
        if [ ! -f "$infile" ]; then
                fatal "$infile does not exist"
        fi
        if [ ! -d "$targdir" ]; then
                fatal "$targdir does not exist"
        fi
        while [ -n "$1" ]; do
                fromline="$1"
                toline="$2"
                shift; shift
                args="$args -e 's/$fromline/$toline/'"
                num_patches=$(( num_patches + 1 ))
        done
        cmd="sed$args <'$infile' >'$outfile'"
        if ! eval "$cmd"; then
                fatal "patch $args failed"
        fi
}

case "$version" in
        1.95.0 )
                patchfile latin_file_names.ads \
                        'INFLECTIONS_FULL_NAME     : constant STRING := "INFLECTS.";' \
                        'INFLECTIONS_FULL_NAME     : constant STRING := "INFLECTS.LAT";' \
                        'UNIQUES_FULL_NAME      : constant STRING := "UNIQUES.";' \
                        'UNIQUES_FULL_NAME      : constant STRING := "UNIQUES.LAT";' \
                        'ADDONS_FULL_NAME       : constant STRING := "ADDONS.";' \
                        'ADDONS_FULL_NAME       : constant STRING := "ADDONS.LAT";'
                ;;
        1.97.1 )
                patchfile addons_package.ads \
                        'TACKONS  : TACKON_ARRAY(1..15);' \
                        'TACKONS  : TACKON_ARRAY(1..20);'
                ;;
esac

if [ $num_patches -eq 0 ]; then
        echo "No patch needed for $version"
else
        echo "$num_patches patch(es) applied to $version"
fi
