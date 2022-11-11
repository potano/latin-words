#!/bin/sh

# Copyright © 2022 Michael Thompson
# SPDX-License-Identifier: FSFAP

fatal () {
        echo $@ >&2
        exit 1
}

if [ $# -lt 2 ]; then
        fatal "Need 2 arguments"
fi

if [ ! -x ./newline ]; then
        fatal "Can't find newline command"
fi

src="$1"
targ="$2"

newline=$(./newline | tr '\r\n' rn)

if [ ! -f "$src" ]; then
        echo "Source file $src does not exist"
fi
if [ -d "$targ" ]; then
        targ="$targ/"$(basename "$src")
fi

if [ "$newline" = n ]; then
        tr -d '\r' <"$src" >"$targ"
else
        cp "$src" "$targ"
fi

