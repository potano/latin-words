#!/bin/sh

# Copyright © 2022 Michael Thompson
# SPDX-License-Identifier: GPL-2.0-or-later

fatal () {
        echo $@ >&2
        exit 1
}

pkgname="$1"
version="$2"
targdir="$3"

have_meanings=N
strip=N

while [ "x" != "x$4" ]; do
        case "$4" in
                meanings ) have_meanings=Y ;;
                nostrip ) strip=Y ;;
                * ) fatal "Unknown argument $4" ;;
        esac
        shift
done

if [ -z "$pkgname" -o -z "$version" -o -z "$targdir" ]; then
        fatal "Required setting is missing"
fi

if [ ! -d "$targdir" ]; then
        fatal "Target directory $targdir does not exist"
fi

ebuild="$pkgname-$version".ebuild
ebuild_pathname="$targdir/$ebuild"

if [ "$have_meanings" = N ]; then
        IUSE=tools
        meanings_config=
else
        IUSE="meanings tools"
        meanings_config='		$(use_enable meanings)  \
'
fi

RESTRICT=
if [ "$strip" = Y ]; then
        RESTRICT='RESTRICT="strip"
'
fi

cat > $ebuild_pathname <<END
# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=7
ADA_COMPAT=( gnat_202{0..1} )
inherit ada

DESCRIPTION="Latin-word parser and dictionary with an English-to-Latin mode"
HOMEPAGE="https://github.com/potano/latin-words"
SRC_URI="https://github.com/potano/latin-words/releases/download/\${PV}/\${P}.tar.gz"

LICENSE="public-domain"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="$IUSE"

RDEPEND="\${ADA_DEPS}"
DEPEND="\${RDEPEND}"
REQUIRED_USE="\${ADA_REQUIRED_USE}"
$RESTRICT

src_configure() {
	econf \\
$meanings_config		\$(use_enable tools)
}

src_install() {
	emake DESTDIR="\${D}" install

	for f in docs/*; do
		docinto / && dodoc "\${f}"
	done
}
END

