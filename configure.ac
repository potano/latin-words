# Copyright © 2022 Michael Thompson
# SPDX-License-Identifier: FSFAP

AC_INIT([latin-words], [(multi-version)])
AC_CONFIG_SRCDIR([PACKAGE_VERSION], [wordsall/words.adb])
AC_CONFIG_FILES([Makefile])

PACKAGE_VERSION=$(head -n1 PACKAGE_VERSION)

AC_CHECK_PROGS([TAR], [tar gtar bsdtar])

AC_SUBST([ebuild_opts])
AS_IF([test "$PACKAGE_VERSION" = "1.95.0"], [ebuild_opts=nostrip])

AC_CHECK_FILE([wordsall/meanings.adb], [
        AC_ARG_ENABLE([meanings],
                AS_HELP_STRING([--enable-meanings], [generate program to display only word meanings]))
        ebuild_opts="$ebuild_opts meanings"
        have_meanings=1
])


AC_ARG_ENABLE([tools],
        AS_HELP_STRING([--enable-tools], [install supplemental tools in library directory]))


AC_SUBST([docfiles])
AC_SUBST([words_htm_src])
AC_SUBST([wordsdoc_src])
AC_SUBST([howto_src])
AC_CHECK_FILE([wordsall/words.htm],
        [docfiles=words.html; words_htm_src=wordsall/words.htm], [
        AC_CHECK_FILE([web/words.htm],
                [docfiles=words.html; words_htm_src=web/words.htm])
        ])
AC_CHECK_FILE([wordsall/wordsdoc.htm],
        [docfiles="$docfiles wordsdoc.html"; wordsdoc_src=wordsall/wordsdoc.htm], [
        AC_CHECK_FILE([web/wordsdoc.htm],
                [docfiles="$docfiles wordsdoc.html"; wordsdoc_src=web/wordsdoc.htm])
        ])
AC_CHECK_FILE([tools/howto..txt],
        [howto_src="tools/howto..txt"], [
        AC_CHECK_FILE([wordsall/HOWTO.txt],
                [howto_src=wordsall/HOWTO.txt])
        ])
AS_IF([test "$enable_tools" = "yes" && test "x$howto_src" != x],
      [docfiles="$docfiles TOOLS_HOWTO.txt"])


AC_SUBST([dictfiles])
AC_SUBST([helpers])
AC_SUBST([inflects_src])
AC_SUBST([addons_src])
AC_SUBST([uniques_src])
AC_CHECK_FILE([wordsall/DICTLINE.GEN],
        [dictfiles="$dictfiles DICTFILE.GEN"; helpers="$helpers makedict"])
AC_CHECK_FILE([wordsall/STEMLIST.GEN],
        [dictfiles="$dictfiles STEMFILE.GEN INDXFILE.GEN"; helpers="$helpers makestem"])
AC_CHECK_FILE([wordsall/EWDSLIST.GEN],
        [dictfiles="$dictfiles EWDSFILE.GEN"; helpers="$helpers makeefil"])
AC_CHECK_FILE([wordsall/INFLECTS.LAT],
        [dictfiles="$dictfiles INFLECTS.SEC"; helpers="$helpers makeinfl"; inflects_src=INFLECTS.LAT])
AC_CHECK_FILE([wordsall/INFLECTS.],
        [dictfiles="$dictfiles INFLECTS.SEC"; helpers="$helpers makeinfl"; inflects_src=INFLECTS.])
AC_CHECK_FILE([wordsall/ADDONS.LAT], [addons_src="ADDONS.LAT"])
AC_CHECK_FILE([wordsall/ADDONS.], [addons_src="ADDONS."])
AC_CHECK_FILE([wordsall/UNIQUES.LAT], [uniques_src="UNIQUES.LAT"])
AC_CHECK_FILE([wordsall/UNIQUES.], [uniques_src="UNIQUES."])
dictfiles="$dictfiles ADDONS.LAT UNIQUES.LAT"

AC_SUBST([mainprogs], [words])
AS_IF([test "$enable_meanings" = "yes" && test "x$have_meanings" != 'x'], [
       mainprogs="$mainprogs meanings"
])

AC_ARG_VAR([WORDS_CMD_NAME], AS_HELP_STRING([command name to use to invoke WORDS [[words]]]))
AC_ARG_VAR([MEANINGS_CMD_NAME], AS_HELP_STRING([command name to use to invoke MEANINGS [[meanings]]]))

AS_IF([test "x$WORDS_CMD_NAME" = "x"], [WORDS_CMD_NAME=words]);
AS_IF([test "x$MEANINGS_CMD_NAME" = "x"], [MEANINGS_CMD_NAME=meanings]);

AC_SUBST([WORDS_CMD_NAME], ["$WORDS_CMD_NAME"])
AC_SUBST([MEANINGS_CMD_NAME], ["$MEANINGS_CMD_NAME"])

AC_SUBST([tools])
for f in tools/*.adb
do
        nm=$(basename "$f")
        AS_IF([test "$nm" != '*.adb'], [tools="$tools ${nm%.*}"])
done

AC_SUBST([progs_to_install], ["$mainprogs"])
AS_IF([test "$enable_tools" = "yes"],
      [progs_to_install="$progs_to_install $helpers $tools"], [tools=])

AC_OUTPUT

