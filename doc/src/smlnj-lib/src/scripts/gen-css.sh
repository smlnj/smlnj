#!/bin/sh
#
# COPYRIGHT (c) 2013 The SML3d Project (http://sml3d.cs.uchicago.edu)
# All rights reserved.
#
# Script to generate CSS files for SML3d webpages by substituting specific colors
# for symbolic color names
#
# usage:
#	gen-css.sh src > dst

#
# general document colors
#
BACKGROUND="#fff"
LIGHT_BACKGROUND="#f8f8f7"
MEDIUM_BACKGROUND="#ececec"
#MEDIUM_BACKGROUND="#f8f8f7"
#MEDIUM_BACKGROUND="#99ccff"
DARK_BACKGROUND="#cae5ff"		# previously #369
BANNER_BACKGROUND="$MEDIUM_BACKGROUND"
TOC_HIGHLIGHT="#99ccff"			# hover color for TOC hrefs
HIGHLIGHT_BACKGROUND="yellow"		# background for highlighted text
TITLE_COLOR="#000099"			# color for title in banner
MAJOR_HEADER_COLOR="#cc6600"		# orange/tawny for major headers
MINOR_HEADER_COLOR="#003366"		# dark blue for minor headers
DEFAULT_COLOR="#000044"			# default color for text
HR_COLOR="#527bbd"			# color for horizontal rules
LINK_COLOR="#000099"			# color for hyperlinks
VISITED_COLOR="#6666ff"			# color for visited links
EM_COLOR="#000044"			# color for emphasis elements
STRONG_COLOR="#000044"			# color for strong elements
BULLET_COLOR="green"			# color for UL bullets
BLOCK_BORDER="#527bbd"			# border for inset blocks
BLOCK_BACKGROUND="#f8f8f8"		# background for inset blocks
BLOCK_COLOR="#839496"			# foreground color for inset blocks

#
# code highlighting colors (pallete from http://ethanschoonover.com/solarized)
#
CODE_BORDER="$BLOCK_BORDER"		# border for listing block
CODE_BACKGROUND="$BLOCK_BACKGROUND"	# background color for listings
CODE_COLOR="#6c71c4"			# default foreground color for code
KW_COLOR="#268bd2"			# keyword color
COM_COLOR="#dc322f"			# comment color
LIT_COLOR="#b58900"			# color for literals
BIND_COLOR="#6c71c4"			# color for identifiers at binding sites
PUNCT_COLOR="#6c71c4"			# color for punctuation
# NOTE: the following two colors are from the default pygments color scheme
TV_COLOR="#aa22ff"			# color for type variables
TY_COLOR="#b00040"			# color for type names

sed -e s/@BACKGROUND@/$BACKGROUND/g \
    -e s/@LIGHT_BACKGROUND@/$MEDIUM_BACKGROUND/g \
    -e s/@MEDIUM_BACKGROUND@/$MEDIUM_BACKGROUND/g \
    -e s/@DARK_BACKGROUND@/$DARK_BACKGROUND/g \
    -e s/@BANNER_BACKGROUND@/$BANNER_BACKGROUND/g \
    -e s/@TOC_HIGHLIGHT@/$TOC_HIGHLIGHT/g \
    -e s/@HIGHLIGHT_BACKGROUND@/$HIGHLIGHT_BACKGROUND/g \
    -e s/@TITLE_COLOR@/$TITLE_COLOR/g \
    -e s/@MAJOR_HEADER_COLOR@/$MAJOR_HEADER_COLOR/g \
    -e s/@MINOR_HEADER_COLOR@/$MINOR_HEADER_COLOR/g \
    -e s/@DEFAULT_COLOR@/$DEFAULT_COLOR/g \
    -e s/@HR_COLOR@/$HR_COLOR/g \
    -e s/@LINK_COLOR@/$LINK_COLOR/g \
    -e s/@VISITED_COLOR@/$VISITED_COLOR/g \
    -e s/@EM_COLOR@/$EM_COLOR/g \
    -e s/@STRONG_COLOR@/$STRONG_COLOR/g \
    -e s/@BULLET_COLOR@/$BULLET_COLOR/g \
    -e s/@BLOCK_BORDER@/$BLOCK_BORDER/g \
    -e s/@BLOCK_BACKGROUND@/$BLOCK_BACKGROUND/g \
    -e s/@BLOCK_COLOR@/$BLOCK_COLOR/g \
    -e s/@CODE_BORDER@/$CODE_BORDER/g \
    -e s/@CODE_BACKGROUND@/$CODE_BACKGROUND/g \
    -e s/@CODE_COLOR@/$CODE_COLOR/g \
    -e s/@KW_COLOR@/$KW_COLOR/g \
    -e s/@COM_COLOR@/$COM_COLOR/g \
    -e s/@LIT_COLOR@/$LIT_COLOR/g \
    -e s/@BIND_COLOR@/$BIND_COLOR/g \
    -e s/@PUNCT_COLOR@/$PUNCT_COLOR/g \
    -e s/@TV_COLOR@/$TV_COLOR/g \
    -e s/@TY_COLOR@/$TY_COLOR/g \
    $1
