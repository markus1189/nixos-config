#!@bash@/bin/bash

APPNAME="${1}"
SUMMARY="${2}"
BODY="${3}"
ICON="${4}"
URGENCY="${5}"

@systemd@/bin/systemd-cat -t dunst-logger -- @jo@/bin/jo \
 appname="${APPNAME}" \
 summary="${SUMMARY}" \
 body="${BODY}" \
 icon="${ICON}" \
 urgency="${URGENCY}"
