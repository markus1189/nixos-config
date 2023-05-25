#!@bash@/bin/bash

set -eo pipefail

COOKIE=
export COOKIE
IP=
export IP
IPV6=
export IPV6
MD5=
export MD5

echo "Called with args: $*" >&2

while [ "$1" ]; do
    if [ "$1" = "--cookie" ];      then shift; COOKIE="$1"; fi
    if [ "$1" = "--client-ip" ];   then shift; IP="$1"; fi
    if [ "$1" = "--client-ipv6" ]; then shift; IPV6="$1"; fi
    if [ "$1" = "--md5" ];         then shift; MD5="$1"; fi
    if [ "$1" = "--client-os" ];   then shift; fi
    shift
done

if [ -z "$COOKIE" ] || [ -z "$MD5" ] || [ -z "$IP$IPV6" ]; then
    echo "Parameters --cookie, --md5, and --client-ip and/or --client-ipv6 are required" >&2
    exit 1;
fi

# Extract username and domain and computer from cookie
USER=$(echo "$COOKIE" | @gnused@/bin/sed -rn 's/(.+&|^)user=([^&]+)(&.+|$)/\2/p')
export USER
DOMAIN=$(echo "$COOKIE" | @gnused@/bin/sed -rn 's/(.+&|^)domain=([^&]+)(&.+|$)/\2/p')
export DOMAIN
COMPUTER=$(echo "$COOKIE" | @gnused@/bin/sed -rn 's/(.+&|^)computer=([^&]+)(&.+|$)/\2/p')
export COMPUTER

HOSTID="deadbeef-dead-beef-dead-beefdeadbeef"
export HOSTID

NOW=$(@coreutils@/bin/date +'%m/%d/%Y %H:%M:%S') # format expected by GlobalProtect
export NOW

DAY=$(@coreutils@/bin/date +'%d')
export DAY
MONTH=$(@coreutils@/bin/date +'%m')
export MONTH
YEAR=$(@coreutils@/bin/date +'%Y')
export YEAR


@envsubst@/bin/envsubst -no-unset -i @hipreportfile@
