# Print the review notes marked "To send" in Emacs (mh-annotate.el) as a
# prompt. They leave Emacs for its archive, unless --peek.

usage() {
  cat <<'USAGE'
usage: annotate-take [--all] [--peek]
  --all   also the notes held back in the review
  --peek  print only; the notes stay in Emacs
USAGE
}

all=nil
peek=nil
for arg in "$@"; do
  case $arg in
    --all) all=t ;;
    --peek) peek=t ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

out=$(mktemp -t annotate-take.XXXXXX)
trap 'rm -f "$out"' EXIT

# emacsclient prints a string as Lisp reads it, escapes and all: hence the file.
if ! count=$(emacsclient --eval "(mh/annotate-take-to-file \"$out\" $all $peek)" 2>&1); then
  printf 'annotate-take: no answer from Emacs: %s\n' "$count" >&2
  exit 1
fi
case $count in
  '' | *[!0-9]*)
    printf 'annotate-take: unexpected answer from Emacs: %s\n' "$count" >&2
    exit 1
    ;;
esac

cat "$out"
