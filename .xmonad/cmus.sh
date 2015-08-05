ALBUMARTIST=$( cmus-remote -Q 2>/dev/null | grep albumartist | head -n 1 | cut -d " " -f 3- )
TITLE=$( cmus-remote -Q 2>/dev/null | grep title | head -n 1 | cut -d " " -f 3- )

if [ -z "$ALBUMARTIST" ];
then
ARTIST=$( cmus-remote -Q 2>/dev/null | grep artist | head -n 1 | cut -d " " -f 3- )
echo "$ARTIST - $TITLE"
else
echo "$ALBUMARTIST - $TITLE"
fi
