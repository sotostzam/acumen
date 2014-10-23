
# Convert & to ,

find . -name "*.acm" -print0 | xargs -0 sed -i '' -e 's/ \&/\,/g' -e 's/\&/\,/g' -e 's/\,\,/\&\&/g'
