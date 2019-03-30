#!/bin/bash
DIR_IN=data/arretes/pdf/
DIR_OUT=data/arretes/plain_text/
LANG="fra"
# create the output path if necessary
if [ ! -d "$DIR_OUT" ]; then
   mkdir -p "$DIR_OUT"
fi
for FILEPATH in $DIR_IN*.pdf; do
    OUTBASE=$DIR_OUT$(basename $FILEPATH)
    # imagemagick: convert pdf to high-res multi-page tiff
    convert -density 300 "$FILEPATH" -depth 8 -strip -background white -alpha off ./temp.tiff > /dev/null 2>&1
    # tesseract: OCR the tiff
    tesseract ./temp.tiff "$OUTBASE" -l $LANG > /dev/null 2>&1
    # clean up temp file
    rm ./temp.tiff
done
       
