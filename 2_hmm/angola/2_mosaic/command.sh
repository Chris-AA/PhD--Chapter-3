# Mosaic outputs into single images

gdal_merge.py -o MP_test.tif -of GTiff -co "COMPRESS=LZW" -ot Byte ../1_apply_hmm/MP_test*.tif
