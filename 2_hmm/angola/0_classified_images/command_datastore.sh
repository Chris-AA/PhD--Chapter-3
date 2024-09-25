# Combine multuiple image tilss per year into a single .vrt file.
# Only required where study area is large and inputs are tiled
# Builds vrts in this dir with tifs stored in another dir

for year in {1990..2013}
do
  echo Doing $year
  gdalbuildvrt -o /geos/d72/shared/landteam/N/hmm/angola/0_classified_images/MP_$year.vrt /exports/csce/datastore/geos/users/s1318698/angola_landcover/hmm/angola/0_classified_images/MP_$year*.tif
done
