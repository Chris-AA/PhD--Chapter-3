# Combine multuiple image tilss per year into a single .vrt file.
# Only required where study area is large and inputs are tiled

for year in {1984..2020}
do
  echo Doing $year
  gdalbuildvrt -o MP_$year.vrt MP_$year*.tif
done
