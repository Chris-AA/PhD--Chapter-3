## Hidden Markov Model: Data analysis- Sam Bowers

# Import libraries
import matplotlib.pyplot as plt   # For plotting figs
import numpy as np                # For managing arrays
from osgeo import gdal            # For geospatial data

# Open the file
ds = gdal.Open("M:\\angola_landcover_temp\\MP_test_reprojected.tif")

# Print some properties
print( 'Number of layers: ', ds.RasterCount)
print('X-size: ', ds.RasterXSize),
print('Y-size: ', ds.RasterYSize)
print('Resolution: ', ds.GetGeoTransform()[1]) # Degrees / pixel

# Extract land cover images for 2000 and 2020
landcover_2020 = ds.GetRasterBand(2020-1984+1).ReadAsArray()
landcover_1984 = ds.GetRasterBand(1984-1984+1).ReadAsArray()

# Display landcover for 2020
plt.imshow(landcover_2020, vmin=0, vmax=4, interpolation='nearest')
plt.colorbar()
plt.title('2020')
plt.show()

# Display landcover for 2000
plt.imshow(landcover_1984, vmin=0, vmax=4, interpolation='nearest')
plt.colorbar()
plt.title('1984')
plt.show()

# Note the change in agricultural land (class 4) between the two images

## Extract land cover statistics

# Get unique raster values and their total
label1, count1 = np.unique(landcover_2020, return_counts = True)

print('Number of pixels per class: ', list(zip(label1, count1)))

label2, count2 = np.unique(landcover_1984, return_counts = True)

print('Number of pixels per class: ', list(zip(label2, count2)))

# Display as a pie chart
plt.pie(count1, labels=label1)
plt.show()

# Display as a mor useful pie chart [more advanced]
plt.pie(count1, labels = ['%s (%s%%)'%(name, round((i/sum(count1)) * 100, 1)) \
                         for i, name in zip(count1, ['No data', 'Forest','Savanna','Grassland','Cropland', 'Urban', 'Riverine veg'])],
        colors = ['black','darkgreen','darkred','yellow','pink', 'red', 'blue'])
plt.show()

plt.pie(count2, labels = ['%s (%s%%)'%(name, round((i/sum(count2)) * 100, 1)) \
                         for i, name in zip(count2, ['No data', 'Forest','Savanna','Grassland','Cropland', 'Urban', 'Riverine veg'])],
        colors = ['black','darkgreen','darkred','yellow','pink', 'red', 'blue'])
plt.show()

# Plot line graph
n_forest_px = []
n_savanna_px = []
n_grassland_px = []
n_cropland_px = []
n_urban_px = []

for year in range(1984,2021):
  print('Doing %s'%year)
  this_landcover = ds.GetRasterBand(year-1984+1).ReadAsArray()
  label, count = np.unique(this_landcover, return_counts = True)
  n_forest_px.append(count[1])
  n_savanna_px.append(count[2])
  n_grassland_px.append(count[3])
  n_cropland_px.append(count[4])
  n_urban_px.append(count[5])

print(n_forest_px, n_savanna_px, n_grassland_px, n_cropland_px, n_urban_px)

plt.plot(range(1984,2021), n_forest_px, color = 'darkgreen')
plt.plot(range(1984,2021), n_savanna_px, color = 'darkred')
plt.plot(range(1984,2021), n_grassland_px, color = 'orange')
plt.plot(range(1984,2021), n_cropland_px, color = 'pink')
plt.plot(range(1984,2021), n_urban_px, color = 'red')

plt.show()

## Make a change map

# Method: loop through each raster layer, identify transitions from forest to non-forest

# Read first image
previous_landcover = ds.GetRasterBand(1984-1984+1).ReadAsArray() # Get first landcover image

# Initialise a deforestation layer
deforestation = np.zeros_like(previous_landcover).astype(np.bool)

# and a deforestation_year layer
deforestation_year = np.zeros_like(previous_landcover).astype(np.int16)


for year in range(1985,2021):

  print('Doing %s'%year)
   
  # Load this year's land cover
  this_landcover = ds.GetRasterBand(year-1984+1).ReadAsArray()
  
  # Extract deforestation by comparing against previous year
  this_deforestation = np.logical_and(previous_landcover == 1, this_landcover != 1)
  
  # Set deforesation layer to True where deforestation observed
  deforestation[this_deforestation] = True

  # Set deforestation_year layer to year where deforestation observed
  deforestation_year[this_deforestation] = year

  # Save this_landcover to previous_landcover for next loop iteration
  previous_landcover = this_landcover.copy()
  
  # Display deforestation mask
plt.imshow(deforestation, interpolation='nearest')
plt.colorbar()
plt.title('Deforestation mask')
plt.show()

# Display deforestation year
plt.figure(figsize=(8,8), dpi=300)
plt.imshow(np.ma.array(deforestation_year, mask = deforestation_year==0),
           vmin=1984, vmax=2020, interpolation='nearest')
plt.colorbar()
plt.title('Deforestation year')
plt.show()

## Export to GeoTiff

# By example, I'll output the deforestation_year array from the previous section.

driver = gdal.GetDriverByName('GTiff')
ds_out = driver.Create('deforestation_year_reprojected.tif',         # Output file name
                       xsize = deforestation_year.shape[1],      # Number of rows
                       ysize = deforestation_year.shape[0],      # Number of columns
                       bands = 1,                                # Number of bands
                       eType = gdal.GDT_UInt16,                  # Output data type. Be careful here, choose something appropriate
                       options = ['COMPRESS=LZW'])       # Magic option to reduce your file sizes
ds_out.GetRasterBand(1).WriteArray(deforestation_year)   # Write array to output band
ds_out.SetGeoTransform(ds.GetGeoTransform())             # Write GeoTransform. As image dimensions haven't changed we can pull this from the input image
ds_out.SetProjection(ds.GetProjection())                 # Likewise the projection info
ds_out = None                                            # Save