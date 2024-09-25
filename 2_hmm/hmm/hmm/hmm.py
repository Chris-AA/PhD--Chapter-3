import argparse
import glob
import numpy as np
import os
from osgeo import gdal
import pdb

class LoadTimeSeries:
  """ """
  def __init__(self, input_files, transition_matrix_file = None,
               simplified = False,
               landcover_codes = None,
               block = None, verbose = True):
    
    self.input_files = sorted(input_files)
    self.transition_matrix_file = transition_matrix_file
    self.simplified = simplified,
    self.block = block
    
    # Read first file
    ds = gdal.Open(self.input_files[0], 0)
    
    self.n_steps = len(self.input_files)
    self.n_labels = ds.RasterCount
    
    if block is None:
      self.x_off = 0
      self.y_off = 0
      self.n_cols = ds.RasterXSize
      self.n_rows = ds.RasterYSize
    else:
      self.x_off = block[0]
      self.y_off = block[1]
      self.n_cols = block[2]
      self.n_rows = block[3]
    
    self.n_samples = self.n_rows * self.n_cols
    
    if landcover_codes is None:
        landcover_codes = range(1,self.n_labels+1)
    self.landcover_codes = np.array(landcover_codes)
     
    # Extract CRS info
    self.projection = ds.GetProjection()
    
    geo_t = ds.GetGeoTransform()
    geo_t_update = (geo_t[0] + (geo_t[1] * self.x_off),
                    geo_t[1],
                    geo_t[2],
                    geo_t[3] + (geo_t[5] * self.y_off),
                    geo_t[4],
                    geo_t[5])
    self.geo_t = geo_t_update

    # Extract band numbers to be replaced at output
    #self.input_labels = np.arange(self.n_labels+1)
    #output_labels = [0]
    #for n in range(self.n_labels):
    #  output_labels.append(ds.GetRasterBand(n+1).GetDescription())
    #self.output_labels = np.array(output_labels)
    
    self.verbose = verbose
    
    # Read transition matrix
    self.transition_matrix = self.readTransitionMatrix(transition_matrix_file, simplified = self.simplified)
     
    # Read all time series info 
    self.readData()

  def readData(self):
    
    # Manufacture time series
    time_series = np.empty((self.n_steps, self.n_labels, self.n_rows, self.n_cols), dtype='float32')

    # Keep track of nodata
    nodata = np.zeros((self.n_steps, self.n_rows, self.n_cols)).astype(np.bool)
    
    for n, input_file in enumerate(self.input_files):

      if self.verbose: print('Reading %s'%input_file)
      
      # Read time series
      ds = gdal.Open(input_file, 0)
      time_series[n,:,:,:] = ds.ReadAsArray(xoff = self.x_off, yoff = self.y_off, xsize = self.n_cols, ysize = self.n_rows)# / 10000.
      
      # Determine nodata points
      nodata[n] = (time_series[n,:,:,:]>0).sum(axis=0) == 0
     
    persistent_nodata = (nodata == False).sum(axis=0) == 0 #== self.n_steps
    
    self.time_series = time_series
    self.nodata = nodata
    self.persistent_nodata = persistent_nodata
  
  def readTransitionMatrix(self, transition_matrix_file, simplified = False):
    '''
    Columns = FROM
    Rows = TO
    '''
    
    if simplified:
        
        # Manufacture a transition matrix
        transition_prior = 0.01 #0.1
        transition_matrix = np.ones((self.n_labels, self.n_labels)) * transition_prior
        np.fill_diagonal(transition_matrix, 1 - transition_prior)
        return transition_matrix

    #transition_matrix = np.genfromtxt('/content/gdrive/MyDrive/jamaica_transition_matrix.csv', delimiter=',')
    transition_constraints = np.genfromtxt(transition_matrix_file, delimiter=',')
    transition_matrix = transition_constraints[1:,1:]
    transition_landcovers = transition_constraints[0,1:].astype(int)

    # Set probabilities
    probability_classes = [0.9,0.1,0.025,0.001]
    
    for n, probability_class in enumerate(probability_classes):
      transition_matrix[transition_matrix == n+1] = probability_class
    
    # Remove land cover classes not included
    landcover_classes = []
    ds = gdal.Open(self.input_files[0])
    
    for lyr in range(ds.RasterCount):
      landcover_classes.append(int(ds.GetRasterBand(lyr+1).GetDescription().split('_')[-1]))
    
    assert len(np.setdiff1d(landcover_classes, transition_landcovers)) == 0, "Not all land covers represented in transition matrix"
    
    lc_excluded = np.setdiff1d(transition_landcovers, landcover_classes)
    
    transition_matrix = transition_matrix[np.isin(transition_landcovers, lc_excluded) == False, :][:, np.isin(transition_landcovers, lc_excluded) == False]
    
    return transition_matrix

  def outputImage(self, im, output_name = None):
    
    if self.verbose: print('Writing output')

    # Set output_name
    if output_name is None:
      output_name = self.output_dir + '/%s'
      if self.block is None:
        output_name = output_name%'landcover_timeSeries.tif'        
      else:
        output_name = output_name%'landcover_timeSeries_%s_%s.tif'%(str(self.y_off).zfill(4), str(self.x_off).zfill(4))    
    
    gdal_driver = gdal.GetDriverByName('GTIFF')
    ds_out = gdal_driver.Create(output_name,\
                                 self.n_cols, self.n_rows, self.n_steps,\
                                 gdal.GDT_Byte, options = ['COMPRESS=LZW'])
    ds_out.SetProjection(self.projection)
    ds_out.SetGeoTransform(self.geo_t)
    
    for layer in range(self.n_steps):
      raster_band = ds_out.GetRasterBand(layer+1)
      raster_band.SetDescription(str(1984+layer))
      raster_band.SetNoDataValue(0)
      raster_band.WriteArray(im[layer])
    
  def _normalize(self, v):
   
    z = v.sum(0)

    out = v
    out[:,z>0] = (v[:,z>0]/z[z>0])

    return out
  
  def _first_nonzero(self, arr, axis, invalid_val=-1):
    # Source: https://stackoverflow.com/questions/47269390/numpy-how-to-find-first-non-zero-value-in-every-column-of-a-numpy-array
    mask = arr!=0
    return np.where(mask.any(axis=axis), mask.argmax(axis=axis), invalid_val)
  
  def _last_nonzero(self, arr, axis, invalid_val=-1):
    # Source: https://stackoverflow.com/questions/47269390/numpy-how-to-find-first-non-zero-value-in-every-column-of-a-numpy-array
    mask = arr!=0
    val = arr.shape[axis] - np.flip(mask, axis=axis).argmax(axis=axis) - 1
    return np.where(mask.any(axis=axis), val, invalid_val)

  def forward(self, output = False):
    
    if self.verbose: print('Doing forward probabilities')

    # Initial probability (first time step)
    fc = np.empty_like(self.time_series, dtype=np.float32)
    fc[0] = self.time_series[0]
    
    # Set fc[0] to next valid figure where nodata
    ind = self._first_nonzero((self.nodata == False).astype(np.int8), axis=0)
    fc[0][:,ind>0] = np.take_along_axis(self.time_series, ind[None,None,:,:], axis=0)[0,:,ind>0].T

    
    for t in range(1, self.n_steps):
      
      # Update probability
      fc[t] = self._normalize(self.time_series[t] * np.tensordot(self.transition_matrix, fc[t-1], axes=1))
      
      # Experimental: Fill nodata using prev value
      #TODO: This currently propagates nodata forward given fc[t] * fc[t-1] makes nodata whether either nodata.
      fc[t][:,self.nodata[t]] = fc[t-1][:,self.nodata[t]]

    return fc

  def backward(self, output = False):

    if self.verbose: print('Doing backward probabilities')
    
    # Initial probability (final time step)
    bc = np.empty_like(self.time_series, dtype=np.float32)
    bc[-1] = self.time_series[-1]
    
    # Set bc[-1] to next valid figure where nodata
    ind = self._last_nonzero((self.nodata == False).astype(np.int8), axis=0)
    bc[-1][:,ind>0] = np.take_along_axis(self.time_series, ind[None,None,:,:], axis=0)[0,:,ind>0].T
    
    for t in range(self.n_steps-1, 0, -1):
      #if t == 12: pdb.set_trace()
      # Update probability
      bc[t-1, :] = self._normalize(np.tensordot(self.transition_matrix, (self.time_series[t] * bc[t]), axes=1))
      
      # Experimental: Fill nodata
      #bc[t-1][:,self.nodata[t-1]] = bc[t][:,self.nodata[t-1]]
      bc[t-1][:,self.nodata[t]] = bc[t][:,self.nodata[t]]

    return bc

  def _likelihood(self, fc, bc):

    """
    Likelihood function
    Args:
        fc (2d array): Forward probabilities.
        bc (2d array): Backward probabilities.
    Returns:
        Posterior probabilities (2d array)
    """

    posterior = fc * bc
    
    # Experimental: Fix nodata by taking max of fc or bc where other isn't present
    nodata_reshape = np.broadcast_to(np.expand_dims(self.nodata, axis=1), posterior.shape)
    posterior[nodata_reshape] = np.maximum(fc, bc)[nodata_reshape]
        
    # Set no measurent to np.nan
    posterior[np.broadcast_to(posterior.sum(axis=1)[:,np.newaxis,:,:]==0, posterior.shape)] = np.nan
    
    posterior[posterior == 0] = 0.0001
    
    z = posterior.sum(axis=1)[:, np.newaxis]
    
    return (posterior / z)

  def forward_backward(self, output = False):
    
    fc = self.forward()
    bc = self.backward()
    
    if self.verbose: print('Merging to likelihood')
    posterior = self._likelihood(fc, bc)
    
    return posterior
  
  def getLandcover(self, method = 'forward-backward',
                   output = False, output_dir = './', output_name = None):
    """
    postprocessing_steps = ['spatial_filter', 'incident_filter']
    """

    if self.verbose: print('Finalising landcover')

    landcover_timeseries = np.empty((self.n_steps, self.n_rows, self.n_cols)).astype(np.uint8)
    
    if method == 'forward-backward': posterior = self.forward_backward()
    if method == 'backward': posterior = self.backward()
    if method == 'forward': posterior = self.forward()
    
    persistent_nodata = (self.nodata == False).sum(axis=0) == 0
    
    for t in range(self.n_steps):

      landcover_timeseries[t] = np.argmax(posterior[t], axis=0) + 1
      nodata = np.isnan(posterior[t]).sum(axis=0) > 0
      
      # Set nodata to 0
      landcover_timeseries[t][nodata] = 0
      
      # Experimental: Get persistent nodata only      
      landcover_timeseries[t, persistent_nodata] = 0
      
      # Set output labels
      i = np.searchsorted(range(1,self.n_labels+1), landcover_timeseries[t])
      landcover_timeseries[t] = self.landcover_codes[i]
      landcover_timeseries[t,self.persistent_nodata] = 0
        
    if output:
      
      if output_name is not None: output_name = output_dir + '/' + output_name

      self.outputImage(landcover_timeseries, output_name = output_name)

    return landcover_timeseries



def main(input_file,
         transition_matrix_file = None,
         block = None,
         landcover_codes = None,
         output_name = 'HMM_output',
         output = True,
         verbose = False):

  '''
  '''
  
  simplified = True if transition_matrix_file is None else False
  
  time_series = LoadTimeSeries(input_files,
                               transition_matrix_file = transition_matrix_file,
                               simplified = simplified,
                               block = block,
                               landcover_codes = landcover_codes,
                               verbose = verbose)
  
  landcover = time_series.getLandcover(output = output, output_name = output_name + '.tif')
  


if __name__ == '__main__':
  
  parser = argparse.ArgumentParser(description='Hidden Markov Model for land cover probability time series.')
  parser._action_groups.pop()
  required = parser.add_argument_group('Required arguments')
  optional = parser.add_argument_group('Optional arguments')
  
  # Required imputs
  required.add_argument('infiles', metavar='PATH', type = str, nargs = '*', help = 'Landcover probability input files (one per year, either .vrt or .tif format). When alphabetically sorted files must line up with oldest data first, most recent data last. Use wildcarts to select multiple files (e.g. input_file_????.vrt) where ???? is a year. Max one file per year (combine multiple tiles into .vrt where required.')  
  # Optional inputs
  optional.add_argument('-t', '--transition_matrix', type= str, nargs = 1, default = None, help = 'Path to a transition matrix .csv file (cols = from, rows = to), with one row/col per landcover.')
  optional.add_argument('-b', '--block', type = int, nargs = 4, help = "Process a subset of an image by specifying a block in the format 'xmin ymin xstep ystep'. This is recommended where input images are large, as this is a memory intensive process.")
  optional.add_argument('-c', '--landcover_codes', type = int, nargs = '*', default=None, help = "List landcover codes associated with each input band.") 
  optional.add_argument('-o', '--output_name', type=str, default = 'HMM_output', help = "Specify an output file name to write output data to. Where a block is used be careful not to overwrite an existing file..")
  optional.add_argument('-v', '--verbose', action='store_true', default=False, help = "Make script more chatty.")
  
  args = parser.parse_args()
  
  input_files = sorted([os.path.abspath(i) for i in args.infiles])
  
  main(input_files,
       transition_matrix_file = args.transition_matrix,
       block = args.block,
       landcover_codes = args.landcover_codes,
       output_name = args.output_name,
       output = True, 
       verbose = args.verbose)
  
