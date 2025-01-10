##---------------------
## This code downloads and cleans distance to rural area when using interpolated pop data for Glidden et al. 2023 Nat Communications
## NOTE: This is written to be run in Google Colab
## You may have to request access to the assetts
##---------------------
import ee
import pandas as pd
from google.colab import drive

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the Earth Engine library
ee.Initialize()()

##-----------------------##
## Set up data           ##
##-----------------------##
# Define the region of interest (Brazil) and load snail occurrence points.
region = ee.FeatureCollection('FAO/GAUL_SIMPLIFIED_500m/2015/level0').filter(ee.Filter.equals('ADM0_NAME', 'Brazil'))
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

def buffer_points(radius, bounds=True):
    """Function to buffer points either as simple buffers or bounded (square)."""
    def buffer_feature(pt):
        pt = ee.Feature(pt)
        return ee.Algorithms.If(bounds, pt.buffer(radius).bounds(), pt.buffer(radius))
    return buffer_feature

# Apply a 500m buffer to each point.
buffered_points = all_sdm_points.map(buffer_points(500))

# Load interpolated population data 
pop1k = ee.Image('users/cglidden/interpolated_population_90s_v3')

# list of years to map over
years = ee.List.sequence(1990, 1999)

##-----------------------------------------------##
## Convert pop images to rural versus non rural  ##
## based on population cutoff (<300 per km2)     ##
##-----------------------------------------------##
def map_rural(year):
    '''Create a binary image indicating rural areas based on population count for a given year.'''
    yearString = ee.Number(year).toInt().format();
    band_name = ee.String("population_").cat(yearString);

    ruralMax = 300;

    ruralMask = (pop1k.select([band_name]).lt(ruralMax)
    .rename(ee.String("rural_").cat(ee.Number(year).toInt().format())));

    return ee.Image(ruralMask)

# Generate rural masks for the 1990s decade.
rural_binary_collection = ee.ImageCollection(years.map(map_rural))
rural_binary = rural_binary_collection.toBands()

# Rename the bands to remove the index suffix.
def remove_index(band_name):
    return ee.String(band_name).slice(-10)

band_names = rural_binary.bandNames()
new_band_names = band_names.map(remove_index)
renamed_rural = rural_binary.select(band_names, new_band_names)

##------------------------------------------------------##
## Create a costmap to represent distance to rural area ##
##------------------------------------------------------##

# Define the function
def map_cost(year):
    """
    Generate a cost map for rural areas for a specified year.

    Args:
    year (int): The year for which the cost map is created.

    Returns:
    ee.Image: An image representing the cumulative cost distance to the nearest rural area.
    """
    # Convert the year into a string format and concatenate to form the band name.
    year_string = ee.Number(year).toInt().format()
    band_name = ee.String("rural_").cat(year_string)

    # Select the rural binary band for the given year.
    final_rural_binary = renamed_rural.select([band_name])

    # This step ensures the binary is converted to boolean where True indicates rural areas.
    # It's necessary for the `cumulativeCost` function to identify source locations.
    rural_binary_true = final_rural_binary.gt(0)

    # Initialize an image where all values are set to a large number (50km) as the default cost.
    # This sets the maximum considered distance to rural areas.
    max_distance_value = 50000  # in meters
    over_rural = ee.Image().toByte().paint(region.geometry(), max_distance_value)

    # Create an even surface image to serve as the base for cost calculations.
    landscape = ee.Image().toByte().paint(region.geometry(), 1)

    # Calculate the cumulative cost from each cell to the nearest rural cell.
    cumulative_cost_rural = landscape.cumulativeCost(
        source=rural_binary_true,
        maxDistance=max_distance_value,
        geodeticDistance=False  # Use planar distance calculations for speed.
    ).rename(ee.String("rural_").cat(year_string))

    # Unmask using the `overRural` image which effectively limits maximum distance calculations.
    cumulative_cost_rural = cumulative_cost_rural.unmask(over_rural)

    # Set any value greater than the max distance (50km) to exactly 50km.
    # This ensures consistency across the output image.
    condition = cumulative_cost_rural.lte(max_distance_value)
    final_cumulative_cost_rural = cumulative_cost_rural.updateMask(condition).unmask(max_distance_value).clip(region)

    return ee.Image(final_cumulative_cost_rural)

# Loop over years
cc_images = ee.ImageCollection(years.map(map_cost));
cc_image = cc_images.toBands();

# Update band names
cc_names = cc_image.bandNames();
new_cc_names = cc_names.map(remove_index);
renamed_cc = cc_image.select(cc_names, new_cc_names);

##-------------------##
## Export image      ##
##-------------------##
# asset ID
cc_id = 'projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/ruralCostMapInterpolated'

# Save the collection
task = ee.batch.Export.image.toAsset(
    image= renamed_cc,
    description ='ruralCostMapInterpolated',
    assetId = cc_id,
    region=region.geometry(),
    scale=1000,
    crs='EPSG:4326'
)
task.start()


##-----------------------------------##
## Get average cost per SDM feature  ##
##-----------------------------------##
# read in images from mapRural
cost_maps = ee.Image('projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/ruralCostMapInterpolated')

reducer2 = ee.Reducer.mean()

finalFeature = cost_maps.reduceRegions(
      collection = buffer_points,
                      reducer = reducer2,
                      scale = 1000,
                      crs = 'EPSG:4326',
                      tileScale = 16);

# Export the image sample feature collection to Drive as a CSV file.
task = ee.batch.Export.table.toDrive(
    collection=finalFeature,
    description='schisto_ruralCC_interpolated_oct162023',
    folder='final_schisto_data',
    fileFormat='CSV',
)
task.start()

##------------------------------------------------##
## Read data back in & clean for later analysis  ##
##-----------------------------------------------##
import pandas as pd
from google.colab import drive

# Mount Google Drive to access the data file stored in Google Colab.
drive.mount('/content/drive')

# Load the data from a CSV file into a pandas DataFrame.
file_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/schisto_ruralCC_interpolated_oct162023.csv'
df = pd.read_csv(file_path)

# Display the column names of the DataFrame to understand its structure.
print("Columns in the DataFrame:", df.columns)

# Subset the data to include only the relevant columns for further processing.
desired_indices = list(range(3, 14)) + [16]
df_sub = df.iloc[:, desired_indices]
print("Selected Columns for Processing:", df_sub.columns)

# Reshape the DataFrame from wide format to long format.
df_long = pd.melt(df_sub, id_vars=['row_code', 'year'], var_name='year2', value_name='dist_rural')

# Drop any rows with missing values
df_long = df_long.dropna()

# Match year2 and year in order to match year of observation andyear of pop data
df_long['year2'] = df_long['year2'].str.extract(r'(\d+)')
df_long['year2'] = pd.to_numeric(df_long['year2'])
df_long = df_long[df_long['year'] == df_long['year2']]

# Remove the temporary 'year2' column as it is no longer needed after matching
df_long = df_long.drop(columns=['year2'])
print(df_long.head()) # check data

# Display the total number of rows to verify the data has been processed correctly.
print("Number of rows in the long format DataFrame:", len(df_long))

# Perform a sanity check to list unique years in the dataset to ensure data consistency.
unique_years = df_long['year'].unique()
print("Unique Years in the Dataset:", unique_years)

# Export the cleaned and transformed data back to a CSV file.
output_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/cleaned_schisto_ruralCC_interpolated_oct162023.csv'
df_long.to_csv(output_path, index=False)
