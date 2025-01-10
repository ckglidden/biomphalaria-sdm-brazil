##---------------------
## This code downloads and cleans distance to rural areas with WorldPop for Glidden et al. 2023 Nat Communications
## NOTE: This is written to be run in Google Colab
## You may have to request access to the assetts
##---------------------
import ee
import pandas as pd
from google.colab import drive

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the Earth Engine library
ee.Initialize()

##-----------------------##
## Set up data           ##
##-----------------------##
# Define the geographic region of interest - Brazil.
region = ee.FeatureCollection('FAO/GAUL_SIMPLIFIED_500m/2015/level0').filter(ee.Filter.equals('ADM0_NAME', 'Brazil'))

# Load point data from Earth Engine user assets.
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

def buffer_points(radius, bounds=True):
    """Apply a buffer to points, optionally bounded to square pixels."""
    def buffer_feature(pt):
        pt = ee.Feature(pt)
        return ee.Algorithms.If(bounds, pt.buffer(radius).bounds(), pt.buffer(radius))
    return buffer_feature

# Buffer points for spatial analysis, setting radius to 500 meters.
buffered_points = all_sdm_points.map(buffer_points(500))

# Load and process WorldPop population data
worldPop = (ee.ImageCollection("WorldPop/GP/100m/pop")
            .filterMetadata('country', 'equals', 'BRA')
            .toBands()
            .unmask(ee.Image().toByte().paint(region.geometry(), 1).multiply(0))
            .clip(region))

# Convert WP to 1km
projection = worldPop.projection()
projectionAt1k = projection.atScale(1000)

pop1k = (worldPop.reduceResolution(
            reducer=ee.Reducer.sum().unweighted(),
            maxPixels=1024)
        .reproject(crs=projectionAt1k))

# list of years to map over
years = ee.List.sequence(2000, 2020)

##-----------------------------------------------##
## Convert pop images to rural versus non rural  ##
## based on population cutoff (<300 per km2)     ##
##-----------------------------------------------##
def map_rural(year):
    """Identify rural areas based on population threshold (< 300 per 1km) for specified years."""
    yearString = ee.Number(year).toInt().format()
    band_name = ee.String("BRA_").cat(yearString).cat("_population")

    ruralMax = 300

    ruralMask = (pop1k.select([band_name]).lt(ruralMax)
    .rename(ee.String("rural_").cat(ee.Number(year).toInt().format())));

    return ee.Image(ruralMask)

def remove_index(band_name):
    return ee.String(band_name).slice(-10)

# Create binary masks for rural areas.
rural_binary = ee.ImageCollection(years.map(map_rural)).toBands()

# Rename the bands
band_names = rural_binary.bandNames()
new_band_names = band_names.map(remove_index)
renamed_rural = rural_binary.select(band_names, new_band_names)

##------------------------------------------------------##
## Create a costmap to represent distance to rural area ##
##------------------------------------------------------##
def map_cost(year):
    """Calculate the cost distance to the nearest rural area for a given year."""
    yearString = ee.Number(year).toInt().format()
    band_name = f"rural_{yearString}"
    final_rural_binary = renamed_rural.select([band_name]).gt(0)
    
    # Create a 50km buffer for rural areas as the maximum cost distance.
    over_rural = ee.Image().toByte().paint(region.geometry(), 50000)
    landscape = ee.Image().toByte().paint(region.geometry(), 1)
    
    cumulative_cost_rural = landscape.cumulativeCost(
        source=final_rural_binary,
        maxDistance=50000,
        geodeticDistance=False
    ).rename(band_name).unmask(over_rural)
    
    # Limit maximum cost to 50km.
    cumulative_cost_rural = cumulative_cost_rural.updateMask(cumulative_cost_rural.lte(50000)).unmask(50000).clip(region)
    return ee.Image(cumulative_cost_rural)

# Loop over years
cc_images = ee.ImageCollection(years.map(map_cost)).toBands()

# Export cost maps to an Earth Engine asset.
asset_id = 'projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/ruralCostMap'
export_task = ee.batch.Export.image.toAsset(
    image=cc_images,
    description='ruralCostMap',
    assetId=asset_id,
    region=region.geometry(),
    scale=1000,
    crs='EPSG:4326'
)
export_task.start()

##-----------------------------------##
## Get average cost per SDM feature  ##
##-----------------------------------##
cost_maps = ee.Image(asset_id)
final_feature = cost_maps.reduceRegions(
    collection=buffered_points,
    reducer=ee.Reducer.mean(),
    scale=1000,
    crs='EPSG:4326',
    tileScale=16
)
drive_task = ee.batch.Export.table.toDrive(
    collection=final_feature,
    description='schisto_ruralCC_oct102023',
    folder='final_schisto_data',
    fileFormat='CSV'
)
drive_task.start()

##------------------------------------------------##
## Read data back in & clean for later analysis  ##
##-----------------------------------------------##
# Mount Google Drive to access the data file
drive.mount('/content/drive')

# Load GEE CC data
file_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/schisto_ruralCC_oct102023.csv'
df = pd.read_csv(file_path)

# Display the columns of the DataFrame to understand what data is included.
print("Columns in DataFrame:", df.columns)

# Select columns for processing
desired_indices = list(range(3, 25)) + [27]
df_sub = df.iloc[:, desired_indices]
print("Columns selected for processing:", df_sub.columns)

# Transform the dataset from wide to long format
df_long = pd.melt(df_sub, id_vars=['row_code', 'year'], var_name='year2', value_name='dist_rural')

# Remove any rows with missing data 
df_long = df_long.dropna()

# Match year2 and year in order to match year of sampling and pop data
df_long['year2'] = df_long['year2'].str.extract(r'(\d+)').astype(int)
df_long = df_long[df_long['year'] == df_long['year2']].drop(columns=['year2'])

# Remove the temporary 'year2' column as it is no longer needed after matching
df_long = df_long.drop(columns=['year2'])
print(df_long.head()) # check data

# Display the total number of rows to verify the data has been processed correctly.
print("Number of rows in processed DataFrame:", len(df_long))

# Perform a sanity check by listing unique years in the dataset to ensure all expected years are present.
unique_years = df_long['year'].unique()
print("Unique Years in processed data:", unique_years)

# Save the processed data back to a CSV file in Google Drive for further use or sharing.
output_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/cleaned_schisto_ruralCC_oct102023.csv'
df_long.to_csv(output_path, index=False)