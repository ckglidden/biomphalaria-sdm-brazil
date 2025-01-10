##---------------------
## This code downloads and cleans distance to urban areas with WorldPop for Glidden et al. 2023 Nat Communications
## urban cutoff based on local definitions for Latin America
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

years = ee.List.sequence(2000, 2020)
##---------------------------------------------------##
## Define function to get contiguous urban pixels    ##
##---------------------------------------------------##
def mapUrban(year, urbanThreshold=300, urbanMax=1500, intPop = 2500, highPop=150000):
    yearString = ee.Number(year).toInt().format()
    band_name = ee.String("BRA_").cat(yearString).cat("_population")

    urbanThreshold = 300
    urbanMax = 1500

    # for medium to high pop
    urbanMask1 = pop1k.select([band_name]).gte(urbanThreshold).And(pop1k.select([band_name]).lt(urbanMax));
    urbanMask1 = urbanMask1.updateMask(urbanMask1.neq(0));

    # for high pop
    urbanMask2 = pop1k.select([band_name]).gte(urbanMax);
    urbanMask2 = urbanMask2.updateMask(urbanMask2.neq(0));

    urban1vect = urbanMask1.select([band_name]).reduceToVectors(
       geometry = region.geometry(),
        crs=pop1k.projection(),
        scale=1000,
      geometryType='polygon',
        eightConnected=True,
        labelProperty='zone',
        bestEffort=False,
        maxPixels=1e13,
        tileScale=16
    )

    urban2vect = urbanMask2.select([band_name]).reduceToVectors(
        geometry= region.geometry(),
        crs=pop1k.projection(),
        scale=1000,
        geometryType='polygon',
        eightConnected=True,
        labelProperty='zone',
        bestEffort=False,
        maxPixels=1e13,
        tileScale=16
    )

    # now get population per feature
    reducer = ee.Reducer.sum();
    pop_sum_features1 = (pop1k.select([band_name]).reduceRegions(
                      collection = urban1vect,
                      reducer = reducer,
                      scale = 1000,
                      crs = 'EPSG:4326',
                      tileScale = 16));


    pop_sum_features2 = (pop1k.select([band_name]).reduceRegions(
                      collection = urban2vect,
                      reducer = reducer,
                      scale = 1000,
                      crs = 'EPSG:4326',
                      tileScale = 16));

    # filter features
    pop_sum_features_final1 = (pop_sum_features1
                              .filter(ee.Filter.gte('sum', intPop)));

    pop_sum_features_final2 = (pop_sum_features2
                              .filter(ee.Filter.gte('sum', highPop)));

    # convert back to binary image
    final_int_urban = (pop_sum_features_final1
                       .filter(ee.Filter.notNull(['sum']))
                       .reduceToImage(
                          properties = ['sum'],
                          reducer = ee.Reducer.first())
                       .rename(ee.String("contig_Iurban_").cat(ee.Number(year).toInt().format()))
                       .unmask(0).clip(region).gt(0));

    final_high_urban = (pop_sum_features_final2
                       .filter(ee.Filter.notNull(['sum']))
                       .reduceToImage(
                          properties = ['sum'],
                          reducer = ee.Reducer.first())
                       .rename(ee.String("contig_Hurban_").cat(ee.Number(year).toInt().format()))
                       .unmask(0).clip(region).gt(0));

    return ee.Image(final_int_urban.addBands(final_high_urban))

##-------------------------------------------------------------------##
## Create image of urban centers per year; batch bc of memory limits ##
##-------------------------------------------------------------------##
# Define a function to remove first two characters from band name
def remove_prefix(band_name):
    return ee.String(band_name).slice(2)

# Loop over each three-year batch
start_year = 2000
end_year = 2020
batch_size = 3

for start in range(start_year, end_year + 1, batch_size):
    end = start + batch_size - 1
    # Adjust end to not exceed the end year
    if end > end_year:
        end = end_year

    # Generate the list of years for the batch
    years_batch = ee.List.sequence(start, end)

    # Process the urban mapping for each year in the batch
    region_images = ee.ImageCollection(years_batch.map(mapUrban))
    region_image = region_images.toBands()

    # Get the band names
    band_names = region_image.bandNames()

    # Apply the function to each band name to remove prefixes
    new_band_names = band_names.map(remove_prefix)

    # Rename the bands
    renamed_image = region_image.select(band_names, new_band_names)

    # Define the asset ID for the batch
    asset_id = f'projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/contigUrbanAR/contigUrban{start}-{end}'

    # Save the collection to Earth Engine Asset
    task = ee.batch.Export.image.toAsset(
        image=renamed_image,
        description=f'contigUrban{start}-{end}',
        assetId=asset_id,
        region=region.geometry(),
        scale=1000,
        crs='EPSG:4326'
    )
    task.start()

##---------------------------------------##
## Functions to get cumulative cost maps ##
##---------------------------------------##
# Function to generate read back on urban centers
def get_batched_images(start_year, end_year, batch_size):
    images = []
    for start in range(start_year, end_year + 1, batch_size):
        end = start + batch_size - 1
        # Adjust end to not exceed the end year
        if end > end_year:
            end = end_year
        
        # Generate the asset name for the batch
        asset_name = f'projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/contigUrbanAR/contigUrban{start}-{end}'
        
        # Load the image, clip to the region, and append to the list
        batch_image = ee.Image(asset_name).clip(region)
        images.append(batch_image)
    
    return images

# Retrieve all batched images
batched_images = get_batched_images(start_year, end_year, batch_size)

# Combine all batched images into a single image by adding bands
contigUrban = ee.Image(batched_images[0])
for image in batched_images[1:]:
    contigUrban = contigUrban.addBands(image)

# Function to get cost maps per type of urban center per year
def mapCost(year):
    # Convert the year into a string to form unique band names for selection
    yearString = ee.Number(year).toInt().format()
    band_name_int = ee.String("contig_Iurban_").cat(yearString)
    band_name_high = ee.String("contig_Hurban_").cat(yearString)

    # Select bands for intermediate and high urban density using constructed names
    final_int_urban_binary = contigUrban.select([band_name_int])
    final_high_urban_binary = contigUrban.select([band_name_high])

    # Set masked areas to a default value of 50km, creating a boundary for cost calculation
    overUrban = ee.Image().toByte().paint(region.geometry(), 50000)

    # Create a constant image as a landscape over which cost distances will be calculated
    landscape = ee.Image().toByte().paint(region.geometry(), 1)

    # Convert the selected urban area from above into binary (1 for urban, 0 otherwise)
    final_int_urban_binary_v2 = final_int_urban_binary.gt(0)  # maybe unnecessary if already binary

    # Calculate the cumulative cost for intermediate urban density over a 50km distance
    cumulativeCostInt0 = (landscape.cumulativeCost(
        source=final_int_urban_binary_v2,
        maxDistance=50 * 1000,  # Max distance for cost calculation in meters
        geodeticDistance=False)  # Assume planar distance calculation for simplicity
        .rename(ee.String("inte_").cat(yearString))  # Rename to include the density and year
        .unmask(overUrban))  # Replace all masked values with the default 50km

    # Convert distances greater than 50km to exactly 50km
    condition1 = cumulativeCostInt0.lte(50000)  # Create a mask for values <= 50km
    cumulativeCostInt = cumulativeCostInt0.updateMask(condition1).unmask(50000).clip(region)

    # Repeat the process for high urban density
    final_high_urban_binary_v2 = final_high_urban_binary.gt(0)
    cumulativeCostHigh0 = (landscape.cumulativeCost(
        source=final_high_urban_binary_v2,
        maxDistance=50 * 1000,
        geodeticDistance=False)
        .rename(ee.String("high_").cat(yearString))
        .unmask(overUrban))

    condition2 = cumulativeCostHigh0.lte(50000)
    cumulativeCostHigh = cumulativeCostHigh0.updateMask(condition2).unmask(50000).clip(region)

    # Return an image with both cumulative cost layers
    return ee.Image(cumulativeCostInt.addBands(cumulativeCostHigh))

def remove_index(band_name):
    # Function to strip the last 9 characters from a band name, assuming these are indexing-related
    return ee.String(band_name).slice(-9)

##---------------##
## Get cost maps ##
##---------------##
cc_images = ee.ImageCollection(years.map(mapCost))
cc_image = cc_images.toBands()

# updated bands
cc_names = cc_image.bandNames()
new_cc_names = cc_names.map(remove_index)
renamed_cc = cc_image.select(cc_names, new_cc_names)

# asset ID
cc_id = 'projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/urbanCostMapAR'

# Save the collection
task = ee.batch.Export.image.toAsset(
    image= renamed_cc,
    description ='urbanCostMapAR',
    assetId = cc_id,
    region=region.geometry(),
    scale=1000,
    crs='EPSG:4326'
)
task.start()

##------------------------------##
## Get avg cost per SDM feature ##
##------------------------------##
# Read in images from mapUrban
cost_maps = (ee.Image("projects/gbsc-gcp-lab-emordeca/assets/urban_mapping/urbanCostMapAR").clip(region))

reducer2 = ee.Reducer.mean()

finalFeature = cost_maps.reduceRegions(
      collection = buffered_points,
                      reducer = reducer2,
                      scale = 1000,
                      crs = 'EPSG:4326',
                      tileScale = 16);

# Export the image sample feature collection to Drive as a CSV file.
task = ee.batch.Export.table.toDrive(
    collection=finalFeature,
    description='schisto_urbanCC_AR_oct102023',
    folder='final_schisto_data',
    fileFormat='CSV',
)
task.start()

##------------------------------##
## Read back in and clean data  ##
##------------------------------##
# Load drive
drive.mount('/content/drive')

# Load the dataset from a specified path
file_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/schisto_urbanCC_AR_oct102023.csv'
df = pd.read_csv(file_path)
print(df.columns)  # Display the column names to verify correct loading

# Subset the dataset for high urban areas based on column indices and specific additional columns
high_indices = list(range(2, 23)) + [45, 48]  
df_high = df.iloc[:, high_indices]
print(df_high.columns)  # Print column names to confirm correct subsetting

# Similarly, subset the dataset for intermediate urban areas
int_indices = list(range(23, 44)) + [45, 48]  # Adjust these indices as necessary
df_int = df.iloc[:, int_indices]
print(df_int.columns)  # Print column names to confirm correct subsetting

# Convert the high urban area data from wide to long format
df_high_long = pd.melt(df_high, id_vars=['row_code', 'year'], var_name='year2', value_name='dist_high_urbanAR')
df_high_long.dropna(inplace=True)  # Remove any rows with missing data

# Filter data so year of data equals year of observation (for snail sampling)
df_high_long['year2'] = df_high_long['year2'].str.extract(r'(\d+)').astype(int)  # Extract year and convert to integer
df_high_long = df_high_long[df_high_long['year'] == df_high_long['year2']].drop(columns=['year2'])  # Align and clean up

# Repeat the conversion for intermediate urban area data
df_int_long = pd.melt(df_int, id_vars=['row_code', 'year'], var_name='year2', value_name='dist_int_urbanAR')
df_int_long.dropna(inplace=True)
df_int_long['year2'] = df_int_long['year2'].str.extract(r'(\d+)').astype(int)
df_int_long = df_int_long[df_int_long['year'] == df_int_long['year2']].drop(columns=['year2'])

# Merge the long-format datasets on 'row_code' and 'year'
merged_df = pd.merge(df_high_long, df_int_long, on=['row_code', 'year'], how='inner')
print(f"Number of rows in merged data: {len(merged_df)}") # Check row numbers make sense
print(merged_df.head())  # Print first few rows to verify the merge

# Save the merged dataset to a new CSV file
output_path = '/content/drive/MyDrive/GEEexports brazil_schisto_snails/final_schisto_data/cleaned_schisto_urbanCC_AR_oct102023.csv'
merged_df.to_csv(output_path, index=False)
