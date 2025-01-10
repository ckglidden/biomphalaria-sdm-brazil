
##---------------------
## This code downloads climate variables for Glidden et al. 2023 Nat Communications
## NOTE: This is written to be run in Google Colab
##---------------------
import ee

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the Earth Engine library
ee.Initialize()

##-----------------------##
## Set up data           ##
##-----------------------##
# Define the region of interest as Brazil.
country_filter = ee.Filter.eq('COUNTRY_NA', 'Brazil')
region = ee.FeatureCollection('USDOS/LSIB/2017').filter(country_filter)

# Load the snail occurrence points 
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

##---------------------------------------##
## Get decadal climate data from CHELSA  ##
##---------------------------------------##
# Function to convert ImageCollection to Image with bands
def process_collection(collection_id):
    return (ee.ImageCollection(collection_id)
            .toBands()  # Convert the image collection to an image with multiple bands
            .reproject('EPSG:4326', None, 1000)  # Reproject images to a common resolution
            .clip(region))  # Clip the image to the Brazil region

# Define the decadal periods
decades = {
    '1981_2000': 'users/cglidden/CHELSA_decadal_mean/1981_2000/CHELSA_1981_2000',
    '1991_2000': 'users/cglidden/CHELSA_decadal_mean/1991_2000/CHELSA_1991_2000',
    '2001_2010': 'users/cglidden/CHELSA_decadal_mean/2001_2010/CHELSA_2001_2010',
    '2011_2017': 'users/cglidden/CHELSA_decadal_mean/2011_2017/CHELSA_2011_2017'
}

# Process each decadal data and combine them into one multi-band image
decadal_chelsa = ee.Image.cat([process_collection(decades[decade]) for decade in decades])

# Load points for extracting climate data
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

# Function to extract climate data at point locations
def extract_climate_data(feature):
    ext_climate = ee.Feature(feature.geometry(), {
        'climate_data': decadal_chelsa.reduceRegion({
            'reducer': ee.Reducer.first(),  # Using first() as a placeholder
            'geometry': feature.geometry(),
            'scale': 1000
        })
    })
    return ext_climate.copyProperties(feature, ['row_code', 'year'])

# Map the function over all SDM points
extracted = all_sdm_points.map(extract_climate_data)

##---------------##
## Export data   ##
##---------------##
export_task = ee.batch.Export.table.toDrive(
    collection= extracted,
    description='all_sdm_points_chelsa_decadal',
    folder='final_schisto_data',
    fileFormat='CSV'
)

# Start the export task
export_task.start()
