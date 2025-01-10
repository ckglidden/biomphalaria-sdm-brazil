##---------------------
## This code downloads static environmental variables for Glidden et al. 2023 Nat Communications
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

# Load the snail occurrence points with a 1km buffer from a pre-defined FeatureCollection.
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

def buffer_points(radius, square=False):
    """Apply a buffer around points; square if specified."""
    def apply_buffer(pt):
        pt = ee.Feature(pt)
        return pt.buffer(radius).bounds() if square else pt.buffer(radius)
    return apply_buffer

# Apply buffer to points, assuming square pixels for uniformity.
buffered_points = all_sdm_points.map(buffer_points(500, True))

# Create a mask to exclude ocean areas in the data.
mask = ee.Image.constant(1).clip(region).mask()

# Create a binary image for later data conversions.
sources = ee.Image.constant(1).toByte().paint(region.geometry(), 1).selfMask()

##-----------------------##
## Get topo data         ##
##-----------------------##
# Load and prepare elevation data from NASA's DEM product.
elevation = ee.Image('NASA/NASADEM_HGT/001').select('elevation').resample("bilinear")\
            .reproject(crs='EPSG:4326', scale=1000).reduce(ee.Reducer.mean()).clip(region)\
            .rename('elevation')

# Calculate terrain slope and aspect from the elevation data.
slope = ee.Terrain.slope(elevation).reproject(crs='EPSG:4326', scale=1000)\
        .reduce(ee.Reducer.mean()).clip(region).rename('slope')

aspect = ee.Terrain.aspect(elevation).reproject(crs='EPSG:4326', scale=1000)\
         .reduce(ee.Reducer.mean()).clip(region).rename('aspect')

##------------------------------------##
## Get hydrology and water features   ##
##------------------------------------##
MERIT = ee.Image('MERIT/Hydro/v1_0_1');

# Hydrology
upa = (MERIT.select('upa')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region).
            rename('upa'));

hnd = (MERIT.select('hnd')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.max())
            .clip(region)
            .rename('hnd'));

# Water surface
waterJRC = ee.Image('JRC/GSW1_3/GlobalSurfaceWater');

occ = (waterJRC.select('occurrence').unmask(sources.multiply(0)).updateMask(mask)
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('waterOccurrence'));

seas = (waterJRC.select('seasonality').unmask(sources.multiply(0)).updateMask(mask)
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('waterSeasonality'));

rec = (waterJRC.select('recurrence').unmask(sources.multiply(0)).updateMask(mask)
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('waterRecurrence'));

# Calculate river distance using a cumulative cost mapping
sheds = ee.FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers").filterMetadata('COUNTRY', 'equals', 'Brazil');

order_sheds = sheds.filterMetadata('RIV_ORD', 'not_greater_than', 8)

river_dist_image = order_sheds.reduceToImage(properties=['BB_ID'], reducer=ee.Reducer.first())\
                  .multiply(0).rename('BB_ID').unmask(sources)

riverDist = river_dist_image.cumulativeCost(source=river_dist_image.eq(0), maxDistance=10 * 1000)\
             .rename('riverDist').toFloat().unmask(sources)

##---------------------------##
## Get soil properties data  ##
##---------------------------##
clay = (ee.Image("OpenLandMap/SOL/SOL_CLAY-WFRACTION_USDA-3A1A1A_M/v02").select('b0')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('clay'));

sand = (ee.Image("OpenLandMap/SOL/SOL_SAND-WFRACTION_USDA-3A1A1A_M/v02").select('b0').rename('sand')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('sand'));

soilWater = (ee.Image("OpenLandMap/SOL/SOL_WATERCONTENT-33KPA_USDA-4B1C_M/v01").select('b0').rename('soilWater')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('soilWater'));

soilCarbon = (ee.Image("OpenLandMap/SOL/SOL_ORGANIC-CARBON_USDA-6A1C_M/v02").select('b0').rename('soilCarbon')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('soilCarbon'));

pH = (ee.Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02").select('b0').rename('pH')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('pH'));

bulk = (ee.Image("OpenLandMap/SOL/SOL_BULKDENS-FINEEARTH_USDA-4A1H_M/v02").select('b0').rename('bulk')
            .reproject(crs='EPSG:4326', scale=1000)
            .reduce(ee.Reducer.mean())
            .clip(region)
            .rename('bulk'));

##----------------------------------##
## Global human modification index  ##
##----------------------------------##
GHM = ee.ImageCollection('CSP/HM/GlobalHumanModification').first();

##---------------------##
## Combind and export  ##
##---------------------##
final_image = (GHM.addBands([elevation, slope, aspect, occ,
  seas, rec, riverDist, clay, sand, soilWater, soilCarbon, pH, bulk, upa, hnd]))


# Export image to drive
export_task = ee.batch.Export.table.toDrive(
    collection=final_image.reduceRegions(
        collection=buffered_points,
        reducer=ee.Reducer.mean(),
        scale=1000,
        tileScale=16
    ),
    description='all_points_sdm_misc_vars',
    folder='final_schisto_data',
    fileFormat='SHP'
)

# Start the export task
export_task.start()
