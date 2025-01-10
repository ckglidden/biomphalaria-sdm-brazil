##---------------------
## This code downloads and cleans mapbiomas LULC for Glidden et al. 2023 Nat Communications
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
# Define the region of interest as Brazil using a simplified global administrative layer
region = ee.FeatureCollection('FAO/GAUL_SIMPLIFIED_500m/2015/level0').filter(ee.Filter.equals('ADM0_NAME', 'Brazil'))

# Load points for schistosomiasis disease modeling (SDM)
all_sdm_points = ee.FeatureCollection('users/cglidden/all_points_schisto_sdm')

# Function to buffer points to a specified radius, optionally returning bounding boxes
def buffer_points(radius, bounds=False):
    def buffer_feature(pt):
        return ee.Algorithms.If(bounds, pt.buffer(radius).bounds(), pt.buffer(radius))
    return buffer_feature

# Apply a 1 km buffer around the points
bufferedPoints = all_sdm_points.map(buffer_points(1000, True))

# Load the Mapbiomas land cover dataset
mapbiomas = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2')

# Asset mapbiomas
asset = 'projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2'

# Numeric attribute to index the shapefile
attribute = "row_code"

# A list of class ids you are interested
classIds = [41, 21] # temp crops, mosaics of use

# Change the scale if you need.
scale = 30

# Territory
territory = ee.FeatureCollection(bufferedPoints)

mapbiomas = ee.Image(asset)

# Image area in km2
pixelArea = ee.Image.pixelArea().divide(1000000);

# Geometry to export
geometry = mapbiomas.geometry();

# Define years of interest
years = [str(year) for year in range(1985, 2021)] 

# LULC classes
classIds = [41, 21]

##------------------------------------##
## Function to get LULC area          ##
##------------------------------------##
def convert2table(obj):
    obj = ee.Dictionary(obj)
    territory = obj.get('territory')
    classesAndAreas = ee.List(obj.get('groups'))

    def mapFunction(classAndArea):
        classAndArea = ee.Dictionary(classAndArea)
        classId = classAndArea.get('class')
        area = classAndArea.get('sum')

        tableColumns = ee.Feature(None)\
            .set(attribute, territory)\
            .set('class', classId)\
            .set('area', area)

        return tableColumns

    tableRows = classesAndAreas.map(mapFunction)
    return ee.FeatureCollection(ee.List(tableRows))

def calculateArea(image, territory, geometry):
    reducer = ee.Reducer.sum().group(groupField=1, groupName='class').group(groupField=1, groupName='territory')

    territoriesData = pixelArea.addBands(territory).addBands(image)\
        .reduceRegion(
            reducer=reducer,
            geometry=geometry,
            scale=scale,
            maxPixels=1e12
        )

    territoriesData = ee.List(territoriesData.get('groups'))
    areas = territoriesData.map(convert2table)
    areas = ee.FeatureCollection(areas).flatten()

    return areas

##---------------------------##
## Process per year          ##
##---------------------------##

def process_year(year):
    image = mapbiomas.select('classification_' + year)

    def calculate_area(feature):
        return calculateArea(
            image.remap(classIds, classIds, 0),
            ee.Image().int64().paint({
                'featureCollection': ee.FeatureCollection(feature),
                'color': attribute
            }),
            feature.geometry()
        )

    areas = territory.map(calculate_area)
    areas = areas.flatten()

    # Set additional properties
    areas = areas.map(lambda feature: feature.set('year', year))

    return areas

areas = ee.List([])

for year in years:
    image = mapbiomas.select('classification_' + year)

    def calculate_area(feature):
      return calculateArea(
        image.remap(classIds, classIds, 0),
        ee.Image().int64().paint(
            featureCollection=ee.FeatureCollection(feature),
            color=attribute
        ),
        feature.geometry()
    )

    areas_year = territory.map(calculate_area)
    areas_year = areas_year.flatten()

    # Set additional properties
    areas_year = areas_year.map(lambda feature: feature.set('year', year))

    areas = areas.add(areas_year)

areas = ee.FeatureCollection(areas).flatten()

##-----------------##
## Export          ##
##-----------------##
export_task = ee.batch.Export.table.toDrive(
    collection=areas,
    description='all_points_sdm_lulc_crops',
    folder='final_schisto_data',
    fileNamePrefix='all_points_sdm_lulc_crops',
    fileFormat='CSV'
)

# Start the export task
export_task.start()