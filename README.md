# biomphalaria-sdm-brazil
> Code for: *Climate and urbanization drive changes in the habitat suitability of Schistosoma mansoni competent snails in Brazil*.
Caroline K. Glidden, Alyson L. Singleton, Andrew Chamberlin, Roseli Tuan, Raquel G.S. Palasio, Roberta Lima Caldeira, Antônio Miguel V. Monteiro, Kamazima M. M. Lwiza, Ping Liu, Vivian Silva, Tejas S. Athni, Susanne H. Sokolow, Erin A. Mordecai, Giulio A. De Leo
bioRxiv 2024.01.03.574120; doi: https://doi.org/10.1101/2024.01.03.574120
> Code written by Caroline K. Glidden
>
This repo contains
1) Occurrence and background points in the zipped file 'all_points_schisto_sdm' and is also available as a feature collection (```ee.FeatureCollection('users/cglidden/all_points_schisto_sdm');```)
2) Python code(GEE API) to down load environmental feature data from Google Earth Engine (code in the 'schisto-env-download')
 >> This code will save cvs and geoTiffs that will be used in the R scripts, for the code to work with the specified paths, download the data into a 'raw-data/gee-data' directory/subdirectory
> > The following python codes produce the following datasets used in each R script:
> > * For R script 2.1 & 2.2:
> >  *  'baseline-vars-schisto.ipynb' produces 'all_points_sdm_misc_vars.shp'    
3) R code to clean feature data, evaluate model performance, test model on historical data, and create Brazil-wide predictions (historical, current, and counterfactual scenarios) in 'code' directory
   * Clean feature data scripts: 'general-code/1-join-feature-collections.R', 'general-code/2.1-clean-merge-training-data.R', 'general-code/2.2-clean-merge-training-data.R'
   * Thin data sets scripts (one per snail species; a = *B. glabrata*, b = *B.straminea*, c = *B. tenagophila*): '3i **(a, b, c)** -prep- **(species name)**-training-data.R', '3ii **(a, b, c)** -prep- **(species name)**-test-data.R'
   * Evaluate model performance via 5-fold CV: '4 **(a, b, c)** - **(species name)** -sdm-brt.R'
   * Test model performance & inspect results via variable contribution and partial dependence plots: '5 **(a, b, c)** - **(species name)**-variableImp.R'
   * Prepare scenario data for Brazil-wide predictions: 'general-code/6-prepare-scenario-data.R'
   * Estimate Brazil-wide predictions: '7 **(a, b, c)** - **(species name)**-prediction-maps-AR.R'
>> For the R code to work using the paths specified in each script, you will need to also create a 'clean-data' and 'output' directory, each with a sub-directory for each species.
