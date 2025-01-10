# biomphalaria-sdm-brazil
> Code for: *Climate and urbanization drive changes in the habitat suitability of Schistosoma mansoni competent snails in Brazil*.
Caroline K. Glidden, Alyson L. Singleton, Andrew Chamberlin, Roseli Tuan, Raquel G.S. Palasio, Roberta Lima Caldeira, Antônio Miguel V. Monteiro, Kamazima M. M. Lwiza, Ping Liu, Vivian Silva, Tejas S. Athni, Susanne H. Sokolow, Erin A. Mordecai, Giulio A. De Leo
bioRxiv 2024.01.03.574120; doi: https://doi.org/10.1101/2024.01.03.574120
> Code written by Caroline K. Glidden; please contact cglidden@stanford.edu with any questions.
>
This repo contains
1) Occurrence and background points in the zipped file 'all_points_schisto_sdm' and is also available as a feature collection (```ee.FeatureCollection('users/cglidden/all_points_schisto_sdm');```)
2) Python code (GEE API) to download environmental feature data from Google Earth Engine (code in the 'schisto-env-download' directory)
 >> This code will save cvs and geoTiffs that will be used in the R scripts, for the code to work with the specified paths, download the data into a 'raw-data/gee-data' directory/subdirectory
> > The following files produce the following datasets used in the R script:
> > * For R script 2.1 & 2.2:
> >   *  'baseline-vars-schisto.ipynb' produces 'raw-data/gee-data/all_points_sdm_misc_vars.shp'
> >   *  'landuse-schisto-vars.ipynb' produces 'raw-data/gee-data/all_points_sdm_lulc_crops.csv'
> >   *   'create-distance-to-rural.ipynb' produces 'raw-data/gee-data/cleaned_schisto_ruralCC_oct102023.csv'
> >   *  'create-distance-to-urban-AR.ipynb' produces 'raw-data/gee-data/cleaned_schisto_urbanCC_AR_oct102023.csv'
> >   *  'create-distance-to-urban-UN.ipynb' produces 'raw-data/gee-data/cleaned_schisto_urbanCC_UN_oct102023.csv'
> >   *  'download-geotiffs-predictions.ipynb' produces geoTiffs for R script 'general-code/6-prepare-scenario-data.R'  
3) R code to clean feature data, evaluate model performance, test model on historical data, and create Brazil-wide predictions (historical, current, and counterfactual scenarios) in 'code' directory. The code is numbered in the order it should be run. A portion of the analysis is run separately for each species, with the scripts found in species specific directorys, and titled with an a (indicating *B. glabrata*), a b (indicating *B. straminea*), or a c (indicating *B. tenagophila*). Model performance, variable importance, and partial dependence plots were created for analyses using the South American definition of an urban (indicated by a ii at the start of the script title and AR at the end) or the UN definition of urban area (indicated by a i at the start of the script title and UN at the end).
   * *Clean feature data scripts:* 'general-code/1-join-feature-collections.R', 'general-code/2.1-clean-merge-training-data.R', 'general-code/2.2-clean-merge-training-data.R'
   * *Thin data sets scripts:* '**(species name)**/ 3i **(a, b, c)** -prep- **(species name)**-training-data.R', '**(species name)**/ 3ii **(a, b, c)** -prep- **(species name)**-test-data.R'
     * e.g., 'glabrata/3ia-prep-glabrata-training-data.R', 'glabrata/3iia-prep-glabrata-test-data.R'
   * *Evaluate model performance via 5-fold CV:* '**(species name)**/ 4 **(a, b, c)** - **(species name)** -sdm-brt.R'
     * e.g., 'glabrata/4a-glabrata-sdm-brt.R'
   * *Test model performance & inspect results via variable contribution and partial dependence plots:* '**(species name)**/ 5 **(a, b, c)** - **(species name)**-variableImp.R'
     * e.g., 'glabrata/5a-glabrata-variableImp.R'
   * *Prepare scenario data for Brazil-wide predictions:* 'general-code/6-prepare-scenario-data.R'
   * *Estimate Brazil-wide predictions:* '**(species name)**/ 7 **(a, b, c)** - **(species name)**-prediction-maps-AR.R'
     * e.g., 'glabrata/7a-glabrata-prediction-maps-AR.R
>> For the R code to work using the paths specified in each script, you will need to also create a 'clean-data' and 'output' directory, each with a sub-directory for each species.
