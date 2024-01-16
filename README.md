# biomphalaria-sdm-brazil
> Code for: *Climate and urbanization drive changes in the habitat suitability of Schistosoma mansoni competent snails in Brazil*.
Caroline K. Glidden, Alyson L. Singleton, Andrew Chamberlin, Roseli Tuan, Raquel G.S. Palasio, Roberta Lima Caldeira, Antônio Miguel V. Monteiro, Kamazima M. M. Lwiza, Ping Liu, Vivian Silva, Tejas S. Athni, Susanne H. Sokolow, Erin A. Mordecai, Giulio A. De Leo
bioRxiv 2024.01.03.574120; doi: https://doi.org/10.1101/2024.01.03.574120
> Code written by Caroline K. Glidden
>
This repo contains
1) Python code(GEE API) to down load environmental feature data from Google Earth Engine (code in the 'schisto-env-download')
2) R code to clean feature data, evaluate model performance, test model on historical data, and create Brazil-wide predictions (historical, current, and counterfactual scenarios) in 'code' directory
   * Clean feature data scripts: 'general-code/1-join-feature-collections.R', 'general-code/2.1-clean-merge-training-data.R', 'general-code/2.2-clean-merge-training-data.R'
   * Thin data sets scripts (one per snail species; a = *B. glabrata*, b = *B.straminea*, c = *B. tenagophila*): '3i **(a, b, c)** -prep- **(species name)**-training-data.R', '3ii **(a, b, c)** -prep- **(species name)**-test-data.R'
   * Evaluate model performance via 5-fold CV: '4 **(a, b, c)** - **(species name)** -sdm-brt.R'
   * Test model performance & inspect results via variable contribution and partial dependence plots: '5 **(a, b, c)** - **(species name)**-variableImp.R'
   * Prepare scenario data for Brazil-wide predictions: 'general-code/6-prepare-scenario-data.R'
   * Estimate Brazil-wide predictions: '7 **(a, b, c)** - **(species name)**-prediction-maps-AR.R'

 > Occurrence and background points are in the zipped file 'all_points_schisto_sdm' and is also available as a feature collection (```ee.FeatureCollection('users/cglidden/all_points_schisto_sdm');```)
 *
