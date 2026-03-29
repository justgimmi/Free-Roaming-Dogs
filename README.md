# Street Dogs & Cats in Portugal
This is the repository where I will upload all the code for the project with Dr. Jafet Belmont Osuna during my visiting period in Glasgow university.
For reproducibility, we report all the info about the CRS of the area (CRS:  EPSG:3035 - ETRS89-extended / LAEA Europe)

## Data Folder ## 
- Conteos folder:
  - covs.tif : environmental covariate raster files
  - denspop_2.tif: population density raster in 2021
  - network.shp: traffic network shapefile
  - Quadriculas_transectos_Portugal.shp: grid shape files containing urbanization gradient

- Camera Trap  --> Raw cats & dogs camera trap sightseeing
- Dogs_records_final.csv: dogs camera trap after 30 min filtering to avoid multiple counts of the same individual
- Database_Dogs&Cats:
  - Counts_dog.shp + Counts_cat.shp : grid cell counts
  - hfp_2.tif: human footprint raster
  - PO_dog.shp + PO_cat.shp: roadkills data set
  - PA_dog.shp + PA_cat.shp: re-classification into presence absence Data for counts data
