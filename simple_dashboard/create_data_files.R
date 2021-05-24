library(raster)
library(sf)
library(gdalUtils)
library(DBI)
library(RPostgres)
library(rpostgis)
library(dplyr)
library(dbplyr)
library(VTatlas)


# Read in the GBIF database #
GBIF_Db <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = 'vt-gbif-data',
                          host = 'localhost', #
                          port = 5432, #
                          user = 'postgres',
                          password = 'password')

Spatial_Db <- DBI::dbConnect(RPostgres::Postgres(),
                             dbname = 'vt-atlas-spatial',
                             host = 'localhost',
                             port = 5432,
                             user = 'postgres',
                             password = 'password')

# Get the number of species 
num_spp <- dbGetQuery(GBIF_Db,
                      'SELECT COUNT(DISTINCT("species"))
                       FROM occurrence;')

# save to datafile
write.csv(num_spp, "simple_dashboard/input_data/num_spp.csv", row.names = FALSE)

# Number of observers 
num_obs <- dbGetQuery(GBIF_Db,
                      'SELECT COUNT(DISTINCT("recordedBy"))
                       FROM occurrence;')

# save to datafile
write.csv(num_obs, "simple_dashboard/input_data/num_obs.csv", row.names = FALSE)

# Total number of observations 
tot_obs <- dbGetQuery(GBIF_Db,
                      'SELECT COUNT(DISTINCT("gbifID"))
                             FROM occurrence;')

# save to datafile
write.csv(tot_obs, "simple_dashboard/input_data/tot_obs.csv", row.names = FALSE)


# Get summary statistics to put in a table # 
sql_command <- 'SELECT class, kingdom, count(distinct(species)) AS total_species 
                FROM occurrence
                GROUP BY kingdom, class'

summary_classes <- dbGetQuery(GBIF_Db, sql_command)

write.csv(summary_classes, "simple_dashboard/input_data/summary_of_species_classes.csv")


# Write spatial data to file to read # 
# These data are for the Biodiversity panel # 
# COUNTIES #
VT <- sf::st_read(Spatial_Db,
                  "boundary")

counties <- sf::st_read(GBIF_Db,
                        "counties")

sql_command <- 'SELECT counties."CNTYNAME", kingdom, count(distinct(species)) AS total_species 
                FROM counties
                LEFT JOIN occurrence ON st_contains(counties.geometry,occurrence.geometry)
                GROUP BY counties."CNTYNAME", kingdom;'

county_class <- dbGetQuery(GBIF_Db, sql_command)

county_kingdom <- tapply(as.numeric(county_class$total_species),
                         list(county_class$CNTYNAME,
                              county_class$kingdom),
                         FUN = sum, na.rm = TRUE)

county_kingdom <- data.frame(county_kingdom)

county_kingdom$Total_spp <- apply(county_kingdom, 1, sum, na.rm = TRUE)

county_kingdom$CNTYNAME <- rownames(county_kingdom)

county_class_sf <- merge(counties, county_kingdom, by = 'CNTYNAME')

county_class_sf <- county_class_sf[,c("CNTYNAME","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis")]

county_class_sf[is.na(county_class_sf)] <- 0

# Write to file #
st_write(county_class_sf, "simple_dashboard/input_data/county_class_sf.shp", append = FALSE)


# Towns # 

towns <- sf::st_read(GBIF_Db,
                        "towns")

sql_command <- 'SELECT towns."TOWNNAME", kingdom, count(distinct(species)) AS total_species 
                FROM towns
                LEFT JOIN occurrence ON st_contains(towns.geometry,occurrence.geometry)
                GROUP BY towns."TOWNNAME", kingdom;'

towns_class <- dbGetQuery(GBIF_Db, sql_command)

towns_kingdom <- tapply(as.numeric(towns_class$total_species),
                         list(towns_class$TOWNNAME,
                              towns_class$kingdom),
                         FUN = sum, na.rm = TRUE)

towns_kingdom <- data.frame(towns_kingdom)

towns_kingdom$Total_spp <- apply(towns_kingdom, 1, sum, na.rm = TRUE)

towns_kingdom$TOWNNAME <- rownames(towns_kingdom)

towns_class_sf <- merge(towns, towns_kingdom, by = 'TOWNNAME')

towns_class_sf <- towns_class_sf[,c("TOWNNAME","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis")]

towns_class_sf[is.na(towns_class_sf)] <- 0

# Write to file #
st_write(towns_class_sf, "simple_dashboard/input_data/towns_class_sf.shp", append = FALSE)

# Species accumulation 
sppCumm <- dbGetQuery(GBIF_Db,
                      'SELECT *, count(*) over (order by "year" ) cum_species
                       FROM (
                       SELECT "species", min("year") "year" FROM occurrence GROUP BY "species"
                       ) occurrence')

write.csv(sppCumm, "simple_dashboard/input_data/sppCumm.csv", row.names = FALSE)




# COUNTIES #
VT <- sf::st_read(Spatial_Db,
                  "boundary")

counties <- sf::st_read(GBIF_Db,
                        "counties")

survey_blocks <- sf::st_read(GBIF_Db,
                             "survey_blocks")

sql_command <- 'SELECT survey_blocks."BLOCKNAME", kingdom, count(distinct(species)) AS total_species 
                FROM survey_blocks
                LEFT JOIN occurrence ON st_contains(survey_blocks.geometry,occurrence.geometry)
                GROUP BY survey_blocks."BLOCKNAME", kingdom;'

block_class <- dbGetQuery(GBIF_Db, sql_command)

block_kingdom <- tapply(as.numeric(block_class$total_species),
                         list(block_class$BLOCKNAME,
                              block_class$kingdom),
                         FUN = sum, na.rm = TRUE)

block_kingdom <- data.frame(block_kingdom)

block_kingdom$Total_spp <- apply(block_kingdom, 1, sum, na.rm = TRUE)

block_kingdom$BLOCKNAME <- rownames(block_kingdom)

block_class_sf <- merge(survey_blocks, block_kingdom, by = 'BLOCKNAME')

block_class_sf <- block_class_sf[,c("BLOCKNAME","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis")]

block_class_sf[is.na(block_class_sf)] <- 0

# Write to file #
st_write(block_class_sf, "simple_dashboard/input_data/block_class_sf.shp", append = FALSE)



# here is a basic example of the species accumulation curves # 
# Species accumulation files need to be run 



plotSppAccum(data_array = spp_accum,
             sample_array = samp_accum,
             columnValue = "Insecta",
             predSamp = 8000)


