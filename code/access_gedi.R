# Install didn't work right away because of R versioning but did eventually...
# install.packages("devtools")
# devtools::install_github("VangiElia/GEDI4R")
#loading GEDI4R package
library(GEDI4R)
library(sf)

# Define date range
start_date <- "2019-01-01"
end_date <- "2023-01-01"

# Load GYE shapefile outline
sgye <- st_read('../data/srecoreg.kml') 

# Extract extent
e <- raster::extent(sgye)
ul_lat <- e@ymax
lr_lat <- e@ymin
ul_lon <- e@xmin
lr_lon <- e@xmax

dir.create('../data/gedi_download')
# file.copy("C:\\Users\\hoecker\\AppData\\Local\\Temp\\Rtmpo9P2zR", "../data/gedi_download", recursive=TRUE)

outdir = '../data/gedi_download' #tempdir()

#Get the list of all files available for the study area in the period selected,
#using just_path = T
file_path <- l4_download(
  ul_lat,
  lr_lat,
  ul_lon,
  lr_lon,
  outdir = outdir,
  from = start_date,
  to = end_date,
  just_path = T
)

# Download the first 15 files (because I have 15 cores)
# This downloads 15 of 1727 complete tracks/orbits that cross the GYE
file_download <- l4_download(
  ul_lat,
  lr_lat,
  ul_lon,
  lr_lon,
  ncore = parallel::detectCores()-1,
  outdir = outdir,
  from = start_date,
  to = end_date,
  just_path = F,
  subset=1:15
)

# Extract multiple paths of GEDI data from downloaded files
l4_data <- l4_getmulti(file_download)

# # Clip the data to the study area using sgye outline
l4_sgye <- l4_clip(l4_data, clip= sgye, usegeometry = T)

# Convert the data to shapefile
dir.create('../data/gedi_sgye')
converted <- l4_convert(l4_sgye,
                        epsg = 4326,
                        filename = '../data/gedi_sgye/gedi_sgye.shp',
                        return_path = T)

l4_sgye_sf <- sf::read_sf(converted)

# Eventually delete the full paths from the outdir - skipping for...

#-------------------------------------------------------------------------------
# Not sure why this step is repeadted so many times...but it is quick
plot_data <- l4_getmulti(l4_sgye)

# Plot footprint locations and AGBD distribution against elevation
l4_plotagb(as.data.frame(l4_sgye),
           beam_id="all",
           type = "both",
           n=100,
           h=c(100,100))

# Plot along-track AGBD profile
l4_plotprofile(as.data.frame(l4_sgye),beam_id="all")






