#TLM, Earth Lab, 2024

#Package management
list.of.packages <- c('tidyverse', 'data.table')
install.packages(setdiff(list.of.packages, rownames(installed.packages())))
invisible(lapply(list.of.packages, library, character.only = TRUE))

#Directory management
datDir <- "~/data-store/data/iplant/home/shared/earthlab/linked_disturbance/data"
figsDir <- "~/data-store/data/iplant/home/shared/earthlab/linked_disturbance/figs"
if (!dir.exists(figsDir)){
  dir.create(figsDir)
}

#Load data
#These datasets have already had the following filters applied:
#forestMask == 1 : forest-only data
#collectionYrFire == 0 : remove if fire in collection year
#collectionYrInsect == 0 : remove if insects in collection year
#sensitivit >= 0.95 : GEDI sensitivity
#l2_quality == 1 : GEDI quality flag
dfB <- data.table::fread(file.path(datDir, 'linked_disturbance_data_frame_baby.csv'))

# Remove columns with NA values - a few columns have these due to the way time was dealt with, depending on when the beams are from
dfBClean <- dfB %>%
  select(where(~ all(!is.na(.))))

#View
head(dfBClean)
