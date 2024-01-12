

list.of.packages('tidyverse')

install.packages(setdiff(list.of.packages, rownames(installed.packages())))

datDir <- "~/data-store/data/iplant/home/shared/earthlab/linked_disturbance/data"

figsDir <- "~/data-store/data/iplant/home/shared/earthlab/linked_disturbance/figs"
if (!dir.exists(figsDir)){
  dir.create(figsDir)
}
