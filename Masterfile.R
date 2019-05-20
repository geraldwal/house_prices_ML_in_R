source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/load_libraries.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/f_partition.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/Session 4/classification_metrics.R')

# Import data
houses <- fread("https://gist.githubusercontent.com/geraldwal/305a85689f1bb1dec1d3087a3dbc2ba8/raw/4ced0740f86ddc5b2b7a75aa8bd9769ae511722b/house_prices_individual_r")
head(houses)