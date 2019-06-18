## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 5/31/2019

# This script is designed to be run as part of
# Region_or_taxGroup_Plots_Code-KM.R and it produces the transformed and
# filtered datasets based on RAM data used to make the region and taxGroup plots
# described in Region_or_taxGroup_Plots_Code-KM.R

library(plyr)
library(dplyr)
library(stringr)


# ***** Notes on naming convention:
# - all variables and dataframes used in the region plots begin with "region"
#     * For example, "region_legend_order" is the order that the taxonomy groups
#       are put in for the color legend in the region plots
# - the taxGroup categories are different in the timeseries_values_views dataframe
#   compared to the All_TBbest.df dataframe; for all plots that use
#   All_TBbest.df as the data source, use variables and dataframes starting with
#   "TB_taxGroup"; the ones starting with "taxGroup" can be used for plots and
#   tables using data from the timeseries_values_views dataframe.

# *** If not run with source() from Region_or_taxGroup_Plots_Code-KM.R, set 
# source directory to wherever the RAM Files (v4.44) folder is *******


##################################################################################
######################### RAM Data sets ##########################################
##################################################################################

# File with majority of data for all stocks:
timeseries_values_views <- read.csv(here::here("Data/RAM Files (v4.44)/views tables/timeseries_values_views.csv"))

# File that contains stockid mapped to region and scientificname:
stock_info <- read.csv(here::here("Data/RAM Files (v4.44)/ram tables/stock.csv"))

# File that contains taxGroup (and FisheryType) mapped to scientificname:
taxonomy <- read.csv(here::here("Data/RAM Files (v4.44)/ram tables/taxonomy.csv"))

# On windows computers, the first column names in the imported dataframes stock.csv 
# and taxonomy.csv had extra characters added, so correcting for that:
colnames(stock_info)[1] <- "stockid"
colnames(taxonomy)[1] <- "tsn"

# Adding region, scientificname, FisheryType and taxGroup variables to 
# timeseries_values_views dataframe:
timeseries_values_views$region <-
  stock_info$region[match(timeseries_values_views$stockid, stock_info$stockid)]
timeseries_values_views$scientificname <-
  stock_info$scientificname[match(timeseries_values_views$stockid, stock_info$stockid)]
timeseries_values_views$FisheryType <-
  taxonomy$FisheryType[match(timeseries_values_views$scientificname, taxonomy$scientificname)]
timeseries_values_views$taxGroup <-
  taxonomy$taxGroup[match(timeseries_values_views$scientificname, taxonomy$scientificname)]

# Change some region values to make them match plots from state space model 
# output:
timeseries_values_views$region <- timeseries_values_views$region %>%
  sub("Russia Japan", "Northwest Pacific", .) %>%
  sub("Europe non EU", "Norway, Iceland, Faroe Islands", .) %>%
  sub("European Union", "European Union (non Mediterranean)", .)

# Capitalize taxGroup values (makes things easier in terms of matching the legend)
timeseries_values_views$taxGroup <- str_to_title(timeseries_values_views$taxGroup)

# Extracting the stocks that have data in the TBbest column into a new 
# dataframe:
# ************(used in the majority of the plots)***************
All_TBbest.df <- subset(timeseries_values_views, 
                        is.na(timeseries_values_views$TBbest) == FALSE)

##################################################################################
######### Parameters for All Data Transformations & Segmentations ################
#################################################################################

# if there are NAs in either region or taxGroup, stop operation
if (any(is.na(timeseries_values_views$region) == TRUE) | 
    any(is.na(timeseries_values_views$taxGroup) == TRUE)) { 
  stop(paste("timeseries_values_views has NAs in region and/or taxGroup column"))
}

# Region lists:
regions <- unique(timeseries_values_views$region)
salmon_reg <- c("Canada West Coast (Pacific Salmon)", 
                "US Alaska (Pacific Salmon)", 
                "Northwest Pacific (Pacific Salmon)", 
                "US West Coast (Pacific Salmon)") 
# use regions to match with timeseries_values_views$region, All_TBbest.df$region 
# and surplus$region
regions <- regions[regions != salmon_reg] 
regions_plot_titles <- regions
number_regions <- length(regions)
# use region_labels for naming region dataframes inside lists
region_labels <- gsub(" ", "_", regions)

# Taxonomy group lists for timeseries_values_views data:
taxGroup_list <- unique(timeseries_values_views$taxGroup)
number_taxGroups <- length(taxGroup_list)
taxGroup_plot_titles <- taxGroup_list
taxGroup_labels <- gsub(" |-", "_", taxGroup_list)
# Taxonomy group lists for All_TBbest.df data:
TB_taxGroup_list <- as.character(taxGroup_list[taxGroup_list %in% unique(All_TBbest.df$taxGroup)])
number_TB_taxGroups <- length(TB_taxGroup_list)
TB_taxGroup_plot_titles <- TB_taxGroup_list
TB_taxGroup_labels <- gsub(" |-", "_", TB_taxGroup_list)
##### use TB_taxGroup_list for matching with All_TBbest.df$taxGroup ###########

# year range:
#year_min <- 1950 - this is defined in Region_or_taxGroup_Plots_Code-KM.R
year_max <- max(timeseries_values_views$year) - 1 # there were no TBbest numbers for 2017, the max year
year_range <- year_max - year_min + 1
years <- c(year_min:year_max)
stock_count_years <- seq(year_min, year_max, by = 5)

# stocks:
stock_ids <- unique(timeseries_values_views$stockid)
number_stocks <- length(stock_ids)

# Separate out timeseries_values_views and All_TBbest.df data into lists of 
# dataframes, one list separated by regions (excluding the salmon regions), the 
# other by taxGroup 
seg_timeseries_values_views <- timeseries_values_views[timeseries_values_views$region %in% regions, ]
timeseries_values_views_region_list <- split(seg_timeseries_values_views, 
                                     seg_timeseries_values_views$region)
timeseries_values_views_taxGroup_list <- split(timeseries_values_views,
                                                   timeseries_values_views$taxGroup)

seg_All_TBbest.df <- All_TBbest.df[All_TBbest.df$region %in% regions, ]
All_TBbest.df_region_list <- split(seg_All_TBbest.df, seg_All_TBbest.df$region)
All_TBbest.df_taxGroup_list <- split(All_TBbest.df, All_TBbest.df$taxGroup)

################################################################################
########################### Summary Dataframes  ################################
################################################################################

# Number of stocks, taxonomy groups and first & last year with data in each 
# region, used to produce summary tables at top of region pages:
stock_tax_per_region <- summary_fun(type_of_plot = "region", 
                                    input_data = timeseries_values_views_region_list, 
                                    number_taxGroup_or_region = number_regions, 
                                    row_names = regions_plot_titles)

# Number of stocks, regions and first & last year with data in each taxonomy 
# group, used to produce summary tables at top of taxonomy group pages:
stock_region_per_taxgroup <- summary_fun(type_of_plot = "taxGroup", 
                                         input_data = timeseries_values_views_taxGroup_list, 
                                         number_taxGroup_or_region = number_taxGroups, 
                                         row_names = taxGroup_labels)

# Version of the above with the All_TBbest.df; this will tell what to expect 
# in the biomass coverage plots
TB_stock_tax_per_region <- summary_fun(type_of_plot = "region", 
                                         input_data = All_TBbest.df_region_list, 
                                         number_taxGroup_or_region = number_regions, 
                                         row_names = regions_plot_titles)

TB_stock_region_per_taxgroup <- summary_fun(type_of_plot = "taxGroup", 
                                            input_data = All_TBbest.df_taxGroup_list, 
                                            number_taxGroup_or_region = number_TB_taxGroups, 
                                            row_names = TB_taxGroup_plot_titles)


################################################################################
############ Color palettes for regions & taxGroups (for plots) ################
################################################################################

# Setting the taxGroup levels to be a specific color (and in a specific order) 
# in all region plots:
region_legend_order <- c("Gadids", "Pleuronectids", "Sebastids", "Other Scorpaenids",
                         "Forage Fish", "Carangids-Mackerels", "Tuna-Billfish",
                         "Elasmobranchs", "Other Marine Percoidids", "Other Marine Fish",
                         "Salmonids", "Eels", "Crabs-Lobsters", "Shrimps", 
                         "Bivalves-Gastropods", "Cephalopods", "Echinoderms")

region_myColors <- c("yellowgreen", "palegreen", "tomato", "pink", "darkorange", "steelblue2", "violet", "mediumpurple", 
              "burlywood", "slategray1", "firebrick3", "khaki", "gold", "gray91", 
              "gray", "gray42", "darksalmon")
names(region_myColors) <- region_legend_order

# Setting the region levels to be a specific color for all taxGroup plots:

taxGroup_myColors <- c("yellow2", "violetred1", "turquoise3", "tomato3", 
                       "steelblue3", "springgreen3", "slateblue3", "cyan",
                       "firebrick3", "plum3", "orangered1", "darkorchid3", "lightskyblue3",
                       "gold3", "darkseagreen3", "chartreuse3", "azure3", "azure4",
                       "darkseagreen3", "darksalmon")
names(taxGroup_myColors) <- regions


################################################################################
########## Dataframes with Mean Biomass (Used for Biomass Coverage plots) ######
################################################################################

# Calculate average biomass over the time series for each stock in each region and 
# extract the first and last years that each stock appears in the assessment 
region_mean_biomass <- mean_biomass_fun("Region",
                                        regions,
                                        number_regions,
                                        All_TBbest.df_region_list,
                                        #regions,
                                        year_min)

# Order factor levels for taxGroup so they will appear in a specific order in
# the color legend of the region plots:
for (i in 1:number_regions) {
  region_mean_biomass[[i]]$taxGroup <- factor(region_mean_biomass[[i]]$taxGroup,
                                       levels = region_legend_order)
}

TB_taxGroup_mean_biomass <- mean_biomass_fun("taxGroup",
                                             TB_taxGroup_list,
                                             number_TB_taxGroups,
                                             All_TBbest.df_taxGroup_list,
                                             #TB_taxGroup_list,
                                             year_min)


################################################################################
## Dataframes with Custom Y-axis (Used for Biomass Coverage All Stocks plots) ##
################################################################################

# Create y axis labels for biomass coverage for all stocks by region plots:
region_custom_y_axis <- custom_y_axis_fun(number_regions, 
                                          region_or_taxGroup = regions,
                                          region_mean_biomass)

# Create y axis labels for biomass coverage for all stocks by taxGroup plots:
TB_taxGroup_custom_y_axis <- custom_y_axis_fun(number_TB_taxGroups, 
                                               region_or_taxGroup = TB_taxGroup_list,
                                               TB_taxGroup_mean_biomass)


###############################################################################
################ Added Productivity Data ######################################
###############################################################################
#
# For RAM v4.44 data:
surplus <- read.csv(here::here("Data/RAM Files (v4.44)/surplus production/sp.data.csv")) # the surplus production with model fit data
NA_ind <- unique(c(which(is.na(surplus$B)), which(is.na(surplus$SP))))
surplus <- surplus[-NA_ind, ]

# Importing stocklong, region, scientificname, taxGroup from other dataframes
surplus$stocklong <- 
  stock_info$stocklong[match(surplus$stockid, stock_info$stockid)]
surplus$scientificname <-
  stock_info$scientificname[match(surplus$stockid, stock_info$stockid)]
surplus$region <- 
  stock_info$region[match(surplus$stockid, stock_info$stockid)]
surplus$taxGroup <-
  taxonomy$taxGroup[match(surplus$scientificname, taxonomy$scientificname)]
surplus$taxGroup <- str_to_title(surplus$taxGroup)
surplus$taxGroup <- factor(surplus$taxGroup, levels = TB_taxGroup_list)

# Change some region values (to match other datasets):
surplus$region <- surplus$region %>%
  sub("Russia Japan", "Northwest Pacific", .) %>%
  sub("Europe non EU", "Norway, Iceland, Faroe Islands", .) %>%
  sub("European Union", "European Union (non Mediterranean)", .)

# Bringing MSY into surplus data from the bioparams_values_view file using stockid:
bioparams_values_views <- read.csv(here::here("Data/RAM Files (v4.44)/views tables/bioparams_values_views.csv"))

surplus$MSYbest <- 
  bioparams_values_views$MSYbest[match(surplus$stockid, 
                                       bioparams_values_views$stockid)]

# Creating lists of with surplus data split by region and by taxGroup:
surplus_region_list <- split(surplus, surplus$region)
surplus_taxGroup_list <- split(surplus, surplus$taxGroup)

###############################################################################
############# Dataframes for Surplus Production Plots #########################
###############################################################################

# Average surplus production and biomass for each stock in each region:
region_surplus_mean_biomass <- mean_SP_fun(surplus_region_list,
                                           region_labels,
                                           number_regions)

# Average surplus production and biomass for each stock in each taxGroup:
TB_taxGroup_surplus_mean_biomass <- mean_SP_fun(surplus_taxGroup_list,
                                                TB_taxGroup_labels,
                                                number_TB_taxGroups)

