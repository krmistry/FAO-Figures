## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 6/28/2019

library(plyr)
library(dplyr)
library(stringr)
library(here)
library(readxl)
library(ggnewscale)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

# Data set-up and figures 1 and 2


#--------------------------------------------------------------------------------
# Function to find number of unique elements in a column of a dataframe
count_unique_elements <- function(data, column_name) {
  column_number <- match(column_name, names(data))
  length(unique(data[, column_number]))
}


##################################################################################
######################### RAM Data sets ##########################################
##################################################################################

load(here::here("Data/RAM v4.46 (7-10-19)/DB Files With Model Fit Data/R Data/DBdata.RData"))

# File that contains stockid mapped to region and scientificname:
stock_info <- read.csv(here::here("Data/updated_stock (6-14-19).csv"))

# FAO codes
FAO_codes <- read.csv(here::here("Data/updated_stock (6-14-19) FAO codes.csv"))


# Adding region and primary_FAOname variables to timeseries_values_views dataframe:
timeseries_values_views$region <-
  stock_info$region[match(timeseries_values_views$stockid, stock_info$stockid)]
timeseries_values_views$primary_FAOname <-
  stock_info$primary_FAOname[match(timeseries_values_views$stockid, stock_info$stockid)]
timeseries_values_views$MSYbest <-
  bioparams_values_views$MSYbest[match(timeseries_values_views$stockid, bioparams_values_views$stockid)]
timeseries_values_views$MSY <-
  bioparams_values_views$MSY[match(timeseries_values_views$stockid, bioparams_values_views$stockid)]

# Adding scientificname in order to match in taxGroup and FisheryType from taxonomy 
# dataframe, also loaded during .RData import above:
timeseries_values_views$scientificname <-
  stock_info$scientificname[match(timeseries_values_views$stockid, stock_info$stockid)]

# Matching in taxGroup and FisheryType from taxonomy dataframe:
timeseries_values_views$FisheryType <-
  taxonomy$FisheryType[match(timeseries_values_views$scientificname, taxonomy$scientificname)]
timeseries_values_views$taxGroup <-
  taxonomy$taxGroup[match(timeseries_values_views$scientificname, taxonomy$scientificname)]

# Creating meanC column with TCbest or TL/CdivmeanC:
timeseries_values_views$meanC <- timeseries_values_views$TCbest/timeseries_values_views$CdivMEANC

# Checking to see if any stocks have TCbest values but not CdivMEANC
CdivMEANC_NA_stocks <- unique(timeseries_values_views$stockid[which(!is.na(timeseries_values_views$CdivMEANC))])
TCbest_NA_stocks <- unique(timeseries_values_views$stockid[which(!is.na(timeseries_values_views$TCbest))])

setdiff(TCbest_NA_stocks, CdivMEANC_NA_stocks)

# Populate MSY_or_meanC column with MSYbest first, then MSY (if NA), then meanC (if 
# still NA):
timeseries_values_views$MSY_or_meanC <- timeseries_values_views$MSYbest
timeseries_values_views$MSY_or_meanC[which(is.na(timeseries_values_views$MSY_or_meanC))] <- timeseries_values_views$MSY[which(is.na(timeseries_values_views$MSY_or_meanC))]
timeseries_values_views$MSY_or_meanC[which(is.na(timeseries_values_views$MSY_or_meanC))] <- timeseries_values_views$meanC[which(is.na(timeseries_values_views$MSY_or_meanC))]


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
# All_TBbest.df <- subset(timeseries_values_views, 
#                         is.na(timeseries_values_views$TBbest) == FALSE)


# Extracting rows that have values (not NA) in the BdivBmsy column of the 
# timeseries_values_views dataframe:
All_BdivBmsy.df <- subset(timeseries_values_views,
                          is.na(timeseries_values_views$BdivBmsypref) == FALSE)

# Adding column with B/Bmsy categories for B/Bmsy plot:
BdivBmsy_categories <- c("B/BMSY < 0.8", 
                         "0.8 < B/BMSY < 1.2", 
                         "B/BMSY > 1.2")

for (i in 1:nrow(All_BdivBmsy.df)) {
  if (All_BdivBmsy.df$BdivBmsypref[i] < 0.8) {
    All_BdivBmsy.df$BdivBmsy_category[i] <- BdivBmsy_categories[1]
  } 
  if (All_BdivBmsy.df$BdivBmsypref[i] >= 0.8 & All_BdivBmsy.df$BdivBmsypref[i] < 1.2) {
    All_BdivBmsy.df$BdivBmsy_category[i] <- BdivBmsy_categories[2]
  } 
  if (All_BdivBmsy.df$BdivBmsypref[i] >= 1.2) {
    All_BdivBmsy.df$BdivBmsy_category[i] <- BdivBmsy_categories[3]
  }
}

any(is.na(All_BdivBmsy.df$BdivBmsy_category))

# BdivBmsy data to use in plot 2 of figure 1, with no NAs in MSY_or_meanC column:
BdivBmsy_with_MSY_data <- All_BdivBmsy.df[!is.na(All_BdivBmsy.df$MSY_or_meanC), ]

# Subsetting data of RAM data for FAO landings plot
RAM_raw_landings <- subset(timeseries_values_views,
                       is.na(timeseries_values_views$TCbest) == FALSE)

RAM_raw_landings$primary_FAOname <- as.character(RAM_raw_landings$primary_FAOname)
RAM_raw_landings$primary_FAOname[RAM_raw_landings$region == "Pacific Ocean" & 
                                   RAM_raw_landings$taxGroup == "Tuna-Billfish"] <- "Pacific Ocean tunas"
RAM_raw_landings$primary_FAOname[RAM_raw_landings$region == "Atlantic Ocean" & 
                                   RAM_raw_landings$taxGroup == "Tuna-Billfish"] <- "Atlantic Ocean tunas"
RAM_raw_landings$primary_FAOname[RAM_raw_landings$region == "Indian Ocean" & 
                                   RAM_raw_landings$taxGroup == "Tuna-Billfish"] <- "Indian Ocean tunas"
RAM_raw_landings$primary_FAOname <- as.factor(RAM_raw_landings$primary_FAOname)


##################################################################################
######### Parameters for All Data Transformations & Segmentations ################
#################################################################################

# if there are NAs in either region or taxGroup, stop operation
# if (any(is.na(timeseries_values_views$region) == TRUE) | 
#     any(is.na(timeseries_values_views$primary_FAOname) == TRUE)) { 
#   stop(paste("timeseries_values_views has NAs in region and/or primary_FAOname column"))
# }

# FAO area lists:
all_FAO_areas <- sort(as.character(unique(timeseries_values_views$primary_FAOname)))
BdivBmsy_FAO_areas <- sort(as.character(unique(All_BdivBmsy.df$primary_FAOname)))
landings_FAO_areas <- sort(as.character(unique(RAM_raw_landings$primary_FAOname)))
filtered_FAO_areas <- landings_FAO_areas[!landings_FAO_areas %in% c("Pacific-WC-71", 
                                                          "Indian-Antarctic-58",
                                                          "inland-Asia-4",
                                                          "Pacific-Antarctic-88")]

number_FAO_areas <- length(all_FAO_areas)
number_BdivBmsy_FAO_areas <- length(BdivBmsy_FAO_areas)
number_filtered_FAO_areas <- length(filtered_FAO_areas)


# Region lists:
# regions <- unique(timeseries_values_views$region)
# salmon_reg <- c("Canada West Coast (Pacific Salmon)", 
#                 "US Alaska (Pacific Salmon)", 
#                 "Northwest Pacific (Pacific Salmon)", 
#                 "US West Coast (Pacific Salmon)") 
# # use regions to match with timeseries_values_views$region, All_TBbest.df$region 
# # and surplus$region
# regions <- regions[regions != salmon_reg] 
# regions_plot_titles <- regions
# number_regions <- length(regions)
# # use region_labels for naming region dataframes inside lists
# region_labels <- gsub(" ", "_", regions)
# 
# # Taxonomy group lists for timeseries_values_views data:
# taxGroup_list <- unique(timeseries_values_views$taxGroup)
# number_taxGroups <- length(taxGroup_list)
# taxGroup_plot_titles <- taxGroup_list
# taxGroup_labels <- gsub(" |-", "_", taxGroup_list)
# # Taxonomy group lists for All_TBbest.df data:
# TB_taxGroup_list <- as.character(taxGroup_list[taxGroup_list %in% unique(All_TBbest.df$taxGroup)])
# number_TB_taxGroups <- length(TB_taxGroup_list)
# TB_taxGroup_plot_titles <- TB_taxGroup_list
# TB_taxGroup_labels <- gsub(" |-", "_", TB_taxGroup_list)
# ##### use TB_taxGroup_list for matching with All_TBbest.df$taxGroup ###########

# year range:
year_min <- 1950 # this is defined in Region_or_taxGroup_Plots_Code-KM.R
year_max <- max(timeseries_values_views$year) - 1 # there were no TBbest numbers for 2017, the max year
year_range <- year_max - year_min + 1
years <- c(year_min:year_max)
# stock_count_years <- seq(year_min, year_max, by = 5)
# 
# # stocks:
# stock_ids <- unique(timeseries_values_views$stockid)
# number_stocks <- length(stock_ids)


All_BdivBmsy.df$primary_FAOname <- factor(All_BdivBmsy.df$primary_FAOname, 
                                          levels = unique(All_BdivBmsy.df$primary_FAOname))
BdivBmsy_with_MSY_data$primary_FAOname <- factor(BdivBmsy_with_MSY_data$primary_FAOname,
                                                 levels = unique(BdivBmsy_with_MSY_data$primary_FAOname))
# All_TBbest.df$primary_FAOname <- factor(All_TBbest.df$primary_FAOname, 
#                                         levels = unique(All_TBbest.df$primary_FAOname))
# # Separate out timeseries_values_views, All_TBbest.df, All_BdivBmsy.df and BdivBmsy_with_MSY 
# data into lists of dataframes separated by FAO areas, and sorting dataframes alphabetically
timeseries_values_views_FAO_list <- split(timeseries_values_views, 
                                          timeseries_values_views$primary_FAOname)
# All_TBbest.df_FAO_list <- split(All_TBbest.df, 
#                                 All_TBbest.df$primary_FAOname)
# All_TBbest.df_FAO_list <- All_TBbest.df_FAO_list[sort(names(All_TBbest.df_FAO_list))]

All_BdivBmsy.df_FAO_list <- split(All_BdivBmsy.df, 
                                  All_BdivBmsy.df$primary_FAOname)
All_BdivBmsy.df_FAO_list <- All_BdivBmsy.df_FAO_list[sort(names(All_BdivBmsy.df_FAO_list))]

BdivBmsy_with_MSY_data_FAO_list <- split(BdivBmsy_with_MSY_data,
                                         BdivBmsy_with_MSY_data$primary_FAOname)
BdivBmsy_with_MSY_data_FAO_list <- BdivBmsy_with_MSY_data_FAO_list[sort(names(BdivBmsy_with_MSY_data_FAO_list))]

RAM_raw_landings_list <- split(RAM_raw_landings, RAM_raw_landings$primary_FAOname)
RAM_summed_landings_list <- RAM_raw_landings_list[sort(names(RAM_raw_landings_list))]

################################################################################
############  BdivBmsy & Number of Stocks across Timeseries ####################
################################################################################


# Stock proportions weighted based on number of stocks
BdivBmsy_prop_df_list <- vector("list", length = number_BdivBmsy_FAO_areas)
names(BdivBmsy_prop_df_list) <- BdivBmsy_FAO_areas

for (i in 1:number_BdivBmsy_FAO_areas) {
  BdivBmsy_prop_df_list[[i]] <- as.data.frame(matrix(NA, nrow = year_range,
                                                     ncol = 5))
  colnames(BdivBmsy_prop_df_list[[i]]) <- c("year", 
                                            "number_stocks",
                                            "prop_stocks_0.8",
                                            "prop_0.8_stocks_1.2",
                                            "prop_stocks_1.2")
  #rownames(BdivBmsy_prop_df_list[[i]]) <- years
  for (j in 1:year_range) {
    x <- subset(All_BdivBmsy.df_FAO_list[[i]], 
                All_BdivBmsy.df_FAO_list[[i]]$year == j + 1949)
    BdivBmsy_prop_df_list[[i]][j, 1] <- years[j]
    BdivBmsy_prop_df_list[[i]][j, 2] <- count_unique_elements(x,
                                                              "stockid")
    BdivBmsy_prop_df_list[[i]][j, 3] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[1]))/BdivBmsy_prop_df_list[[i]][j, 2]
    BdivBmsy_prop_df_list[[i]][j, 4] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[2]))/BdivBmsy_prop_df_list[[i]][j, 2]
    BdivBmsy_prop_df_list[[i]][j, 5] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[3]))/BdivBmsy_prop_df_list[[i]][j, 2]
  }
}

for (j in 1:number_BdivBmsy_FAO_areas) {
  for (i in 1:nrow(BdivBmsy_prop_df_list[[j]])) {
    BdivBmsy_prop_df_list[[j]]$prop_of_stocks[i] <- BdivBmsy_prop_df_list[[j]]$number_stocks[i]/max(BdivBmsy_prop_df_list[[j]]$number_stocks)
  }
}


# Stocks weighted by MSY (or meanC if MSY not available)
BdivBmsy_prop_MSY_list <- vector("list", length = length(unique(BdivBmsy_with_MSY_data$primary_FAOname)))
names(BdivBmsy_prop_MSY_list) <- sort(unique(as.character(BdivBmsy_with_MSY_data$primary_FAOname)))

for (i in 1:length(unique(BdivBmsy_with_MSY_data$primary_FAOname))) {
  BdivBmsy_prop_MSY_list[[i]] <- as.data.frame(matrix(NA, nrow = year_range,
                                                      ncol = 5))
  colnames(BdivBmsy_prop_MSY_list[[i]]) <- c("year", 
                                             "summed_MSY",
                                             "prop_stocks_0.8",
                                             "prop_0.8_stocks_1.2",
                                             "prop_stocks_1.2")
  #rownames(BdivBmsy_prop_df_list[[i]]) <- years
  for (j in 1:year_range) {
    x <- subset(BdivBmsy_with_MSY_data_FAO_list[[i]], 
                BdivBmsy_with_MSY_data_FAO_list[[i]]$year == j + 1949)
    x_under_0.8 <- filter(x,
                          x$BdivBmsy_category == BdivBmsy_categories[1])
    x_0.8_to_1.2 <- filter(x,
                           x$BdivBmsy_category == BdivBmsy_categories[2])
    x_over_1.2 <- filter(x,
                         x$BdivBmsy_category == BdivBmsy_categories[3])
    BdivBmsy_prop_MSY_list[[i]][j, 1] <- years[j]
    BdivBmsy_prop_MSY_list[[i]][j, 2] <- sum(x$MSY_or_meanC)
    BdivBmsy_prop_MSY_list[[i]][j, 3] <- sum(x_under_0.8$MSY_or_meanC)/BdivBmsy_prop_MSY_list[[i]][j, 2] 
    BdivBmsy_prop_MSY_list[[i]][j, 4] <- sum(x_0.8_to_1.2$MSY_or_meanC)/BdivBmsy_prop_MSY_list[[i]][j, 2]
    BdivBmsy_prop_MSY_list[[i]][j, 5] <- sum(x_over_1.2$MSY_or_meanC)/BdivBmsy_prop_MSY_list[[i]][j, 2]
  }
}

for (j in 1:number_BdivBmsy_FAO_areas) {
  for (i in 1:nrow(BdivBmsy_prop_df_list[[j]])) {
    BdivBmsy_prop_MSY_list[[j]]$prop_of_MSY[i] <- BdivBmsy_prop_MSY_list[[j]]$summed_MSY[i]/max(BdivBmsy_prop_MSY_list[[j]]$summed_MSY)
  }
}



BdivBmsy_prop_stock_gathered <- lapply(BdivBmsy_prop_df_list, function(x) {
  df <- gather(x[, -2], 
               key = stock_status, 
               value = prop_of_statuses, 
               prop_stocks_0.8:prop_stocks_1.2)
  df$stock_status <- factor(df$stock_status, 
                            levels = rev(c("prop_stocks_0.8",
                                           "prop_0.8_stocks_1.2",
                                           "prop_stocks_1.2")))
  return(df)
})


BdivBmsy_prop_MSY_gathered <- lapply(BdivBmsy_prop_MSY_list, function(x) {
  df <- gather(x[, -2], 
               key = stock_status, 
               value = prop_of_statuses, 
               prop_stocks_0.8:prop_stocks_1.2)
  df$stock_status <- factor(df$stock_status, 
                            levels = rev(c("prop_stocks_0.8",
                                           "prop_0.8_stocks_1.2",
                                           "prop_stocks_1.2")))
  return(df)
})


# gather_test$stock_status <- factor(gather_test$stock_status, 
#                                    levels = rev(c("prop_stocks_0.8",
#                                               "prop_0.8_stocks_1.2",
#                                               "prop_stocks_1.2")))
gather_colors <- c("red", "yellow", "green")
names(gather_colors) <- c("prop_stocks_0.8",
                          "prop_0.8_stocks_1.2",
                          "prop_stocks_1.2")

################ Plot 1 of Figure 1 (no legend) ########################################
figure_1_function <- function(stock_gathered_data, 
                              MSY_gathered_data, 
                              FAO_area) {
  # Create plot 1:
  weighted_equal_plot <- ggplot(data = stock_gathered_data,
                                aes(x = year, y = prop_of_statuses)) +
    geom_bar(aes(fill = stock_status, alpha = prop_of_stocks),
             position = "stack", stat = "identity") +
    scale_fill_manual(name = "Stock status",
                      values = gather_colors,
                      labels = c("B/BMSY > 1.2",
                                 "0.8 < B/BMSY < 1.2",
                                 "B/BMSY < 0.8")) +
    scale_x_continuous(limits = c(1949, 2018), breaks = seq(1950, 2010, 20), 
                       labels = seq(1950, 2010, 20), expand = c(0, 0)) +
    scale_alpha_continuous(name = "", range = c(0.1, 1)) +
    guides(alpha = F) +
    guides(fill = F) +
    theme_light() +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10)) +
    labs(y = expression(paste("Proportion of stocks in B/", B[M][S][Y], " category", sep = "")), 
         x = "", title = "") 
  
  # Create plot 2
  MSY_weighted_plot <- ggplot(data = MSY_gathered_data,
                              aes(x = year, y = prop_of_statuses)) +
    geom_bar(aes(color = prop_of_MSY), fill = "white",
             position = "stack", stat = "identity", size = 0) +
    scale_fill_manual(name = "Stock status",
                      values = gather_colors,
                      labels = c("B/BMSY > 1.2",
                                 "0.8 < B/BMSY < 1.2",
                                 "B/BMSY < 0.8")) +
    scale_color_continuous(name = expression(paste("B/", B[M][S][Y], " > 1.2", sep = "")), high = "green",
                           low = alpha("green", 0.1), labels = c("", "", "Coverage", "", ""),
                           limits = c(0,1),
                           guide = guide_colorbar(frame.colour = "black", 
                                                  ticks.colour = "black",
                                                  title.vjust = 0.1,
                                                  order = 1,
                                                  label.position = "top",
                                                  label.theme = element_text(size = 10))) +
    theme_light() +
    guides(alpha = FALSE) +
    guides(fill = FALSE) +
    scale_x_continuous(limits = c(1949, 2018), breaks = seq(1950, 2010, 20), 
                       labels = seq(1950, 2010, 20), expand = c(0, 0)) +
    labs(x = "", 
         y = expression(paste("Proportion of summed MSY in B/", B[M][S][Y], " category", sep = ""))) +
    theme(legend.position = "top", 
          legend.box = "vertical",
          legend.box.just = "right",
          legend.justification = c(1, 0),
          legend.margin = margin(0.01, 0.01, 0.01, 0.01, "cm"),
          legend.key.height = unit(0.75, "line"),
          legend.key.width = unit(0.5, "cm"),
          legend.spacing = unit(0.05, "cm"),
          legend.title = element_text(size = 10),
          legend.key = element_rect(colour = "black", size = 4)) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10)) +
    new_scale_color() +
    geom_bar(aes(color = prop_of_MSY), fill = "white",
             position = "stack", stat = "identity", size = 0) +
    scale_color_continuous(name = expression(paste("0.8 < B/", B[M][S][Y], " < 1.2")), high = "yellow",
                           low = alpha("yellow", 0.1), 
                           limits = c(0, 1),
                           guide = guide_colorbar(frame.colour = "black", 
                                                  ticks.colour = "black",
                                                  title.vjust = 0.5,
                                                  order = 2,
                                                  label = FALSE)) +
    new_scale_color() +
    geom_bar(aes(fill = stock_status, alpha = prop_of_MSY, color = prop_of_MSY),
             position = "stack", stat = "identity", size = 0) +
    scale_color_continuous(name = expression(paste("B/", B[M][S][Y], " < 0.8")), high = "red",
                           low = alpha("red", 0.1), labels = c("0", "", "0.5", "", "1"),
                           limits = c(0,1),
                           guide = guide_colorbar(frame.colour = "black", 
                                                  ticks.colour = "black",
                                                  title.vjust = 0.9,
                                                  order = 3)) +
    scale_alpha_continuous(name = "", range = c(0.1, 1)) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
  
  # Add plots together into 1 figure
  both_plots <- 
    ggdraw() +
    draw_plot(weighted_equal_plot, x = 0, y = 0, width = 0.5, height = 0.78) +
    draw_plot(MSY_weighted_plot, x = 0.5, y = 0, width = 0.5, height = 1) +
    draw_plot_label(c("h", "i"), x = c(0.07, 0.57), y = c(1, 1), size = 10, 
                    hjust = -0.1, fontface = "bold")
  
  # Save figure
  ggsave(filename = paste(FAO_area, "_figure_1.png", sep = ""), path = here("Figures/Figure 1"),
         plot = both_plots, dpi = 600, device = "png", width = 14, height = 7.5, units = "in", 
         scale = 0.5)
}

for (i in 1:number_BdivBmsy_FAO_areas) {
  figure_1_function(stock_gathered_data = BdivBmsy_prop_stock_gathered[[i]], 
                    MSY_gathered_data = BdivBmsy_prop_MSY_gathered[[i]], 
                    FAO_area = BdivBmsy_FAO_areas[i])
}



#####################################################################################
# Data set up for Figure 2 (FAO and RAM total landings, 1950 - 2017)

FAO_landings <- read.csv(here::here("/Data/FAO landings by FAOarea merged tab.csv"))
FAO_landings_gathered <- gather(FAO_landings, 
                                key = "Year", 
                                value = "Landings", 
                                X1950:X2017)

# Take "X" off the year and make it numeric
FAO_landings_gathered$Year <- as.numeric(gsub("X", "", FAO_landings_gathered$Year))

# Delete rows with NA Landings values
FAO_landings_gathered <- FAO_landings_gathered[-which(is.na(FAO_landings_gathered$Landings)), ]

FAO_landings_list <- split(FAO_landings_gathered, 
                           FAO_landings_gathered$reg_FAOreport)
FAO_landings_list <- FAO_landings_list[sort(names(FAO_landings_list))]


# Summing across years within each FAO region for the RAM landings data:
RAM_summed_landings_list <- vector("list", length = number_FAO_areas)
names(RAM_summed_landings_list) <- landings_FAO_areas

for (i in 1:length(landings_FAO_areas)) {
  RAM_summed_landings_list[[i]] <- as.data.frame(matrix(NA, nrow = year_range,
                                                      ncol = 2))
  colnames(RAM_summed_landings_list[[i]]) <- c("year",
                                               "summed_TCbest")
  first_subset <- subset(RAM_raw_landings, 
                         RAM_raw_landings$primary_FAOname == as.character(landings_FAO_areas)[i])
  for (j in 1:year_range) {
    x <- subset(first_subset, 
                first_subset$year == j + 1949)
    RAM_summed_landings_list[[i]][j, 1] <- years[j]
    RAM_summed_landings_list[[i]][j, 2] <- sum(x$TCbest)
  }
}




# Extract 
all_meanCs <- vector("list", length = number_FAO_areas)
names(all_meanCs) <- landings_FAO_areas

for (j in 1:length(landings_FAO_areas)) {
  x <- RAM_raw_landings_list[[j]]
  stocks <- unique(x$stockid)
  all_meanCs[[j]] <- as.data.frame(matrix(NA, nrow = length(stocks), ncol = 3))
  colnames(all_meanCs[[j]]) <- c("stockid", "meanC", "stocklong")
  for (i in 1:length(stocks)) {
    all_meanCs[[j]][i, 1] <- stocks[i]
    all_meanCs[[j]][i, 2] <- x$meanC[x$stockid == stocks[i]][1]
    all_meanCs[[j]][i, 3] <- x$stocklong[x$stockid == stocks[i]][1]
    all_meanCs[[j]] <- all_meanCs[[j]][order(all_meanCs[[j]]$meanC, decreasing = TRUE), ]
    all_meanCs[[j]] <- all_meanCs[[j]][1:3, ]
    all_meanCs[[j]]$stocklong <- sub("\\s+$", "", gsub('(.{1,16})(\\s|$)', '\\1\n', all_meanCs[[j]]$stocklong))
  }
}

# Check how many regions have less than 2 stocks:
small_stocks_list <- vector()

for (i in 1:length(landings_FAO_areas)) {
  small_stocks_list[i] <- length(which(!is.na(all_meanCs[[i]]$meanC)))
}
small_stock_ind <- which(small_stocks_list < 3)

for (j in 1:length(landings_FAO_areas)) {
    stock_1_data <- RAM_raw_landings_list[[j]][RAM_raw_landings_list[[j]]$stockid == all_meanCs[[j]]$stockid[1], c(1:3, 5)]
    stock_2_data <- RAM_raw_landings_list[[j]][RAM_raw_landings_list[[j]]$stockid == all_meanCs[[j]]$stockid[2], c(1:3, 5)]
    stock_3_data <- RAM_raw_landings_list[[j]][RAM_raw_landings_list[[j]]$stockid == all_meanCs[[j]]$stockid[3], c(1:3, 5)]
    column_3 <- paste(stock_1_data$stockid[1],"_TCbest", sep = "")
    column_4 <- paste(stock_2_data$stockid[1], "_TCbest", sep = "")
    column_5 <- paste(stock_3_data$stockid[1], "_TCbest", sep = "")
    RAM_summed_landings_list[[j]][, 3] <- stock_1_data$TCbest[match(RAM_summed_landings_list[[j]]$year, stock_1_data$year)]
    RAM_summed_landings_list[[j]][, 4] <- stock_2_data$TCbest[match(RAM_summed_landings_list[[j]]$year, stock_2_data$year)]
    RAM_summed_landings_list[[j]][, 5] <- stock_3_data$TCbest[match(RAM_summed_landings_list[[j]]$year, stock_3_data$year)]
    colnames(RAM_summed_landings_list[[j]]) <- c("year", 
                                                 "summed_TCbest", 
                                                 column_3, 
                                                 column_4, 
                                                 column_5)
}


sub("\\s+$", "", gsub('(.{1,16})(\\s|$)', '\\1\n', all_meanCs[[1]]$stocklong))



figure_2_function <- function(FAO_data, 
                              RAM_data, 
                              FAO_area,
                              meanC_stocks) {
  stock_1_name <- meanC_stocks$stocklong[1]
  stock_2_name <- meanC_stocks$stocklong[2]
  stock_3_name <- meanC_stocks$stocklong[3]
  # print(stock_1_name)
  # print(stock_2_name)
  # print(stock_3_name)
  stockid_1 <- as.symbol(colnames(RAM_data)[3])
  stockid_2 <- as.symbol(colnames(RAM_data)[4])
  stockid_3 <- as.symbol(colnames(RAM_data)[5])
  # stockid_1 <- colnames(RAM_data)[3]
  # stockid_2 <- colnames(RAM_data)[4]
  # stockid_3 <- colnames(RAM_data)[5]
  # RAM_data$stock_1_column <- rep(stock_1_name, nrow(RAM_data))
  # print(RAM_data$stock_1_column)
  # 
  figure_2_plot <- ggplot() +
    geom_line(data = FAO_data, 
              aes(x = Year, y = Landings/1000000, color = "FAO Database"), size = 1.5) +
    geom_line(data = RAM_data,
              aes(x = year, y = summed_TCbest/1000000, color = "RAM v4.44"), size = 1.5) +
    geom_area(data = RAM_data,
              aes(x = year, y = (!!stockid_1 + 
                                   !!stockid_2 + 
                                   !!stockid_3)/1000000, 
                  fill = stock_1_name)) +
    geom_area(data = RAM_data,
              aes(x = year, y = (!!stockid_1 +
                                   !!stockid_2)/1000000,
                  fill = quo(stock_2_name))) +
    geom_area(data = RAM_data,
              aes(x = year, y = !!stockid_1/1000000,
                  fill = quo(stock_3_name))) +
    scale_color_manual(name = "", 
                       values = c("FAO Database" = "red", 
                                  "RAM v4.44" = "blue")) +
    scale_fill_manual(name = "RAM Stocks with \nHighest Mean Catch",
                      values = c(stock_1_name = "dodgerblue",
                                 stock_2_name = "deepskyblue",
                                 stock_3_name = "slategray1")) +
    scale_x_continuous(limits = c(1949, 2018), breaks = seq(1950, 2010, 20), 
                       labels = seq(1950, 2010, 20)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_classic(base_size = 10) +
    labs(x = "", y = "Summed catch or landings (MMT)", title = "f") +
    theme(legend.position = "top",
          legend.box = "vertical",
          legend.box.just = "right",
          legend.justification = c(1, 0),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2))
  return(figure_2_plot)
  # ggsave(filename = paste(FAO_area, "-figure_2.png", sep = ""), plot = figure_2_plot, 
  #        dpi = 600, device = "png", width = 7, height = 7, path = here("Figures/Figure 2"))
  
}


figure_2_function(FAO_data = FAO_landings_list[[2]],
                  RAM_data = RAM_summed_landings_list[[1]],
                  FAO_area = landings_FAO_areas[1],
                  meanC_stocks = all_meanCs[[1]])





