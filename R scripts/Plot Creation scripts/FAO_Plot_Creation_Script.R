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

# Data set-up and figures 1 and 2

##################################################################################
######################### RAM Data sets ##########################################
##################################################################################

load(here::here("Data/RAM v4.44 DB Files With Model Fit Data (1-29-19)/DBdata.RData"))

# File with majority of data for all stocks is timeseries_values_views (loaded from 
# above .RData)

#**************** Temporary fix because the static timeseries_values_views df has 2 
#     stockids that are wrong - TAKE OUT ONCE THIS IS FIXED ************************
timeseries_values_views$stockid[timeseries_values_views$stockid == "CODIabdce"] <- "COD1abdce"
timeseries_values_views$stockid[timeseries_values_views$stockid == "CODIf-XIV"] <- "COD1f-XIV"
timeseries_values_views$stockid[timeseries_values_views$stockid == "NPOUTVIb"] <- "NPOUTVIa"
#**********************************************************************************

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

# Creating temporary meanC column until I get the new version of timeseries_values_views
# from Daniel with this column included
for (i in 1:nrow(timeseries_values_views)) {
  stockid_x <- timeseries_values_views$stockid[i]
  nrow_stockid_x <- nrow(timeseries_values_views[timeseries_values_views$stockid == stockid_x, ])
  timeseries_values_views$meanC[i] <- sum(timeseries_values_views$TCbest[timeseries_values_views$stockid == stockid_x],
                                          na.rm = TRUE)/nrow_stockid_x
}


for(i in 1:nrow(timeseries_values_views)) {
  if (is.na(timeseries_values_views$MSYbest[i]) == FALSE) {
    timeseries_values_views$MSY_or_meanC[i] <- timeseries_values_views$MSYbest[i]
  } else if (is.na(timeseries_values_views$MSYbest[i]) == TRUE & 
             is.na(timeseries_values_views$MSY[i]) == FALSE) {
    timeseries_values_views$MSY_or_meanC[i] <- timeseries_values_views$MSY[i]
  } else if (is.na(timeseries_values_views$MSYbest[i]) == TRUE & 
             is.na(timeseries_values_views$MSY[i]) == TRUE) {
    timeseries_values_views$MSY_or_meanC[i] <- timeseries_values_views$meanC[i]
  }
}


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


# Extracting rows that have values (not NA) in the timeseries_values_views dataframe:
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

# Subsetting data of RAM data for FAO landings plot
RAM_raw_landings <- subset(timeseries_values_views,
                       is.na(timeseries_values_views$TCbest) == FALSE & 
                         !timeseries_values_views$region %in% "Pacific Salmon")

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
all_FAO_areas <- unique(timeseries_values_views$primary_FAOname)
BdivBmsy_FAO_areas <- unique(All_BdivBmsy.df$primary_FAOname)
landings_FAO_areas <- unique(RAM_raw_landings$primary_FAOname)
filtered_FAO_areas <- landings_FAO_areas[!landings_FAO_areas %in% c("Pacific-WC-71", 
                                                          "Indian-Antarctic-58",
                                                          "inland-Asia-4",
                                                          "Pacific-Antarctic-88")]

number_FAO_areas <- length(all_FAO_areas)
number_BdivBmsy_FAO_areas <- length(BdivBmsy_FAO_areas)
number_filtered_FAO_areas <- length(filtered_FAO_areas)


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
year_min <- 1950 # this is defined in Region_or_taxGroup_Plots_Code-KM.R
year_max <- max(timeseries_values_views$year) - 1 # there were no TBbest numbers for 2017, the max year
year_range <- year_max - year_min + 1
years <- c(year_min:year_max)
stock_count_years <- seq(year_min, year_max, by = 5)

# stocks:
stock_ids <- unique(timeseries_values_views$stockid)
number_stocks <- length(stock_ids)


All_BdivBmsy.df$primary_FAOname <- factor(All_BdivBmsy.df$primary_FAOname, 
                                          levels = unique(All_BdivBmsy.df$primary_FAOname))

All_TBbest.df$primary_FAOname <- factor(All_TBbest.df$primary_FAOname, 
                                        levels = unique(All_TBbest.df$primary_FAOname))
# Separate out timeseries_values_views, All_TBbest.df and All_BdivBmsy.df data into 
# lists of dataframes separated by FAO areas 
timeseries_values_views_FAO_list <- split(timeseries_values_views, 
                                          timeseries_values_views$primary_FAOname)
All_TBbest.df_FAO_list <- split(All_TBbest.df, 
                                All_TBbest.df$primary_FAOname)
All_BdivBmsy.df_FAO_list <- split(All_BdivBmsy.df, 
                                  All_BdivBmsy.df$primary_FAOname)
RAM_raw_landings_list <- split(RAM_raw_landings, RAM_raw_landings$primary_FAOname)


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
BdivBmsy_prop_MSY_list <- vector("list", length = number_BdivBmsy_FAO_areas)
names(BdivBmsy_prop_MSY_list) <- BdivBmsy_FAO_areas

for (i in 1:number_BdivBmsy_FAO_areas) {
  BdivBmsy_prop_MSY_list[[i]] <- as.data.frame(matrix(NA, nrow = year_range,
                                                      ncol = 5))
  colnames(BdivBmsy_prop_MSY_list[[i]]) <- c("year", 
                                             "summed_MSY",
                                             "prop_stocks_0.8",
                                             "prop_0.8_stocks_1.2",
                                             "prop_stocks_1.2")
  #rownames(BdivBmsy_prop_df_list[[i]]) <- years
  for (j in 1:year_range) {
    x <- subset(All_BdivBmsy.df_FAO_list[[i]], 
                All_BdivBmsy.df_FAO_list[[i]]$year == j + 1949)
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

weighted_equal_plot <- ggplot(data = BdivBmsy_prop_stock_gathered$`Pacific-NE-67`,
                aes(x = year, y = prop_of_statuses)) +
  geom_bar(aes(fill = stock_status, alpha = prop_of_stocks),
           position = "stack", stat = "identity") +
  scale_fill_manual(name = "Stock status",
                    values = gather_colors,
                    labels = c("B/BMSY > 1.2",
                               "0.8 < B/BMSY < 1.2",
                               "B/BMSY < 0.8")) +
  scale_x_continuous(limits = c(1949, 2020), breaks = seq(1950, 2020, 10), 
                     labels = seq(1950, 2020, 10)) +
  scale_alpha_continuous(name = "", range = c(0.3, 1)) +
  guides(alpha = F) +
  guides(fill = F) +
  theme_light() +
  labs(y = expression(paste("Proportion of stocks in B/", B[M][S][Y], " category", sep = "")), 
       x = "", title = "") 


############### Plot 2 of Figure 1 (includes legend #########################

MSY_weighted_plot <- ggplot(data = BdivBmsy_prop_MSY_gathered$`Pacific-NE-67`,
                aes(x = year, y = prop_of_statuses)) +
  geom_bar(aes(color = prop_of_MSY), fill = "white",
           position = "stack", stat = "identity", size = 0) +
  scale_fill_manual(name = "Stock status",
                    values = gather_colors,
                    labels = c("B/BMSY > 1.2",
                               "0.8 < B/BMSY < 1.2",
                               "B/BMSY < 0.8")) +
  scale_color_continuous(name = expression(paste("B/", B[M][S][Y], " > 1.2", sep = "")), high = "green",
                         low = alpha("green", 0.3), labels = c("", "", "Coverage", "", ""),
                         limits = c(0,1),
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black",
                                                title.vjust = 0.1,
                                                order = 1,
                                                label.position = "top",
                                                label.theme = element_text(size = 10))) +
  # scale_color_continuous(name = "0.8 < B/BMSY < 1.2", high = "yellow",
  #                        low = alpha("yellow", 0.3), labels = c("", "", "", "", "")) +
  theme_light() +
  guides(alpha = FALSE) +
  guides(fill = FALSE) +
  #guides(color = guide_colorbar(title.vjust = 0.8)) +
  scale_x_continuous(limits = c(1949, 2020), breaks = seq(1950, 2020, 10), 
                     labels = seq(1950, 2020, 10)) +
  labs(x = "", 
       y = expression(paste("Proportion of summed MSY in B/", B[M][S][Y], "category"))) +
  theme(legend.position = "top", 
        legend.box = "vertical",
        legend.box.just = "right",
        legend.justification = c(1, 0),
        legend.margin = margin(0.01, 1, 0.01, 1, "cm"),
        legend.key.height = unit(0.75, "line"),
        legend.spacing = unit(0.05, "cm"),
        #legend.key.width = unit(1, "cm"),
        legend.title = element_text(size = 9),
        legend.key = element_rect(colour = "black", size = 4)) +
  new_scale_color() +
  geom_bar(aes(color = prop_of_MSY), fill = "white",
           position = "stack", stat = "identity", size = 0) +
  # scale_color_continuous(name = "B/BMSY > 1.2", high = "green", 
  #                        low = alpha("green", 0.3), labels = c("", "", "", "", "")) +
  scale_color_continuous(name = expression(paste("0.8 < B/", B[M][S][Y], " < 1.2")), high = "yellow",
                         low = alpha("yellow", 0.3), 
                         limits = c(0,1),
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black",
                                                title.vjust = 0.5,
                                                order = 2,
                                                label = FALSE)) +
  new_scale_color() +
  geom_bar(aes(fill = stock_status, alpha = prop_of_MSY, color = prop_of_MSY),
           position = "stack", stat = "identity", size = 0) +
  scale_color_continuous(name = expression(paste("B/", B[M][S][Y], " < 0.8")), high = "red",
                         low = alpha("red", 0.3), labels = c("0", "0.25", "0.5", "0.75", "1"),
                         limits = c(0,1),
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black",
                                                title.vjust = 0.9,
                                                order = 3)) +
  scale_alpha_continuous(name = "", range = c(0.3, 1)) +
  coord_cartesian(ylim = c(0, 1), clip = "off")


##### Putting the 2 plots together into Figure 1 and saving ##############

both_plots <- 
  ggdraw() +
  draw_plot(weighted_equal_plot, x = 0, y = 0, width = 0.5, height = 0.89) +
  draw_plot(MSY_weighted_plot, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(c("h) Stocks weighted equally", "i) Stocks weighted by MSY"), 
                  c(0, 0.5), c(0.9, 0.9), size = 10, hjust = -0.2, fontface = "plain")

ggsave(filename = "figure 1 - FAO 67.png", plot = both_plots, dpi = 600, device = "png", 
       width = 15, height = 8)

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


# Summing across years within each FAO region for the RAM landings data:
RAM_summed_landings_list <- vector("list", length = number_FAO_areas)
names(RAM_summed_landings_list) <- all_FAO_areas

for (i in 1:number_FAO_areas) {
  RAM_summed_landings_list[[i]] <- as.data.frame(matrix(NA, nrow = year_range,
                                                      ncol = 2))
  colnames(RAM_summed_landings_list[[i]]) <- c("year",
                                               "summed_TCbest")
  first_subset <- subset(RAM_raw_landings, 
                         RAM_raw_landings$primary_FAOname == all_FAO_areas[i])
  for (j in 1:year_range) {
    x <- subset(first_subset, 
                first_subset$year == j + 1949)
    RAM_summed_landings_list[[i]][j, 1] <- years[j]
    RAM_summed_landings_list[[i]][j, 2] <- sum(x$TCbest)
  }
}




# Testing with options for highlighting stocks with the most leverage on RAM (most
# leverage defined as max meanC for the moment)
max_meanCs <- sort(unique(RAM_raw_landings_list$`Pacific-NE-67`$meanC), decreasing = TRUE)[1:3]

# first version with top 3 stocks as separate geom_areas:
meanC_1_ind <- which(RAM_raw_landings_list$`Pacific-NE-67`$meanC == max_meanCs[1])[1]
meanC_2_ind <- which(RAM_raw_landings_list$`Pacific-NE-67`$meanC == max_meanCs[2])[1]
meanC_3_ind <- which(RAM_raw_landings_list$`Pacific-NE-67`$meanC == max_meanCs[3])[1]

top_3_meanC_stocklongs <- c(RAM_raw_landings_list$`Pacific-NE-67`$stocklong[meanC_1_ind],
                          RAM_raw_landings_list$`Pacific-NE-67`$stocklong[meanC_2_ind],
                          RAM_raw_landings_list$`Pacific-NE-67`$stocklong[meanC_3_ind])

stock_1_data <- RAM_raw_landings_list$`Pacific-NE-67`[RAM_raw_landings_list$`Pacific-NE-67`$stocklong == top_3_meanC_stocklongs[1],]
stock_2_data <- RAM_raw_landings_list$`Pacific-NE-67`[RAM_raw_landings_list$`Pacific-NE-67`$stocklong == top_3_meanC_stocklongs[2],]
stock_3_data <- RAM_raw_landings_list$`Pacific-NE-67`[RAM_raw_landings_list$`Pacific-NE-67`$stocklong == top_3_meanC_stocklongs[3],]

stock_1_data <- stock_1_data[, c(1:3, 5)]
stock_1_data <- stock_1_data[stock_1_data$year >= 1950, ]
stock_2_data <- stock_2_data[, c(1:3, 5)]
stock_2_data <- stock_2_data[stock_2_data$year >= 1950, ]
stock_3_data <- stock_3_data[, c(1:3, 5)]
stock_3_data <- stock_3_data[stock_3_data$year >= 1950, ]

column_3 <- paste(stock_1_data$stockid[1],"_TCbest", sep = "")
column_4 <- paste(stock_2_data$stockid[1], "_TCbest", sep = "")
column_5 <- paste(stock_3_data$stockid[1], "_TCbest", sep = "")

test_summed_landings <- RAM_summed_landings_list$`Pacific-NE-67`
test_summed_landings[, 3] <- stock_1_data$TCbest[match(test_summed_landings$year, stock_1_data$year)]
test_summed_landings[, 4] <- stock_2_data$TCbest[match(test_summed_landings$year, stock_2_data$year)]
test_summed_landings[, 5] <- stock_3_data$TCbest[match(test_summed_landings$year, stock_3_data$year)]
colnames(test_summed_landings) <- c("year", "summed_TCbest", column_3, column_4, column_5)


figure_2_plot <- ggplot() +
  geom_line(data = FAO_landings_list$`Pacific-NE-67`, 
            aes(x = Year, y = Landings/1000000, color = "FAO Database"), size = 1.5) +
  geom_line(data = test_summed_landings,
            aes(x = year, y = summed_TCbest/1000000, color = "RAM v4.44"), size = 1.5) +
  geom_area(data = test_summed_landings,
            aes(x = year, y = (WPOLLEBS_TCbest + PHAKEPCOAST_TCbest + PCODBS_TCbest)/1000000, 
                fill = "Walleye pollock \nEastern Bering Sea")) +
  geom_area(data = test_summed_landings,
            aes(x = year, y = (PHAKEPCOAST_TCbest + PCODBS_TCbest)/1000000, 
                fill = "Pacific cod \nBering Sea")) +
  geom_area(data = test_summed_landings,
            aes(x = year, y = PHAKEPCOAST_TCbest/1000000, 
                fill = "Pacific hake \nPacific Coast")) +
  scale_color_manual(name = "", 
                     values = c("FAO Database" = "red", 
                                "RAM v4.44" = "blue")) +
  scale_fill_manual(name = "RAM Stocks with \nHighest Mean Catch",
                    values = c("Walleye pollock \nEastern Bering Sea" = "dodgerblue",
                               "Pacific cod \nBering Sea" = "deepskyblue",
                               "Pacific hake \nPacific Coast" = "slategray1")) +
  scale_x_continuous(limits = c(1949, 2020), breaks = seq(1950, 2020, 10), 
                     labels = seq(1950, 2020, 10)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_classic() +
  labs(x = "", y = "Summed catch or landings (MMT)", title = "f)") +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2))

ggsave(filename = "figure 2 - FAO 67.png", plot = figure_2_plot, dpi = 600, device = "png", 
       width = 7, height = 7)

# Version of png with specific size (75 mm X 75 mm):
ggsave(filename = "figure 2 v2 - FAO 67.png", plot = figure_2_plot, dpi = 1200, device = "png", 
       width = 75, height = 75, units = "mm", scale = 2)
# didn't insert correctly into Word - figure out why that is



