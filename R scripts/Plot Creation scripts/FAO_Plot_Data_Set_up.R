## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 6/19/2019

# This script is designed to be run as part of
# Region_or_taxGroup_Plots_Code-KM.R and it produces the transformed and
# filtered datasets based on RAM data used to make the region and taxGroup plots
# described in Region_or_taxGroup_Plots_Code-KM.R

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

load(here::here("Data/RAM v4.44 DB Files With Model Fit Data (1-29-19)/DBdata.RData"))

# File with majority of data for all stocks is timeseries_values_views (loaded from 
# above .RData)

#**************** Temporary fix because the static timeseries_values_views df has 2 
#     stockids that are wrong - TAKE OUT ONCE THIS IS FIXED ************************
timeseries_values_views$stockid[timeseries_values_views$stockid == "CODIabdce"] <- "COD1abdce"
timeseries_values_views$stockid[timeseries_values_views$stockid == "CODIf-XIV"] <- "COD1f-XIV"
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
number_FAO_areas <- length(all_FAO_areas)
number_BdivBmsy_FAO_areas <- length(BdivBmsy_FAO_areas)

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



################################################################################
########################### Summary Dataframes  ################################
################################################################################

# Number of stocks, taxonomy groups and first & last year with data in each 
# region, used to produce summary tables at top of region pages:
# stock_tax_per_FAO_area <- summary_fun(type_of_plot = "region", 
#                                     input_data = timeseries_values_views_region_list, 
#                                     number_taxGroup_or_region = number_FAO_areas, 
#                                     row_names = all_FAO_areas)


# Version of the above with the All_BdivBmsy.df; this will tell what to expect 
# in the biomass coverage plots
BdivBmsy_stock_tax_per_FAO_area <- summary_fun(input_data = All_BdivBmsy.df_FAO_list, 
                                               number_of_variables = number_BdivBmsy_FAO_areas, 
                                               row_names = BdivBmsy_FAO_areas)

# Version of the above with the All_TBbest.df; this will tell what to expect 
# in the biomass coverage plots
TB_stock_tax_per_FAO_area <- summary_fun(input_data = All_TBbest.df_FAO_list, 
                                         number_of_variables = length(unique(All_TBbest.df$primary_FAOname)), 
                                         row_names = unique(All_TBbest.df$primary_FAOname))


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
    number_stocks <- length(unique(x$stockid))
    BdivBmsy_prop_MSY_list[[i]][j, 1] <- years[j]
    BdivBmsy_prop_MSY_list[[i]][j, 2] <- sum(x$MSY_or_meanC)
    BdivBmsy_prop_MSY_list[[i]][j, 3] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[1]))/number_stocks
    BdivBmsy_prop_MSY_list[[i]][j, 4] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[2]))/number_stocks
    BdivBmsy_prop_MSY_list[[i]][j, 5] <- nrow(filter(x,
                                                    x$BdivBmsy_category == BdivBmsy_categories[3]))/number_stocks
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

################ Plot 1 of Figure 1 ########################################

plot1 <- ggplot(data = BdivBmsy_prop_stock_gathered$`Atlantic-NW-21`,
                aes(x = year, y = prop_of_statuses)) +
  geom_bar(aes(color = prop_of_stocks), fill = "white", 
           position = "stack", stat = "identity", size = 0) +
  scale_fill_manual(name = "Stock status",
                    values = gather_colors,
                    labels = c("B/BMSY > 1.2",
                               "0.8 < B/BMSY < 1.2",
                               "B/BMSY < 0.8")) +
  scale_color_continuous(name = "B/BMSY > 1.2", high = "green", 
                         low = alpha("green", 0.3), labels = c("", "", "", "", "")) +
  scale_x_continuous(limits = c(1949, 2020), breaks = seq(1950, 2020, 10), 
                     labels = seq(1950, 2020, 10)) +
  guides(alpha = F) +
  guides(fill = F) +
  #guides(color = guide_colorbar(title.position = "right")) +
  theme_light() +
  labs(y = "", x = "", 
       caption = "Stocks weighted equally", title = "") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10, face = "bold")) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "top", 
        legend.box = "vertical",
        legend.box.just = "right",
        legend.justification = c(1, 0),
        legend.margin = margin(0.001, 1, 0.001, 1, "cm"),
        legend.key.height = unit(0.75, "line"),
        legend.spacing = unit(0.05, "cm"),
        #legend.key.width = unit(1, "cm"),
        legend.title = element_text(size = 9)) +
  new_scale_color() +
  geom_bar(aes(color = prop_of_stocks), fill = "white",
           position = "stack", stat = "identity", size = 0) +
  scale_color_continuous(name = "0.8 < B/BMSY < 1.2", high = "yellow",
                         low = alpha("yellow", 0.3), labels = c("", "", "", "", "")) +
  #guides(color = guide_colorbar(title.position = "right")) +
  new_scale_color() +
  geom_bar(aes(fill = stock_status, alpha = prop_of_stocks, color = prop_of_stocks),
           position = "stack", stat = "identity", size = 0) +
  scale_color_continuous(name = "B/BMSY < 0.8", high = "red",
                         low = alpha("red", 0.3), labels = c("", "", "", "", "")) +
  scale_alpha_continuous(name = "", range = c(0.3, 1)) +
  annotate("text", x = 2012, y = 1.3, label = "Coverage") +
  annotate("text", x = 2012, y = 1.07, label = "    0.25  0.5  0.75   1", size = 3.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") 
#guides(color = guide_colorbar(title.position = "right")) +
#theme(legend.margin = margin(0.001, 1, 0.001, 1, "cm")) +
#theme(legend.key.height = unit(0.75, "line")) +
#theme(legend.title = element_text(size = 9))

#guides(colour = guide_colorbar(title.vjust = 0.5))


############### Plot 2 of Figure 1 (stockes weighed by MSY) #########################

plot2 <- ggplot(data = BdivBmsy_prop_MSY_gathered$`Atlantic-NW-21`,
                aes(x = year, y = prop_of_statuses)) +
  geom_bar(aes(fill = stock_status, alpha = prop_of_MSY),
           position = "stack", stat = "identity") +
  scale_fill_manual(name = "Stock status",
                    values = gather_colors,
                    labels = c("B/BMSY > 1.2",
                               "0.8 < B/BMSY < 1.2",
                               "B/BMSY < 0.8")) +
  theme_light() +
  guides(alpha = FALSE) +
  guides(fill = FALSE) +
  scale_x_continuous(limits = c(1949, 2020), breaks = seq(1950, 2020, 10), 
                     labels = seq(1950, 2020, 10)) +
  scale_alpha_continuous(name = "", range = c(0.3, 1)) +
  labs(caption = "Stocks weighted by MSY", x = "", 
       y = "Proportion of stocks in B/BMSY category") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10, face = "bold")) 


##### Putting the 2 plots together into Figure 1 ##############

grid.arrange(
  plot1,
  plot2,
  nrow = 1,
  top = BdivBmsy_FAO_areas[1]
)


grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

grid_arrange_shared_legend(plot1, plot2)

plot_grid(plot2, plot1, labels=c("A", "B"), nrow = 1)

# this is the best version at the moment (includes overall figure title)
ggdraw() +
  #draw_label(BdivBmsy_FAO_areas[1], x = 0.5, y = 0.985) +
  draw_plot(plot2, x = 0, y = 0, width = 0.5, height = 0.82) +
  draw_plot(plot1, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_label(BdivBmsy_FAO_areas[1], x = 0.5, y = 0.98)





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

