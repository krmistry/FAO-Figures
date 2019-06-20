### FAO Report Plots ##########
## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 6/19/2019

library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(here)

# Start the clock
ptm <- proc.time()

# The purpose of this code is to produce the following plots for the FAO report:
# -----------
# - BdivBmsy proportion of stocks plot
#
# - Biomass Coverage plots -- horizontal bar chart that plots the year range 
#     of data covered inthe most recent assessment for each stock, beginning in
#     year_min (1950 as of this revision). Each bar represents an individual
#     stock, sorted vertically from largest to smallest by average biomass over
#     the time series. There are 2 versions of this plot; one that scales with
#     biomass and has all stocks in 1 plot, and the other that doesn't scale
#     with biomass so all stocks are equally visible (stocks are individually
#     labeled in this version)
#
# - Surplus Production plots -- vertical bar chart that plots annual surplus 
#     production (net increase of biomass plus catch) over time beginning in
#     year_min for the four stocks with the highest average biomass in the
#     region or taxGroup. The plot also includes a subtitle that has the
#     percentage of absolute surplus production that the stock represents in the
#     region or taxGroup.
#
# - Surplus Production vs. Biomass plots -- line and point plot that shows path 
#     of annual surplus production compared to annual biomass over time
#     (beginning in year_min) for the four stocks that contribute the most to
#     absolute surplus production in the region or taxGroup. Years are
#     represented by a continuous color scale, with the first and last years
#     labeled on the plot. 0 is emphasized with a thick black line.



########## All Directories (source and to save plots to) #####################
#------- CHANGE AS NEEDED to match local directories before running code------

# String with folder name for each type of plot, to combine with source directory 
# and either by_region or by_taxGroup:
BdivBmsy_prop_folder <- "Figures/BdivBmsy Proportion of Stocks"

biomass_coverage_by_stock_folder <- "Figures/Biomass Coverage by Stock/"
biomass_coverage_all_stocks_folder <- "Figures/Biomass Coverage All Stocks/"
surplus_top_4_folder <- "Figures/Surplus Production - Top 4 Stocks/"
surplus_v_biomass_top_4_folder <- "Figures/Surplus v Biomass - Top 4 Stocks/"
# summary_pdf_of_plots_folder <- "Summary PDF of Plots/"
# summary_table_folder <- "Summary Tables/"

###############################################################################
########## Initial Set Up ####################################################
#

# ****** Make sure the RAM v4.44 folder is in the source directory! ********
#setwd(source_directory)

# Load custom functions for data set up:
source(here::here("R scripts/Functions/RAM_Plot_Data_Set_Up_Functions.R"))

# Minimum year used for all data set up in the RAM_Plot_Data_Set_up.R file:
year_min <- 1950

# Load data sets needed for plots
source(here::here("R scripts/Analysis scripts/RAM_Plot_Data_Set_up.R"))

# Datasets loaded from RAM: 
# - timeseries_values_views.csv
# - taxonomy.csv
# - stock.csv
# - sp.data.csv 
# - bioparams_values_views.csv
#
# Transformed datasets (filtered or calcualted from above RAM data) loaded, and 
# which outputs they are used with:
# - All_TBbest.df -- timeseries_values_views.csv without rows with NA in the 
#     TB column; used as the primary data source for most of the transformed
#     data sets described below
# 
# - stock_tax_per_region -- Number of stocks, taxonomy groups and first & last 
#     year from timeseries_values_views with data in each region, used to
#     produce summary tables at top of region pages.
#     - TB_stock_tax_per_region is a version of the above from the All_TBbest.df
#       data; used in the biomass coverage by stocks plot function, and to
#       double check that no regions have more than 90 stocks.
#       
# - stock_region_per_taxgroup -- Number of stocks, regions and first & last 
#     year with data in each taxonomy group, used to produce summary tables at
#     top of taxonomy group pages
#     - TB_stock_region_per_taxgroup is a version of the above from the 
#       All_TBbest.df data; used in the biomass coverage by stocks plot
#       function, and to double check that no taxGroups have more than 90
#       stocks.
#       
# - region_mean_biomass -- list of region dataframes with average biomass over 
#     the time series for each stock, the first and last years that each stock
#     appears in the latest assessment, the stock name, and a jittered mean
#     column, which is used to make sure that there isn't any overlapping stocks
#     in the plot. Used in the biomass coverage by stock and for all stocks
#     plots.
#
# - TB_taxGroup_mean_biomass -- Same content as the region_mean_biomass, but 
#     separated into dataframes by taxGroup
#
# - MSY_per_region -- list of region dataframes, with each year and MSY summed 
#     for each year; NOT CURRENTLY USED - originally calculated to use in
#     surplus production of all stocks plot
#
# - MSY_per_TB_taxGroup -- same content as MSY_per_region but with dataframes 
#     separated by taxGroup; NOT CURRENTLY USED
#
# - region_surplus_mean_biomass -- list of region dataframes with absolute average 
#     surplus production, average biomass, and the percent of total (absolute)
#     average surplus production for the region of each stock; used in the
#     surplus production for top 4 stock plots and the surplus productivity vs.
#     biomass for top 4 stock plots
#
# - TB_taxGroup_surplus_mean_biomass -- same content as region_surplus_mean_biomass,
#     but separated into dataframes by taxGroup
#
# - region_sum_bio_SP_per_year -- 
#
# - TB_taxGroup_sum_bio_SP_per_year

# Load custom functions for creating plots:
source(here::here("R scripts/Functions/RAM_Plot_Creation_Functions.R"))

# Functions loaded:
#------------------------------------------------------------------------------
# basic_biomass_by_stock_ggplot(dataframe, 
#                               first_stock, 
#                               end_stock, 
#                               plot_title,
#                               type_of_plot = c("Region", "Taxonomy Group"), 
#                               custom_colors,
#                               color_segment = c("region", "taxGroup"))
#
# biomass_plot_fun(dataframe,
#                  end_stock,
#                  plot_title,
#                  type_of_plot = c("Region", "Taxonomy Group"),
#                  custom_colors,
#                  color_segment = c("region", "taxGroup"),
#                  print = TRUE,
#                  save = FALSE)
# 
# biomass_all_stock_ggplot(dataframe, 
#                          y_axis, 
#                          plot_title, 
#                          custom_colors,
#                          type_of_plot = c("Region", "Taxonomy Group"), 
#                          color_segment = c("region", "taxGroup"), 
#                          print = TRUE, 
#                          save = FALSE)
# 
# surplus_top_stocks_plot(dataframe, 
#                         type_of_plot = c("Region", "Taxonomy Group"),
#                         region_or_taxGroup, 
#                         SP_mean_biomass_data, 
#                         stock, 
#                         percent_surplus,
#                         plot_title, 
#                         custom_colors, 
#                         color_legend_title = c("Taxonomy Group", "Region"), 
#                         color_segment = c("taxGroup", "region"), 
#                         print = TRUE,
#                         save = FALSE)
# 
# top_stocks_SP_vs_bio_plot(surplus_data, 
#                           stock, 
#                           regions_or_taxGroup,
#                           SP_mean_biomass_data,
#                           type_of_plot = c("Region", "Taxonomy Group"),
#                           plot_titles,
#                           print = TRUE,
#                           save = FALSE)
# 

# Stop operation if there are any mismatches in stockid and scientificname:
if (length(setdiff(timeseries_values_views$stockid, stock_info$stockid)) != 0 |
    length(setdiff(stock_info$scientificname, taxonomy$scientificname)) != 0) {
  stop(paste("Either stockid or scientificname has discrepancies between 
             timeseries_values_views and stock_info or between stock_info and taxonomy"))
}


###############################################################################
########## Biomass divided by Biomass at MSY (BdivBmsy) #######################
#
# BdivBmsy < 0.8 (red), between 0.8-1.2 (yellow), and >1.2 (green)
# instead of solid colors for each of 3 bands, scale them so they range from full 
# saturation for years of 100% coverage of B/BMSY estimates, and no saturation (white) 
# for years of 0% coverage. Could be done by using alpha parameter of rgb() that 
# depends on fraction of stocks covered in any given year, but there could be other 
# ways as well. 
# - x-axis = “Year”, y-axis = “Proportion of stocks in B/BMSY category”, plot title = FAOname
# - legend 1 = “Stock status:”, with categories B/BMSY < 0.8, 0.8 < B/BMSY < 1.2, 1.2 < B/BMSY
# - legend 2 = “Coverage:”, with 3 horizontal bands ranging from white to full saturation 
# (R/Y/G), and under or over the 3 bands, a few markers such as “0%, 25%, 50%... 100%”. 
# See the 3-panel state-space model plots that you’ve put on the website by region or 
# taxgroup as an example, now there would be 3 bands instead of 1

# Data needed: 
# - number of stocks in each year in each FAO area - use existing function
# - B/Bmsy divided into 3 categories; <0.8, 0.8 - 1.2, >1.2 - write from scratch



###############################################################################
########## Biomass Coverage ###################################################
#
# This describes the years of data that are covered in the most recent
# assessment for each stock. The bars for each stock are color coded based on
# taxGroup OR region, and bars are sorted vertically by average biomass over the
# time series. In some regions/taxGroups with many stocks the bars get squished,
# so they can spill over onto multiple pages. These can also read off “TBbest”
# in the timeseries_values_views table, linking to taxGroup in the stock table.
#
#----------------------- Data separated by REGION------------------------------

# Produce and save all of the regions' biomass coverage by stocks plots:
unlist(lapply(regions, 
              biomass_plot_fun, 
              num_stocks_df = TB_stock_tax_per_region, 
              type_of_plot = "Region",
              file_path = region_biomass_coverage_by_stock_directory,
              print = FALSE,
              save = TRUE))



# Loop to produce and save all of the taxonomy group's biomass coverage for all 
# stocks plots:
unlist(lapply(regions,
              biomass_all_stock_ggplot,
              type_of_plot = "Region",
              file_path = region_biomass_coverage_all_stocks_directory,
              print = FALSE,
              save = TRUE))


#----------------------- Data separated by TAXONOMY GROUP ----------------------

# Loop to produce and save all of the taxonomy group's biomass coverage by 
# stock plots:
unlist(lapply(TB_taxGroup_list, 
              biomass_plot_fun, 
              num_stocks_df = TB_stock_region_per_taxgroup, 
              type_of_plot = "Taxonomy Group",
              file_path = taxGroup_biomass_coverage_by_stock_directory,
              print = FALSE,
              save = TRUE))



# Loop to produce and save all of the taxonomy group's biomass coverage for all 
# stocks plots:
unlist(lapply(TB_taxGroup_list,
              biomass_all_stock_ggplot,
              type_of_plot = "Taxonomy Group",
              file_path = taxGroup_biomass_coverage_all_stocks_directory,
              print = FALSE,
              save = TRUE))


###############################################################################
########## Added Productivity #################################################
#
# Plotting annual surplus production (net increase of biomass, plus catch) over
# time for the four stocks with highest average biomass in each region or
# taxGroup (based on the average across years of the absolute values of annual
# surplus production). Color-coded by taxGroup or region. Data comes from
# surplus.csv file.

#----------------------- Data separated by REGION------------------------------
region_surplus_nrow <- unlist(lapply(region_surplus_mean_biomass, nrow))
region_surplus_less_than_4 <- which(region_surplus_nrow < 4)
region_surplus_greater_than_4 <- which(region_surplus_nrow > 4)

# Create and save surplus production plots for the 4 stocks with highest average 
# biomass in each region:
for (i in region_surplus_greater_than_4) {
  for (j in 1:4) {
    surplus_top_stocks_plot(dataframe = surplus_region_list[[i]], 
                            type_of_plot = "Region",
                            SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                            stock_row = j,
                            file_path = region_surplus_top_4_directory,
                            print = FALSE,
                            save = TRUE)
  }
}
for (i in region_surplus_less_than_4) {
  for (k in 1:nrow(region_surplus_mean_biomass[[i]])) {
    surplus_top_stocks_plot(dataframe = surplus_region_list[[i]], 
                            type_of_plot = "Region",
                            SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                            stock_row = k,
                            file_path = region_surplus_top_4_directory,
                            print = FALSE,
                            save = TRUE)
  }
}


#----------------------- Data separated by TAXONOMY GROUP ----------------------
TB_taxGroup_surplus_nrow <- unlist(lapply(TB_taxGroup_surplus_mean_biomass, nrow))
TB_taxGroup_surplus_less_than_4 <- which(TB_taxGroup_surplus_nrow < 4)
TB_taxGroup_surplus_greater_than_4 <- which(TB_taxGroup_surplus_nrow > 4)

# Create and save surplus production plots for the 4 stocks with highest average 
# biomass in each region:
for (i in TB_taxGroup_surplus_greater_than_4) {
  for (j in 1:4) {
    surplus_top_stocks_plot(dataframe = surplus_taxGroup_list[[i]], 
                            type_of_plot = "Taxonomy Group",
                            SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[i]],
                            stock_row = j,
                            file_path = taxGroup_surplus_top_4_directory,
                            print = FALSE,
                            save = TRUE)
  }
}
for (i in TB_taxGroup_surplus_less_than_4) {
  for (k in 1:nrow(TB_taxGroup_surplus_mean_biomass[[i]])) {
    surplus_top_stocks_plot(dataframe = surplus_taxGroup_list[[i]],
                            type_of_plot = "Taxonomy Group",
                            SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[i]],
                            stock_row = k,
                            file_path = taxGroup_surplus_top_4_directory,
                            print = FALSE,
                            save = TRUE)
  }
}

###############################################################################
########## Biomass X Productivity #############################################
#
# This shows relationships between surplus production (y-axis) and biomass
# (x-axis), for the four stocks with the highest average biomass in the region
# or taxGroup (same as those in Added Productivity plots above). Points are
# colored from blue to red over time, starting in 1950 or the earliest year with
# data. These can read off surplus production file which contains paired surplus
# production and biomass time series by stock.

#----------------------- Data separated by REGION------------------------------
# Create and save surplus production vs. biomass plots for the 4 stocks with highest average 
# biomass in each region:
for (j in 1:number_regions) {
  if (nrow(region_surplus_mean_biomass[[j]]) > 4) {
    for (i in 1:4) {
      top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                stock = region_surplus_mean_biomass[[j]][i, 1],
                                regions_or_taxGroup = regions[j],
                                SP_mean_biomass_data = region_surplus_mean_biomass[[j]],
                                type_of_plot = "Region",
                                file_path = region_surplus_v_biomass_top_4_directory,
                                print = FALSE,
                                save = TRUE)
    }
  }  else {
    for (k in 1:nrow(region_surplus_mean_biomass[[j]])) {
      top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                stock = region_surplus_mean_biomass[[j]][k, 1],
                                regions_or_taxGroup = regions[j],
                                SP_mean_biomass_data = region_surplus_mean_biomass[[j]],
                                type_of_plot = "Region",
                                file_path = region_surplus_v_biomass_top_4_directory,
                                print = FALSE,
                                save = TRUE)
    }
  }
}


#----------------------- Data separated by TAXONOMY GROUP ----------------------
# Create and save surplus production vs. biomass plots for the 4 stocks with highest average 
# biomass in each region:
for (j in 1:number_TB_taxGroups) {
  if (nrow(TB_taxGroup_surplus_mean_biomass[[j]]) > 4) {
    for (i in 1:4) {
      top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                stock = TB_taxGroup_surplus_mean_biomass[[j]][i, 1],
                                regions_or_taxGroup = TB_taxGroup_list[j],
                                SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[j]],
                                type_of_plot = "Taxonomy Group",
                                file_path = taxGroup_surplus_v_biomass_top_4_directory,
                                print = FALSE,
                                save = TRUE)
    }
  }  else {
    for (k in 1:nrow(TB_taxGroup_surplus_mean_biomass[[i]])) {
      top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                stock = TB_taxGroup_surplus_mean_biomass[[j]][i, 1],
                                regions_or_taxGroup = TB_taxGroup_list[j],
                                SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[j]],
                                type_of_plot = "Taxonomy Group",
                                file_path = taxGroup_surplus_v_biomass_top_4_directory,
                                print = FALSE,
                                save = TRUE)
    }
  }
}


###############################################################################
######### PDF of all Plots ####################################################
#
# Create single PDF of all the above plots for each region and taxGroup
#
# Order of plots: biomass coverage for all stocks, biomass coverage by stock, 
# status of assessed stocks, surplus production for the top 4 stocks, and the 
# surplus production vs. biomass for the top 4 stocks

#----------------------- Data separated by REGION------------------------------

for (i in 1:number_regions) {
  pdf(file = paste(region_summary_PDF_directory, sort(regions)[i], 
                   "Summary Plots.pdf"), width = 14, height = 8)
  
  plot_1 <- biomass_all_stock_ggplot(region_or_taxGroup = sort(regions)[i],
                                     type_of_plot = "Region",
                                     print = FALSE,
                                     save = FALSE)
  print(plot_1)
  
  plot_2 <- biomass_plot_fun(region_or_taxGroup = sort(regions)[i],
                             num_stocks_df = TB_stock_tax_per_region,
                             type_of_plot = "Region",
                             print = FALSE,
                             save = FALSE)
  print(plot_2)
  
  if (nrow(region_surplus_mean_biomass[[i]]) > 4) {
    for (j in 1:4) {
      plots_3 <- surplus_top_stocks_plot(dataframe = surplus_region_list[[i]], 
                                         type_of_plot = "Region",
                                         SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                                         stock_row = j,
                                         print = FALSE,
                                         save = FALSE)
      print(plots_3)
    }
  } else {
    for (k in 1:nrow(region_surplus_mean_biomass[[i]])) {
      plots_3 <- surplus_top_stocks_plot(dataframe = surplus_region_list[[i]], 
                                         type_of_plot = "Region",
                                         SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                                         stock_row = k,
                                         print = FALSE,
                                         save = FALSE)
      print(plots_3)
    }
  }
  
  if (nrow(region_surplus_mean_biomass[[i]]) > 4) {  
    for (j in 1:4) {
      plots_4 <- top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                           stock = region_surplus_mean_biomass[[i]][j, 1],
                                           regions_or_taxGroup = regions[i],
                                           SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                                           type_of_plot = "Region",
                                           print = FALSE,
                                           save = FALSE)
      print(plots_4)
    } 
  } else {
      for (k in 1:nrow(region_surplus_mean_biomass[[i]])) {
        plots_4 <- top_stocks_SP_vs_bio_plot(surplus_data = surplus,
                                             stock = region_surplus_mean_biomass[[i]][k, 1],
                                             regions_or_taxGroup = regions[i],
                                             SP_mean_biomass_data = region_surplus_mean_biomass[[i]],
                                             type_of_plot = "Region",
                                             print = FALSE,
                                             save = FALSE)
        print(plots_4)
      }
    }
  dev.off()
}

  
#----------------------- Data separated by TAXONOMY GROUP------------------------------
#

for (i in 1:number_TB_taxGroups) {
  pdf(file = paste(taxGroup_summary_PDF_directory, sort(TB_taxGroup_list)[i], 
                   "Summary Plots.pdf"), width = 14, height = 8)
  
  plot_1 <- biomass_all_stock_ggplot(region_or_taxGroup = sort(TB_taxGroup_list)[i],
                                     type_of_plot = "Taxonomy Group",
                                     print = FALSE,
                                     save = FALSE)
  print(plot_1)
  plot_2 <- biomass_plot_fun(region_or_taxGroup = sort(TB_taxGroup_list)[i],
                             num_stocks_df = TB_stock_region_per_taxgroup,
                             type_of_plot = "Taxonomy Group",
                             print = FALSE,
                             save = FALSE)
  print(plot_2)
  if (nrow(TB_taxGroup_surplus_mean_biomass[[i]]) > 4) {
    for (j in 1:4) {
      plots_3 <- surplus_top_stocks_plot(dataframe = surplus_taxGroup_list[[i]], 
                                         type_of_plot = "Taxonomy Group",
                                         SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[i]],
                                         stock_row = j,
                                         print = FALSE,
                                         save = FALSE)
      print(plots_3)
    }
  } else {
    for (k in 1:nrow(TB_taxGroup_surplus_mean_biomass[[i]])) {
      plots_3 <- surplus_top_stocks_plot(dataframe = surplus_taxGroup_list[[i]], 
                                         type_of_plot = "Taxonomy Group",
                                         SP_mean_biomass_data = TB_taxGroup_surplus_mean_biomass[[i]],
                                         stock_row = k,
                                         print = FALSE,
                                         save = FALSE)
      print(plots_3)
    }
  }
  if (nrow(TB_taxGroup_surplus_mean_biomass[[i]]) > 4) {  
    for (j in 1:4) {
      plots_4 <- top_stocks_SP_vs_bio_plot(surplus,
                                           TB_taxGroup_surplus_mean_biomass[[i]][j, 1],
                                           sort(TB_taxGroup_list)[i],
                                           TB_taxGroup_surplus_mean_biomass[[i]],
                                           "Taxonomy Group",
                                           print = FALSE,
                                           save = FALSE)
      print(plots_4)
    } 
  } else {
    for (k in 1:nrow(TB_taxGroup_surplus_mean_biomass[[i]])) {
      plots_4 <- top_stocks_SP_vs_bio_plot(surplus,
                                           TB_taxGroup_surplus_mean_biomass[[i]][k, 1],
                                           sort(TB_taxGroup_list)[i],
                                           TB_taxGroup_surplus_mean_biomass[[i]],
                                           "Taxonomy Group",
                                           print = FALSE,
                                           save = FALSE)
      print(plots_4)
    }
  }
  dev.off()
}



###############################################################################
######### Summary Table CSVs ###################################################
#
# Create csv files of summary information for tables on website, at the top of
# every region and taxGroup page (info included: number of stocks, number of
# taxGroups for region-separated data or number of regions for
# taxGroup-separated data, and first year and last year found in that region or
# taxGroup data set)
# 

#----------------------- Data separated by REGION------------------------------
# Uses the stock_tax_per_region dataframe as the data source
#
region_column_one_text <- c("Number of individual stocks:", 
                     "Number of taxonomy groups represented by stocks:",
                     "Earliest year with available data in this region:",
                     "Latest year with available data in this region:")

for (i in 1:number_regions) {
  region_summary_table <- as.data.frame(matrix(NA, nrow = 4, ncol = 2))
  region_summary_table[, 1] <- region_column_one_text
  region_summary_table[1, 2] <- stock_tax_per_region[i, 1]
  region_summary_table[2, 2] <- stock_tax_per_region[i, 2]
  region_summary_table[3, 2] <- stock_tax_per_region[i, 3]
  region_summary_table[4, 2] <- stock_tax_per_region[i, 4]
  write.table(region_summary_table, 
            paste(region_summary_table_directory, regions[i], "Summary Table.csv"),
            row.names = FALSE,
            col.names = FALSE, sep = ",")
}

  
#----------------------- Data separated by TAXONOMY GROUP------------------------------
# Uses the stock_region_per_taxgroup dataframe as the data source; this produces
# more taxgroup summary tables then are actually in used in the website

taxGroup_column_one_text <- c("Number of individual stocks:", 
                     "Number of regions where stocks are found:",
                     "Earliest year with available data in this taxonomy group:",
                     "Latest year with available data in this taxonomy group:")

for (i in 1:number_taxGroups) {
  taxGroup_summary_table <- as.data.frame(matrix(NA, nrow = 4, ncol = 2))
  taxGroup_summary_table[, 1] <- taxGroup_column_one_text
  taxGroup_summary_table[1, 2] <- stock_region_per_taxgroup[i, 1]
  taxGroup_summary_table[2, 2] <- stock_region_per_taxgroup[i, 2]
  taxGroup_summary_table[3, 2] <- stock_region_per_taxgroup[i, 3]
  taxGroup_summary_table[4, 2] <- stock_region_per_taxgroup[i, 4]
  write.table(taxGroup_summary_table, 
            file = paste(taxGroup_summary_table_directory, taxGroup_list[i], "Summary Table.csv"),
            row.names = FALSE,
            col.names = FALSE, sep = ",")
}




###############################################################################
######### Troubleshooting for Plot functions ##################################
#
# Test each function individually, for 1 region or taxGroup at a time
#--------------------------------------------------------------------------------
# All plot functions for region data:
#------------------------------------------------------------------------------
# basic_biomass_by_stock_ggplot(region_or_taxGroup = sort(regions)[20],
#                               first_stock = 1,
#                               end_stock = TB_stock_tax_per_region$Num_Stocks[20],
#                               type_of_plot = "Region")
# 
# biomass_plot_fun(region_or_taxGroup = regions[20],
#                  num_stocks_df = TB_stock_tax_per_region,
#                  type_of_plot = "Region")
# 
# 
# biomass_all_stock_ggplot(region_or_taxGroup = regions[7],
#                          type_of_plot = "Region")

# Single stock surplus plot:
# surplus_top_stocks_plot(dataframe = surplus_region_list[[9]],
#                         type_of_plot = "Region",
#                         SP_mean_biomass_data = region_surplus_mean_biomass[[9]],
#                         stock_row = 1)
# 
# # All 4 stock surplus plots for 1 region:
# for (i in 1:4) {
#   plot <- surplus_top_stocks_plot(dataframe = surplus_region_list[[9]],
#                                   type_of_plot = "Region",
#                                   SP_mean_biomass_data = region_surplus_mean_biomass[[9]],
#                                   stock_row = i)
# }

# Single stock surplus v. biomass plot:
# top_stocks_SP_vs_bio_plot(surplus_data = surplus,
#                           stock = region_surplus_mean_biomass[[2]][4, 1],
#                           regions_or_taxGroup = sort(regions)[2],
#                           SP_mean_biomass_data = region_surplus_mean_biomass[[2]],
#                           type_of_plot = "Region")
# 
# All 4 stock surplus v. biomass plots for 1 region:
# for (i in 1:4) {
#   top_stocks_SP_vs_bio_plot(surplus_data = surplus,
#                             stock = region_surplus_mean_biomass[[2]][i, 1],
#                             regions_or_taxGroup = sort(regions)[2],
#                             SP_mean_biomass_data = region_surplus_mean_biomass[[2]],
#                             type_of_plot = "Region")
# }

#--------------------------------------------------------------------------------
# All plot functions for taxGroup data:
#------------------------------------------------------------------------------
# basic_biomass_by_stock_ggplot(region_or_taxGroup = TB_taxGroup_list[2],
#                               first_stock = 1,
#                               end_stock = TB_stock_region_per_taxgroup$Num_Stocks[2],
#                               type_of_plot = "Taxonomy Group",
#                               custom_colors = taxGroup_myColors)
# 
# biomass_plot_fun(TB_taxGroup_mean_biomass[[7]],
#                  end_stock = TB_stock_region_per_taxgroup$Num_Stocks[7],
#                  taxGroup_plot_titles[7],
#                  "Taxonomy Group",
#                  taxGroup_myColors,
#                  "Region",
#                  "region",
#                  print = TRUE,
#                  save = FALSE)
# 
# biomass_all_stock_ggplot(TB_taxGroup_mean_biomass[[7]],
#                          TB_taxGroup_custom_y_axis[[7]],
#                          TB_taxGroup_plot_titles[7],
#                          taxGroup_myColors,
#                          "Taxonomy Group",
#                          "Region",
#                          "region")
#
# # Single stock surplus plot:
# surplus_top_stocks_plot(surplus,
#                         "Taxonomy Group",
#                         TB_taxGroup_list[14],
#                         TB_taxGroup_surplus_mean_biomass[[14]],
#                         TB_taxGroup_surplus_mean_biomass[[14]][2, 1],
#                         TB_taxGroup_surplus_mean_biomass[[14]][2, 4],
#                         TB_taxGroup_plot_titles[14],
#                         taxGroup_myColors,
#                         "Region",
#                         "region")
# 
# # All 4 stock surplus plots for 1 taxGroup:
# for (i in 1:4) {
#   plot <- surplus_top_stocks_plot(surplus,
#                                   "Taxonomy Group",
#                         TB_taxGroup_list[14],
#                         TB_taxGroup_surplus_mean_biomass[[14]],
#                         TB_taxGroup_surplus_mean_biomass[[14]][i, 1],
#                         TB_taxGroup_surplus_mean_biomass[[14]][i, 4],
#                         TB_taxGroup_plot_titles[14],
#                         taxGroup_myColors,
#                         "Region",
#                         "region")
# }
#
# # Single stock surplus v. biomass plot:
# top_stocks_SP_vs_bio_plot(surplus,
#                           TB_taxGroup_surplus_mean_biomass[[12]][3, 1],
#                           TB_taxGroup_list[12],
#                           TB_taxGroup_surplus_mean_biomass[[12]],
#                           "Taxonomy Group",
#                           TB_taxGroup_plot_titles[12],
#                           print = TRUE)
# 
# # All 4 stock surplus plots for 1 taxGroup:
# for (i in 1:4) {
#   top_stocks_SP_vs_bio_plot(surplus,
#                             TB_taxGroup_surplus_mean_biomass[[1]][i, 1],
#                             TB_taxGroup_list[1],
#                             TB_taxGroup_surplus_mean_biomass[[1]],
#                             "Taxonomy Group",
#                             TB_taxGroup_plot_titles[1])
# }


# Stop the clock
proc.time() - ptm


