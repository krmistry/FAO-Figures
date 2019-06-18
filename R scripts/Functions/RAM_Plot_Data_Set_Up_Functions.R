### RAM Legacy Stock Assessment Database - Data Transformation Functions ##########
## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 5/31/2019

#--------------------------------------------------------------------------------
# Function to find number of unique elements in a column of a dataframe
count_unique_elements <- function(data, column_name) {
  column_number <- match(column_name, names(data))
  length(unique(data[, column_number]))
}

#--------------------------------------------------------------------------------
# Function to find the minimum number in a column of a dataframe
find_column_min <- function(data, column_name) {
  column_number <- match(column_name, names(data))
  min(data[, column_number])
}

#--------------------------------------------------------------------------------
# Function to find the maximum number in a column of a dataframe
find_column_max <- function(data, column_name) {
  column_number <- match(column_name, names(data))
  max(data[, column_number])
}

#--------------------------------------------------------------------------------
# Function creating dataframe with the number of stocks and either taxonomy 
# groups in each region or regions in taxonomy groups, along with the first 
# and last year for data in each region or taxGroup. This dataframe is used 
# to generate data to put in the table at the top of region or taxGroup pages 
# on the website.
#
# Parameters:
#--- input_data is timeseries_values_views in RAM v4.44
#--- region_or_taxGroup is the vector with either region or taxonomy names
#     that match the corresponding input_data row values (regions or 
#     TB_taxGroup_list)
#--- row_names will be either region_plot_titles or TB_taxGroup_plot_titles

summary_fun <- function(type_of_plot = c("region", "taxGroup"),
                          input_data,
                          number_taxGroup_or_region, 
                          row_names) {
  dest_dataframe <- data.frame(matrix(NA, 
                                      nrow = number_taxGroup_or_region, 
                                      ncol = 4))
  rownames(dest_dataframe) <- sort(row_names)
  if (type_of_plot == "region") {
    colnames(dest_dataframe) <- c("Num_Stocks", 
                                  "Num_taxGroup", 
                                  "First_year", 
                                  "Last_year")
    dest_dataframe[, 2] <- unlist(lapply(input_data, 
                                         count_unique_elements, 
                                         column_name = "taxGroup"))
  } else {
    colnames(dest_dataframe) <- c("Num_Stocks", 
                                  "Num_Region", 
                                  "First_year", 
                                  "Last_year")
    dest_dataframe[, 2] <- unlist(lapply(input_data, 
                                         count_unique_elements, 
                                         column_name = "region"))
  }
  dest_dataframe[, 1] <- unlist(lapply(input_data, 
                                count_unique_elements, 
                                column_name = "stockid"))
  dest_dataframe[, 3] <- unlist(lapply(input_data, 
                                find_column_min, column_name = "year"))
  dest_dataframe[, 4] <- unlist(lapply(input_data, 
                                find_column_max, column_name = "year"))
  return(dest_dataframe)
}

#--------------------------------------------------------------------------------
# Function to populate mean_biomass function with stock values (mean biomass, first
# year, last year, and taxGroup)
mean_biomass_values_fun <- function(mean_biomass_list,
                                    input_data,
                                    #region_or_taxGroup_labels,
                                    number_of_stocks, 
                                    stock_names,
                                    fifth_column_name = c("taxGroup", 
                                                          "region")) {
  for (i in 1:length(number_of_stocks)) {
    mean_biomass_list[[i]] <- data.frame(matrix(NA, 
                                                nrow = number_of_stocks[i], 
                                                ncol = 5))
    colnames(mean_biomass_list[[i]]) <- c("stock_names", 
                                     "mean_bio", 
                                     "first_year", 
                                     "last_year", 
                                     fifth_column_name)
    for (j in 1:number_of_stocks[i]) {
      x <- input_data[[i]][input_data[[i]]$stocklong == stock_names[[i]][j], ]
      mean_biomass_list[[i]][j, 1] <- as.character(x$stocklong[1])
      mean_biomass_list[[i]][j, 2] <- round(mean(x$TBbest))
      mean_biomass_list[[i]][j, 3] <- max(min(x$year), 1950)
      mean_biomass_list[[i]][j, 4] <- max(x$year)
      if (fifth_column_name == "taxGroup") {
        mean_biomass_list[[i]][j, 5] <- as.character(x$taxGroup[1])
      } else {
        mean_biomass_list[[i]][j, 5] <- as.character(x$region[1])
      }
    }
  }
  return(mean_biomass_list)
}

#--------------------------------------------------------------------------------
# Function to calculate average biomass over the time series for each stock in 
# each region or taxGroup and extract the first and last years that each stock 
# appears in the assessment 
#
# Parameters:
#--- input_data is All_TBbest.df_region_list or All_TBbest.df_taxGroup_list
#--- region_or_taxGroup_labels is either region_labels or TB_taxGroup_labels
#--- number_region_or_taxGroup is either number_regions or number_TB_taxGroups
#--- regions_or_taxGroup is the vector with either region or taxonomy names 
#     that match the corresponding input_data row values
#--- regions_or_taxGroup is either regions or TB_taxGroup_list

mean_biomass_fun <- function(type_of_plot = c("Region", "taxGroup"),
                             regions_or_taxGroup,
                             number_region_or_taxGroup,
                             input_data,
                             #regions_or_taxGroup,
                             year_min) {
  
  mean_biomass <- vector("list", length = number_region_or_taxGroup)
  names(mean_biomass) <- sort(regions_or_taxGroup)
  
  num_stocks <- unlist(lapply(input_data, 
                              count_unique_elements, 
                              column_name = "stockid"))
  stock_names <- lapply(input_data, 
                        function(x) {
                          column_number <- match("stocklong", names(input_data[[1]]))
                          unique(x[, column_number])
                        })
  if (type_of_plot == "Region") {
    mean_biomass <- mean_biomass_values_fun(mean_biomass_list = mean_biomass,
                                            input_data = input_data,
                                            #region_or_taxGroup_labels = regions_or_taxGroup, 
                                            number_of_stocks = num_stocks, 
                                            stock_names = stock_names, 
                                            fifth_column_name = "taxGroup")
  } else {
    mean_biomass <- mean_biomass_values_fun(mean_biomass_list = mean_biomass,
                                            input_data = input_data,
                                            #region_or_taxGroup_labels = regions_or_taxGroup, 
                                            number_of_stocks = num_stocks, 
                                            stock_names = stock_names, 
                                            fifth_column_name = "region")
  }
  mean_biomass <- lapply(mean_biomass, 
                         function(x, col = 2){
                           arrange(x, desc(x[, col]))
                         })
  # Adding jittering to duplicated values so that they will plot on separate
  # lines for biomass coverage plots
  for (i in 1:number_region_or_taxGroup) {
    mean_biomass[[i]]$jittered_mean <- mean_biomass[[i]]$mean_bio
    dup_index <- which(duplicated(mean_biomass[[i]]$mean_bio) == TRUE)
    mean_biomass[[i]]$jittered_mean[dup_index] <- jitter(mean_biomass[[i]]$mean_bio[dup_index])
  }
  return(mean_biomass)
}


#--------------------------------------------------------------------------------
# Function to create custom y axis vectors for each of the region or taxGroup
# biomass coverage for all stocks plots

custom_y_axis_fun <- function(number_region_or_taxGroup, 
                              region_or_taxGroup,
                              mean_biomass_data) {
  y_axis_labels <- c(1, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 
                     1000000, 5000000, 10000000, 50000000)
  custom_y_axis <- vector("list", length = number_region_or_taxGroup)
  names(custom_y_axis) <- region_or_taxGroup
  
  for (i in 1:number_region_or_taxGroup) {
    custom_y_axis[[i]]$max <- max(mean_biomass_data[[i]]$mean_bio)
    custom_y_axis[[i]]$min <- min(mean_biomass_data[[i]]$mean_bio)
    custom_y_axis[[i]]$max[2] <- y_axis_labels[(y_axis_labels >= custom_y_axis[[i]]$max) == TRUE][1]
    custom_y_axis[[i]]$min[2] <- rev(y_axis_labels[(y_axis_labels <= custom_y_axis[[i]]$min) == TRUE])[1]
    custom_y_axis[[i]]$y_axis_labels <- subset(y_axis_labels, 
                                               y_axis_labels >= custom_y_axis[[i]]$min[2] & 
                                                 y_axis_labels <= custom_y_axis[[i]]$max[2])
  }
  return(custom_y_axis)
}


#--------------------------------------------------------------------------------
# Function to calculate values for mean_SP_fun
mean_SP_fun <- function(input_data,
                        region_or_taxGroup_labels,
                        number_region_or_taxGroup) {
  surplus_mean_biomass <- vector("list", length = number_region_or_taxGroup)
  names(surplus_mean_biomass) <- sort(region_or_taxGroup_labels)
  
  num_stocks <- unlist(lapply(input_data, 
                              count_unique_elements, 
                              column_name = "stockid"))
  stock_names <- lapply(input_data, 
                        function(x) {
                          column_number <- match("stocklong", 
                                                 names(input_data[[1]]))
                          unique(x[, column_number])
                        })
  for (i in 1:number_region_or_taxGroup) {
    surplus_mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks[i], 
                                                   ncol = 3))
    colnames(surplus_mean_biomass[[i]]) <- c("stock_names", 
                                             "mean_bio",
                                             "mean_surplus")
    for (j in 1:num_stocks[i]) {
      x <- input_data[[i]][input_data[[i]]$stocklong == stock_names[[i]][j], ]
      surplus_mean_biomass[[i]][j, 1] <- as.character(stock_names[[i]][j])
      surplus_mean_biomass[[i]][j, 2] <- round(mean(x$B))
      surplus_mean_biomass[[i]][j, 3] <- round(mean(abs(x$SP)))
    }
    surplus_mean_biomass[[i]]$percent_of_total <- surplus_mean_biomass[[i]][, 3]/sum(surplus_mean_biomass[[i]][, 3])
  }
  surplus_mean_biomass <- lapply(surplus_mean_biomass, 
                                 function(x) {
                                   arrange(x, desc(mean_bio))
                                 })
  return(surplus_mean_biomass)
}

# ptm <- proc.time()
# test <- mean_SP_values_calc(surplus_region_list, region_labels, number_regions)
# proc.time() - ptm
#--------------------------------------------------------------------------------
# Function creating dataframe with absolute mean surplus production for each 
# stock in each region or taxGroup, ordered by mean B so that the top 4 stocks 
# can be plotted individually

# mean_SP_fun <- function(surplus_file, 
#                         type_of_plot = c("region", "taxGroup"),
#                         region_or_taxGroup,
#                         region_or_taxGroup_labels,
#                         number_region_or_taxGroup) {
#   
#   surplus_mean_biomass <- vector("list", length = number_region_or_taxGroup)
#   names(surplus_mean_biomass) <- sort(region_or_taxGroup_labels)
#   
#   num_stocks <- unlist(lapply(input_data, 
#                               count_unique_elements, 
#                               column_name = "stockid"))
#   stock_names <- lapply(input_data, 
#                         function(x) {
#                           column_number <- match("stocklong", 
#                                                  names(input_data[[1]]))
#                           unique(x[, column_number])
#                         })
#   if (type_of_plot == "region") {
#     # for (i in 1:number_region_or_taxGroup) {
#     #   x <- subset(surplus_file, 
#     #               surplus_file$region == region_or_taxGroup[i] & 
#     #                 is.na(surplus_file$SP) == FALSE)
#     #   sp_stock_names <- unique(x$stocklong)
#     #   num_stocks <- length(sp_stock_names)
#     #   surplus_mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 3))
#     #   colnames(surplus_mean_biomass[[i]]) <- c("stock_names", "mean_bio","mean_surplus")
#     #   for (j in 1:num_stocks) {
#     #     surplus_mean_biomass[[i]][j, 1] <- as.character(sp_stock_names[j])
#     #     surplus_mean_biomass[[i]][j, 2] <- round(mean(x$B[x$stocklong == sp_stock_names[j]]))
#     #     surplus_mean_biomass[[i]][j, 3] <- round(mean(abs(x$SP[x$stocklong == sp_stock_names[j]])))
#     #     surplus_mean_biomass[[i]] <- arrange(surplus_mean_biomass[[i]], desc(mean_bio))
#     #   }
#     #   surplus_mean_biomass[[i]]$percent_of_total <- surplus_mean_biomass[[i]][, 3]/sum(surplus_mean_biomass[[i]][, 3])
#     # }
#     surplus_mean_biomass <- mean_SP_values_calc(surplus_mean_biomass_list = surplus_mean_biomass,
#                                                 input_data = input_data,
#                                                 type_of_plot = "region",
#                                                 )
#   } else {
#     for (i in 1:number_region_or_taxGroup) {
#       x <- subset(surplus_file, 
#                   surplus_file$taxGroup == region_or_taxGroup[i] & 
#                     is.na(surplus_file$SP) == FALSE)
#       sp_stock_names <- unique(x$stocklong)
#       num_stocks <- length(sp_stock_names)
#       surplus_mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 3))
#       colnames(surplus_mean_biomass[[i]]) <- c("stock_names", "mean_bio","mean_surplus")
#       for (j in 1:num_stocks) {
#         surplus_mean_biomass[[i]][j, 1] <- as.character(sp_stock_names[j])
#         surplus_mean_biomass[[i]][j, 2] <- round(mean(x$B[x$stocklong == sp_stock_names[j]]))
#         surplus_mean_biomass[[i]][j, 3] <- round(mean(abs(x$SP[x$stocklong == sp_stock_names[j]])))
#         surplus_mean_biomass[[i]] <- arrange(surplus_mean_biomass[[i]], desc(mean_bio))
#       }
#       surplus_mean_biomass[[i]]$percent_of_total <- surplus_mean_biomass[[i]][, 3]/sum(surplus_mean_biomass[[i]][, 3])
#     }
#   }
#   surplus_mean_biomass <- lapply(surplus_mean_biomass, 
#                                  function(x) {
#                                    arrange(x, desc(mean_bio))
#                                    })
#   return(surplus_mean_biomass)
# } 









