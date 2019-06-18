### RAM Legacy Stock Assessment Database - Plot Creation Functions ##########
## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 5/31/2019

library(scales)

#-----------------------------------------------------------------------------------
# Function creating basic biomass coverage plot that will be used for all regions
# and taxGroups.
#
# @param dataframe - needs to be segmented by either region or taxonomy group
# @param plot_title - the region or taxonomy group being plotted
# @oaram type_of_plot - either "Region" or "Taxonomy Group" (will show up in title
#     after the plot_title (either the region or the taxonomy group) and
#     will also be the color legend title)
# @param custom_colors - a vector of named colors, either for the regions or
#     the taxonomy groups
# @param color_segment - either "region" or "taxGroup" (has to match the 
#     column name in the dataframe, to match to aes(color))
#
# @return - horizontal bar plot with a bar for each stock, spanning from the 
#           start to end year of the data for that stock, ordered from highest
#           to lowest average biomass for each stock

basic_biomass_by_stock_ggplot <- function(region_or_taxGroup, 
                                          first_stock, 
                                          end_stock, 
                                          type_of_plot = c("Region", 
                                                           "Taxonomy Group")) {
  if (type_of_plot == "Region") {
    color_legend_title <- "Taxonomy Group"
    color_segment <- "taxGroup"
    custom_colors <- region_myColors
    dataframe <- region_mean_biomass[region_or_taxGroup]
  } else {
    color_legend_title <- "Region"
    color_segment <- "region"
    custom_colors <- taxGroup_myColors
    dataframe <- TB_taxGroup_mean_biomass[region_or_taxGroup]
  }
  
  plot_title <- region_or_taxGroup
  data <- dataframe[[1]][first_stock:end_stock, ]
  
  ggplot(data = data) +
    geom_segment(aes(
      x = as.factor(jittered_mean),
      xend = as.factor(jittered_mean),
      y = first_year,
      yend = last_year,
      color = get(color_segment)
    ), 
    lineend = "butt",
    size = 5) +
    scale_color_manual(name = color_legend_title, values = custom_colors) +
    geom_text(
      data = data,
      aes(
        x = as.factor(jittered_mean),
        y = first_year,
        label = stock_names
      ),
      size = 2.5,
      hjust = -0.05,
      vjust = 0.5
    ) +
    scale_y_continuous(limits = c(1950, 2020), breaks = seq(1950, 2020, 10), 
                       labels = seq(1950, 2020, 10)) +
    scale_x_discrete(labels = rev(comma(dataframe[[1]]$mean_bio[first_stock:end_stock]))) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none",  
          panel.border = element_blank()) +
    labs(title = paste("Biomass Coverage Over Time in", plot_title, type_of_plot, sep = " "), 
         x = "Average biomass of stock (MT)", 
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(guide = guide_legend()) +
    theme(legend.position = "bottom") +
    theme(plot.margin = unit(c(1.5, 1.5, 1, 1.5), "cm"))
}

#-----------------------------------------------------------------------------------
# Function to create, print and/or save 1, 2 or 3 plots to png for regions or  
# taxGroups depending on how many stocks are in each group (less than 30 stocks
# fit on 1 plot, less than 60 on 2, less than 90 on 3). If the number of stocks 
# ever goes over 90 for any region or taxGroup, then this function will need to 
# be adjusted
#
# Almost the same parameters as basic_biomass_by_stock_ggplot, with the addition 
# of print (either TRUE or FALSE) and save (either TRUE or FALSE), and not
# including first_stock

biomass_plot_fun <- function(region_or_taxGroup, 
                             num_stocks_df,
                             type_of_plot = c("Region", 
                                              "Taxonomy Group"),
                             file_path = NULL,
                             print = TRUE,
                             save = FALSE) {
  
  end_stock <- num_stocks_df[rownames(num_stocks_df) == region_or_taxGroup, 1]
  # If any region or taxGroup has more than 90 stocks, stop execution and return the following 
  # error message:
  if (end_stock > 90) {
    stop(paste(region_or_taxGroup, "has more than 90 stocks; biomass_plot_fun will need to be
               modified to accommodate a 4th plot"))
  }
  
  plot_title <- region_or_taxGroup
  
  if (end_stock <= 30) {
    plot_x1 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = 1, 
                                             end_stock = end_stock,
                                             type_of_plot = type_of_plot)
    plot_x1 <- plot_x1 +
      labs(subtitle = "Stocks ordered from lowest to highest average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    if (print == TRUE) {
      print(plot_x1)
    }
    if (save == TRUE) {
      ggsave(plot_x1, filename = paste("Biomass_Coverage_by_stock-", plot_title,".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path)
    }
    if (print == FALSE & save == FALSE) {
      return(plot_x1)
    }
  } else if (end_stock > 30 & end_stock <= 60) {
    plot_x1 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = 1, 
                                             end_stock = round(end_stock/2),
                                             type_of_plot = type_of_plot)
    plot_x1 <- plot_x1 +
      labs(subtitle = "Stocks with highest average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    plot_x2 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = round(end_stock/2) + 1, 
                                             end_stock = end_stock,
                                             type_of_plot = type_of_plot)
    plot_x2 <- plot_x2 + 
      labs(subtitle = "Stocks with lowest average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    if (print == TRUE) {
      print(plot_x1)
      print(plot_x2)
    }
    if (save == TRUE) {
      ggsave(plot_x1,filename = paste("Biomass_Coverage_by_stock-", plot_title, "_p1", ".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path)
      ggsave(plot_x2,filename = paste("Biomass_Coverage_by_stock-", plot_title, "_p2", ".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path)
    }
    if (print == FALSE & save == FALSE) {
      return(list(plot_x1, plot_x2))
    }
  } else {
    plot_x1 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = 1, 
                                             end_stock = round(end_stock/3),
                                             type_of_plot = type_of_plot)
    plot_x1 <- plot_x1 +
      labs(subtitle = "1/3 of Stocks with highest average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    plot_x2 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = round(end_stock/3) + 1, 
                                             end_stock = round(end_stock*(2/3)),
                                             type_of_plot = type_of_plot)
    plot_x2 <- plot_x2 +
      labs(subtitle = "1/3 of Stocks with medium average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    plot_x3 <- basic_biomass_by_stock_ggplot(region_or_taxGroup = region_or_taxGroup, 
                                             first_stock = round(end_stock*(2/3)) + 1, 
                                             end_stock = end_stock,
                                             type_of_plot = type_of_plot)
    plot_x3 <- plot_x3 +
      labs(subtitle = "1/3 of Stocks with lowest average biomass") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    if (print == TRUE) {
      print(plot_x1)
      print(plot_x2)
      print(plot_x3)
    }
    if (save == TRUE) {
      ggsave(plot_x1,filename = paste("Biomass_Coverage_by_stock-", plot_title, "_p1", ".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path)
      ggsave(plot_x2,filename = paste("Biomass_Coverage_by_stock-", plot_title, "_p2", ".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path)
      ggsave(plot_x3,filename = paste("Biomass_Coverage_by_stock-", plot_title, "_p3", ".png",sep=""),
             device = "png", width = 16, height = 7, units = "in", path = file_path) 
    }
    if (print == FALSE & save == FALSE) {
      return(list(plot_x1, plot_x2, plot_x3))
    }
  }
}


#-----------------------------------------------------------------------------------
# Function to create simplified (1 page) biomass coverage plot (with same data
# as the biomass coverage by stock plot function above)
#
# Parameters:
#--- dataframe is either region_mean_biomass or taxGroup_mean_biomass
#--- y_axis is either region_custom_y_axis or taxGroup_custom_y_axis
#--- plot_title is either region_plot_titles or TB_taxGroup_plot_titles
#--- custom_colors is either region_myColors or taxGroup_myColors

biomass_all_stock_ggplot <- function(region_or_taxGroup, 
                                     type_of_plot = c("Region", 
                                                      "Taxonomy Group"),
                                     file_path = NULL,
                                     print = TRUE,
                                     save = FALSE) { #max_mean_biomass only required if creating custom y-axis
  if (type_of_plot == "Region") {
    color_legend_title <- "Taxonomy Group"
    color_segment <- "taxGroup"
    custom_colors <- region_myColors
    dataframe <- region_mean_biomass[region_or_taxGroup][[1]]
    y_axis <- region_custom_y_axis[region_or_taxGroup][[1]]
  } else {
    color_legend_title <- "Region"
    color_segment <- "region"
    custom_colors <- taxGroup_myColors
    dataframe <- TB_taxGroup_mean_biomass[region_or_taxGroup][[1]]
    y_axis <- TB_taxGroup_custom_y_axis[region_or_taxGroup][[1]]
  }
  plot_title <- region_or_taxGroup
  
  plot_all <- ggplot(data = dataframe) +
    geom_segment(aes(
      x = log10(jittered_mean),
      xend = log10(jittered_mean),
      y = first_year,
      yend = last_year,
      color = get(color_segment)
    ), 
    lineend = "butt",
    size = 1.25) +
    scale_color_manual(name = color_legend_title, values = custom_colors) +
    
    # For log10 transformed y-axis values, with custom ranges for each region:
    scale_x_continuous(limits = c(log10(y_axis$min[2]), log10(y_axis$max[2])), 
                       breaks = log10(y_axis$y_axis_labels), 
                       labels = comma(y_axis$y_axis_labels)) +
    scale_y_continuous(limits = c(1950, 2020), breaks = seq(1950, 2020, 10), 
                       labels = seq(1950, 2020, 10)) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none",  
          panel.border = element_blank()) +
    labs(title = paste("Biomass Coverage in", plot_title, type_of_plot, sep = " "), 
         x = "Average biomass (MT)", 
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(guide = guide_legend()) +
    theme(legend.position = "right") +
    theme(plot.margin = unit(c(1.5, 1.5, 1, 1.5), "cm"))
  
  if (print == TRUE) {
    print(plot_all)
  }
  if (save == TRUE) {
    ggsave(plot_all, 
           filename = paste("Biomass_Coverage_all_stocks-", plot_title, 
                            ".png",sep = ""), path = file_path,
           device = "png", width = 8, height = 8, units = "in") 
  }
  if (print == FALSE & save == FALSE) {
    return(plot_all)
  }
}




#-----------------------------------------------------------------------------------
# Function to create plot of surplus production for one stock in one region 
# or taxGroup:

surplus_top_stocks_plot <- function(dataframe, 
                                    type_of_plot = c("Region", 
                                                     "Taxonomy Group"),
                                    SP_mean_biomass_data, 
                                    stock_row,
                                    file_path = NULL,
                                    print = TRUE,
                                    save = FALSE) {
  stock <- SP_mean_biomass_data[stock_row, 1]
  percent_surplus <- SP_mean_biomass_data[stock_row, 4]
  stock_number <- which(SP_mean_biomass_data[, 1] == stock)
  
  surplus_top_stocks_data <- dataframe[dataframe$stocklong == stock & 
                                         dataframe$year >= 1950, ]
  
  if (type_of_plot == "Region") {
    color_legend_title = "Taxonomy Group"
    color_segment = "taxGroup"
    custom_colors = region_myColors
    plot_title = unique(dataframe$region)
  } else {
    color_legend_title = "Region"
    color_segment = "region"
    custom_colors = taxGroup_myColors
    plot_title = unique(dataframe$taxGroup)
  }
  
  surplus_top_stocks <- ggplot(data = surplus_top_stocks_data) +
    geom_col(aes(x = year, y = SP, fill = get(color_segment))) +
    scale_fill_manual(name = color_legend_title, values = custom_colors) +
    scale_x_continuous(limits = c(1950, 2020), breaks = seq(1950, 2020, 10), 
                       labels = seq(1950, 2020, 10)) +
    scale_y_continuous(labels = comma) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    theme_light() +
    theme(legend.position = "none",  
          panel.border = element_blank()) +
    labs(title = paste(stock, "-",  plot_title, type_of_plot, sep = " "), 
         subtitle = paste("Stock represents ", percent(percent_surplus), 
                          " of average absolute surplus production", sep = ""), 
         y = "Surplus production (MT)", 
         x = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(legend.position="bottom") +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  if (print == TRUE) {
    print(surplus_top_stocks)
  }
  if (save == TRUE) {
    ggsave(surplus_top_stocks,
           filename = paste("Surplus_Production_- Stock", stock_number, 
                            plot_title, ".png",sep="_"), path = file_path,
           device = "png", width = 11, height = 8, units = "in")
  }
  if (print == FALSE & save == FALSE) {
    return(surplus_top_stocks)
  }
}



#-----------------------------------------------------------------------------------
# Function to produce surplus production vs. biomass plot for individual stocks

top_stocks_SP_vs_bio_plot <- function(surplus_data,
                                      stock,
                                      regions_or_taxGroup,
                                      SP_mean_biomass_data,
                                      type_of_plot = c("Region",
                                                       "Taxonomy Group"),
                                      file_path = NULL,
                                      print = TRUE,
                                      save = FALSE) {
  plot_data <- surplus_data[surplus_data$stocklong == stock & 
                              surplus_data$year >= year_min, ]
  stock_number <- which(SP_mean_biomass_data[, 1] == stock)
  plot_title <- regions_or_taxGroup
  
  SP_v_Bio_plot <- ggplot(data = plot_data, aes(x = B, y = SP)) +
    geom_point(aes(color = year)) +
    geom_path(aes(color = year), size = 0.25) +
    geom_text(data = plot_data[c(1, nrow(plot_data)), ],
              aes(x = B, y = SP, label = year), vjust = -1, size = 4) +
    geom_hline(yintercept = 0, size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    labs(title = paste(stock, "\nin", plot_title, type_of_plot, sep = " "),
         x = "Biomass (MT)",
         y = "Surplus Production (MT)",
         color = "Year",
         caption = "First and last years with recorded surplus production are labeled") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (print == TRUE) {
    print(SP_v_Bio_plot)
  }
  if (save == TRUE) {
    ggsave(SP_v_Bio_plot,
           filename = paste("Production_v_Biomass-Stock", stock_number, regions_or_taxGroup, 
                            ".png", sep="_"), path = file_path, 
           device = "png", width = 11, height = 8, units = "in")
  }
  if (print == FALSE & save == FALSE) { # returns the plot as an object that can
    return(SP_v_Bio_plot)               # be assigned and called later (to create pdf)
  }
}
