# ------------------------------------------------------------------------------
# Functions to be sourced for Homework 9
# March 25, 2020
# TS O'Leary
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Function: get_data
# Description: Generates random norm dist vals for two groups
# Inputs: n number of individuals, group 1 & 2 name, means, and stdev
# Outputs: output_description

get_data <- function(n, g1_avg, g1_sd, g2_avg, g2_sd, g1 = "g1", g2 = "g2") {
  
  df <- data.frame(grp = c(rep(g1, n), rep(g2, n)),
                   val = c(rnorm(n, mean = g1_avg, sd = g1_sd), 
                           rnorm(n, mean = g2_avg, sd = g2_sd)))
  
  return(df)
} 
# End function -----------------------------------------------------------------


# ------------------------------------------------------------------------------
# Function: calculate_stuff
# Description: Performs an analysis of variance on your groups
# Inputs: dat data.frame
# Outputs: aov lm object

calculate_stuff <- function(dat) {
  aov <- aov(val ~ grp, data = dat)
  return(aov)
} 
# End function -----------------------------------------------------------------


# ------------------------------------------------------------------------------
# Function: summarize_output
# Description: Get the p-value from an aov object
# Inputs: aov object
# Outputs: p-value

summarize_output <- function(aov) {
  pval <- summary(aov)[[1]][["Pr(>F)"]]
  return(pval[1])
} 
# End function -----------------------------------------------------------------


# ------------------------------------------------------------------------------
# Function: graph_results
# Description: Graph a box plot of the data frame
# Inputs: data.frame, variables to plot
# Outputs: plot

graph_results <- function(dat, x, y, g_title = NULL, x_lab = x, y_lab = y) {
  ggplot(dat, aes(x = dat[, x], dat[, y])) +
    geom_boxplot() +
    labs(title = g_title, x = x_lab, y = y_lab) +
    theme_classic()
} 
# End function -----------------------------------------------------------------


# ------------------------------------------------------------------------------
# Function: graphResults
# Description: Graph density plots of the data
# Inputs: data.frame, variables to plot
# Outputs: plot
require(wesanderson)

graphResults <- function(dat, x, g_title = NULL, x_lab = x, y_lab = "Counts",
                         legend_title = NULL,
                          colors = wes_palette("Darjeeling1")) {
  ggplot(dat, aes(x = dat[, x], fill = grp)) +
    geom_density(color = "grey50", alpha = 0.75) +
    labs(title = g_title, x = x_lab, y = y_lab) +
    scale_fill_manual(values = colors,
                      name = legend_title) +
    theme_classic()
} 
# End function -----------------------------------------------------------------