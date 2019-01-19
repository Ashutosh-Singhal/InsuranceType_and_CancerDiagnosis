library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(reshape)
library(magrittr)
library(plyr)
library(gridExtra)
library(grid)
require(stringr)



# loading datset
master_df <- readRDS("./data/master_df.RDS")
master_df <- as.data.frame(master_df)
master_df$Status <- as.character(master_df$Status)

pre_post_data <- readRDS("./data/pre_post_data.RDS")
pre_post_data <- as.data.frame(pre_post_data)


# DROPDOWN MENU ++++++++++++++++++++++

# Insurance Status
status <- master_df %>% 
  select(Status) %>% 
  unique()

# Year
year <- master_df %>% 
  select(Year) %>% 
  unique()

# Stage of the cancer

stage <- stage_itype_year_melt1 %>% 
  select(variable) %>% 
  unique()

# Graphs++++++++++++++++++++++++++++++

# All insurance type and year-wise Facet plot (stage drop-down)
stage_itype_year1 <- master_df %>%
  select(1, 2, 12:19)
stage_itype_year_melt1 <- melt(stage_itype_year1, id=c("Year", "Status"))

# Pre-ACA/post-ACA graph: Data frame manipulations
pre_post_data_melt <- pre_post_data %>%
  select(1, 2, 12:19)

pre_post_data_melt1 <- melt(pre_post_data_melt, id=c("ACA", "Status"))

colnames(pre_post_data_melt1) <- c("ACA", "Status", "Stage", "Percentage")

pre_post_data_spread <- pre_post_data_melt1 %>% 
  spread(ACA, Percentage)


# Mosaic graph : Manipulation in master_df
Y2007 <- master_df %>%
  select(1, 2, 12:19, 21) %>%
  filter(Year == 2007) %>%
  arrange(desc(itype_pctg)) %>%
  mutate(xmax = cumsum(itype_pctg), xmin = xmax - itype_pctg)
Y2007$Status <- as.vector(Y2007$Status)
Y2007$xmax <- as.vector(Y2007$xmax)
Y2007$xmin <- as.vector(Y2007$xmin)
# Removing extra columns before melting
Y2007$itype_pctg <- NULL
Y2007$Year <- NULL
Y2007m <- melt(Y2007, id = c("Status", "xmin", "xmax"))
#Unlisting the columns and calculating the ymax and ymin by ddply function
Y2007m$xmin <- unlist(Y2007m$xmin)
Y2007m$xmax <- unlist(Y2007m$xmax)
Y2007m$Status <- unlist(Y2007m$Status)
#Now we need to determine how the columns are stacked and where to position the text labels.
Y2007mx <- plyr::ddply(Y2007m, .(Status), transform, ymax = cumsum(value))

Y2007mx <- plyr::ddply(Y2007mx, .(Status), transform,
                       ymin = ymax - value)
#Setting up the position of the text 
Y2007mx$xtext <- with(Y2007mx, xmin + (xmax - xmin)/2)
Y2007mx$ytext <- with(Y2007mx, ymin + (ymax - ymin)/2)

  
#

