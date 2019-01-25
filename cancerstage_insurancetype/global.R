# Ashutosh
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(scales)
library(reshape)
library(magrittr)
require(stringr)
library(shiny)
library(shinythemes)

# READING DATA+++++++++++++++++++++++++++++++++++++++++
master_df <- readRDS("./data/master_df.RDS")
master_df <- as.data.frame(master_df)
master_df$Status <- as.character(master_df$Status)

pre_post_data <- readRDS("./data/pre_post_data.RDS")
pre_post_data <- as.data.frame(pre_post_data)

pre_post_data_new <- readRDS("./data/pre_post_data_new.RDS")
pre_post_data_new <- as.data.frame(pre_post_data_new)

pre_post_data_lolipop <- pre_post_data_new %>% mutate(Post_ACA_difference = round(post_ACA - pre_ACA))

# GRAPHS++++++++++++++++++++++++++++++++++++++++++++++++++++++
# All insurance type and year-wise Facet plot (stage drop-down)
# stage_itype_year1 <- master_df %>%
#   select(1, 2, 12:19) 
# 
# stage_itype_year_melt1 <- melt(stage_itype_year1, id=c("Year", "Status"))


stage_itype_year_melt1 <- master_df %>%
  select(1, 2, 12:19) %>% 
  melt(id=c("Year", "Status"))

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
Y2007mx <- plyr::ddply(Y2007mx, .(Status), transform, ymin = ymax - value)
#Setting up the position of the text 
Y2007mx$xtext <- with(Y2007mx, xmin + (xmax - xmin)/2)
Y2007mx$ytext <- with(Y2007mx, ymin + (ymax - ymin)/2)

#2016 Mosaid plot
Y2016 <- master_df %>%
  select(1, 2, 12:19, 21) %>%
  filter(Year == 2016) %>%
  arrange(desc(itype_pctg)) %>%
  mutate(xmax = cumsum(itype_pctg), xmin = xmax - itype_pctg)
Y2016$Status <- as.vector(Y2016$Status)
Y2016$xmax <- as.vector(Y2016$xmax)
Y2016$xmin <- as.vector(Y2016$xmin)
# Removing extra columns before melting
Y2016$itype_pctg <- NULL
Y2016$Year <- NULL
Y2016m <- melt(Y2016, id = c("Status", "xmin", "xmax"))
#Unlisting the columns and calculating the ymax and ymin by ddply function
Y2016m$xmin <- unlist(Y2016m$xmin)
Y2016m$xmax <- unlist(Y2016m$xmax)
Y2016m$Status <- unlist(Y2016m$Status)
#Now we need to determine how the columns are stacked and where to position the text labels.
Y2016mx <- plyr::ddply(Y2016m, .(Status), transform, ymax = cumsum(value))
Y2016mx <- plyr::ddply(Y2016mx, .(Status), transform, ymin = ymax - value)
#Setting up the position of the text 
Y2016mx$xtext <- with(Y2016mx, xmin + (xmax - xmin)/2)
Y2016mx$ytext <- with(Y2016mx, ymin + (ymax - ymin)/2)

# DROPDOWN MENU +++++++++++++++++++++++++++++++++++++++++++++
# Insurance Status
status <- pre_post_data_new %>% 
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



# Making new data frame as Status column in pre_post_data_spread df is not filtering the content
# b <- pre_post_data_spread %>% 
#   select(1:4)
# bx <- data.frame("test" = 1:48)
# bz <- cbind(b, bx [!names(bx) %in% names(b)])
# bz$test[which(bz$test==1:8)] <- "Medicaid"
# bz$test[which(bz$test==9:16)] <- "Medicare"
# bz$test[which(bz$test==17:24)] <- "Other/Govt"
# bz$test[which(bz$test==25:32)] <- "Pvt/Managed"
# bz$test[which(bz$test==33:40)] <- "Uninsured"
# bz$test[which(bz$test==41:48)] <- "Unknown"

# pre_post_data_new <- bz %>% select(2:5)
# pre_post_data_new$Status <- pre_post_data_new$test

#saveRDS(pre_post_data_new, file= "pre_post_data_new.RDS")
# b$Status[sample(31, 1)] <- 5



# test_plot <- pre_post_data_lolipop %>% 
#   #filter(Status == "Medicaid") %>% 
#   ggplot(aes(x=Stage, y=Post_ACA_difference, label=Post_ACA_difference)) + 
#   geom_point(aes(x=Stage, y=Post_ACA_difference), stat='identity', color= "deeppink4", size=10)  +
#   geom_segment(aes(y = 0, 
#                    x = Stage, 
#                    yend = Post_ACA_difference, 
#                    xend = Stage), 
#                color = "deeppink4") +
#   #geom_text(color="white", size=2) +
#   labs(y = "% of Patient") + 
#   ylim(-10, 10) +
#   coord_flip() +
#   theme_bw()+
#   facet_wrap(~ Status)
# test_plot


