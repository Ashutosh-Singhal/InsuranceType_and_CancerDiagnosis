#PACKAGES INSTALLATION & IMPORTING LIBRARIES:
if (!"evaluate" %in% installed.packages()) {install.packages("evaluate")}
if (!"reshape" %in% installed.packages()) {install.packages("reshape")}

#if (!"data.table" %in% installed.packages()) {install.packages("data.table")}
#library(data.table)

#pkg <- "package:plyr"
#detach(pkg, character.only = TRUE)


#Libraries required
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dbplyr)
library(plyr)
library(reshape)
library(plotly)
library(readxl)
library(data.table)
library(scales)
library(gridExtra)
library(grid)
require(stringr)


#READING DATA FILES:
Crude_data1 <- read_excel("data/HCBM_Report.xlsx")

pre_post_data <- read_excel("data/Pre_post_ACA_data.xlsx") 

#Pre-post ACA data wrangling
colnames(pre_post_data)<- c("ACA","Status","S0", "SI", "SII", "SIII", "SIV", "OC", "NApp", "UNK")
pre_post_data <- as.data.frame(pre_post_data)
str(pre_post_data)
pre_post_data$itype_total <- rowSums(pre_post_data[,3:10])
pre_post_data <- pre_post_data %>%
  mutate(Stage_0 = S0/itype_total*100,
         Stage_I = SI/itype_total*100,
         Stage_II = SII/itype_total*100,
         Stage_III = SIII/itype_total*100,
         Stage_IV = SIV/itype_total*100,
         Stage_OC = OC/itype_total*100,
         Stage_NA = NApp/itype_total*100,
         Stage_UNK = UNK/itype_total*100)

pre_post_data$Status <- str_replace_all(pre_post_data$Status, "Insurance Status Unknown", "Unknown")
pre_post_data$Status <- str_replace_all(pre_post_data$Status, "Private/ Managed", "Pvt/Managed")
pre_post_data$Status <- str_replace_all(pre_post_data$Status, "Other Government", "Other/Govt")
pre_post_data$Status <- str_replace_all(pre_post_data$Status, "Not Insured", "Uninsured")
pre_post_data$Status <- str_replace_all(pre_post_data$Status, " ", "")

#View(pre_post_data)

# Master_df data wrangling--deleting Count and percentage column
Crude_data1$Count <- NULL
Crude_data1$pctg <- NULL

#renaming column names
colnames(Crude_data1)<- c("Year","Status","S0", "SI", "SII", "SIII", "SIV", "OC", "NApp", "UNK")

#removing the rows of stage_percentage distict by "NA"
row.has.na <- apply(Crude_data1, 1, function(x){any(is.na(x))})
#sum(row.has.na)
Crude_data2 <- Crude_data1[!row.has.na,]
# OC is Chr so it is needed to be converted into numeric
Crude_data2$OC <- as.numeric(Crude_data2$OC)
#converting into numeric added "NA" where there was a dot mark, in row 59, column 8, so, converting NA to 0
Crude_data2[is.na(Crude_data2)] <- 0
print(Crude_data2$Status)
Crude_data2$Status <- str_replace_all(Crude_data2$Status, "Insurance Status Unknown", "Unknown")
Crude_data2$Status <- str_replace_all(Crude_data2$Status, "Private/ Managed", "Pvt/Managed")
Crude_data2$Status <- str_replace_all(Crude_data2$Status, "Other Government", "Other/Govt")
Crude_data2$Status <- str_replace_all(Crude_data2$Status, "Not Insured", "Uninsured")
Crude_data2$Status <- str_replace_all(Crude_data2$Status, " ", "")
print(Crude_data2$Status)
# Adding some importnat calculations into the data
Crude_data2$itype_total <- rowSums(Crude_data2[,3:10])

Crude_data3 <- Crude_data2 %>%
  mutate(Stage_0 = S0/itype_total*100,
         Stage_I = SI/itype_total*100,
         Stage_II = SII/itype_total*100,
         Stage_III = SIII/itype_total*100,
         Stage_IV = SIV/itype_total*100,
         Stage_OC = OC/itype_total*100,
         Stage_NA = NApp/itype_total*100,
         Stage_UNK = UNK/itype_total*100)

#class(Crude_data3) #Converting into dataframe
Crude_data3 <- as.data.frame(Crude_data3)

# Year-wise calculating total number of all type of insurance status and percentage of each insurance type
year_2007 <- Crude_data3 %>%
  filter(Year==2007)
year_2007$sum_itype_total = sum(year_2007$itype_total)
year_2007$itype_pctg = year_2007$itype_total/year_2007$sum_itype_total*100

year_2008 <- Crude_data3 %>%
  filter(Year==2008)
year_2008$sum_itype_total = sum(year_2008$itype_total)
year_2008$itype_pctg = year_2008$itype_total/year_2008$sum_itype_total*100

year_2009 <- Crude_data3 %>%
  filter(Year==2009)
year_2009$sum_itype_total = sum(year_2009$itype_total)
year_2009$itype_pctg = year_2009$itype_total/year_2009$sum_itype_total*100

year_2010 <- Crude_data3 %>%
  filter(Year==2010)
year_2010$sum_itype_total = sum(year_2010$itype_total)
year_2010$itype_pctg = year_2010$itype_total/year_2010$sum_itype_total*100

year_2011 <- Crude_data3 %>%
  filter(Year==2011)
year_2011$sum_itype_total = sum(year_2011$itype_total)
year_2011$itype_pctg = year_2011$itype_total/year_2011$sum_itype_total*100

year_2012 <- Crude_data3 %>%
  filter(Year==2012)
year_2012$sum_itype_total = sum(year_2012$itype_total)
year_2012$itype_pctg = year_2012$itype_total/year_2012$sum_itype_total*100

year_2013 <- Crude_data3 %>%
  filter(Year==2013)
year_2013$sum_itype_total = sum(year_2013$itype_total)
year_2013$itype_pctg = year_2013$itype_total/year_2013$sum_itype_total*100

year_2014 <- Crude_data3 %>%
  filter(Year==2014)
year_2014$sum_itype_total = sum(year_2014$itype_total)
year_2014$itype_pctg = year_2014$itype_total/year_2014$sum_itype_total*100

year_2015 <- Crude_data3 %>%
  filter(Year==2015)
year_2015$sum_itype_total = sum(year_2015$itype_total)
year_2015$itype_pctg = year_2015$itype_total/year_2015$sum_itype_total*100

year_2016 <- Crude_data3 %>%
  filter(Year==2016)
year_2016$sum_itype_total = sum(year_2016$itype_total)
year_2016$itype_pctg = year_2016$itype_total/year_2016$sum_itype_total*100

# Combining all years into one
master_df <- rbind(year_2007, year_2008, year_2009, year_2010, year_2011, year_2012, year_2013, year_2014, year_2015, year_2016)
master_df$Status <- as.factor(master_df$Status)
master_df$Year <- as.factor(master_df$Year)

#saveRDS(master_df, file= "master_df.RDS")

#=====Selection of Type of insurance and percentage of cancer diagnosis stage
master_df$Status <- as.character(master_df$Status)

stage_itype_year1 <- master_df %>%
  select(1, 2, 12:19)

stage_itype_year_melt1 <- melt(stage_itype_year1, id=c("Year", "Status"))

# Year-wise distribution of stages of cancer--Preparing for selection of two variables: Year & Stage

# Filter == Stage and get year-wise distribution of that stage & compare in different type of insurance.
facet_allyear_plt <- stage_itype_year_melt1 %>%
group_by(Year, Status) %>%
  filter(variable == "Stage_I") %>% 
  ggplot(aes(x= Year, y = value, shape = factor(Status)), group=1) +
  geom_point(aes(colour = factor(Status)), group=1, size=4)+
  geom_line(aes(colour = factor(Status)), group=1)+
  facet_grid(~Status, scales = "free")+
  ggtitle("Year-wise change in diagnosis of cancer stages")+
  xlab("Year") + 
  ylab("% Cancer Stage Diagnosed")+
  theme_linedraw(base_size = 16)+
  theme(
    legend.position="none",
    plot.title = element_text(color="red", size=18, face="bold", hjust=0.5),
    axis.text.x = element_text(face="bold",angle=90)
    
  )
facet_allyear_plt

# Alternate of dot plot
stage_itype_year_bar1 <- stage_itype_year_melt1 %>% 
  filter(str_detect(Status, "Uninsured")) %>% 
  group_by(Year, Status, variable)%>%
  ggplot(aes(x=Year, y=value)) +
  geom_col(aes(fill=variable), width = 0.9)+ 
  theme_classic()

ggplotly(stage_itype_year_bar1)

# Filter Year and get Insurance type based % of cancer stages in stacked bar ---Mosaic replacement
itype_year_bar <- stage_itype_year_melt1 %>% 
  filter(Year == 2016) %>% 
  group_by(Year, Status, variable)%>%
  ggplot(aes(x=Status, y=value)) +
  geom_col(aes(fill=variable), width = 0.9)+ 
  theme_classic()

ggplotly(itype_year_bar)

#+++++++++++++++++++++++++++++++++
View(pre_post_data)

saveRDS(pre_post_data, file= "pre_post_data.RDS")

# PRE-ACA
pre_post_data_melt <- pre_post_data %>%
  select(1, 2, 12:19)

pre_post_data_melt1 <- melt(pre_post_data_melt, id=c("ACA", "Status"))

colnames(pre_post_data_melt1) <- c("ACA", "Status", "Stage", "Percentage")

pre_post_data_spread <- pre_post_data_melt1 %>% 
  spread(ACA, Percentage)

View(pre_post_data_melt1)
# filter(Stage == "Stage_0" | 
#          Stage == "Stage_I" |
#          Stage == "Stage_II" | 
#          Stage == "Stage_III" | 
#          Stage == "Stage_IV" ) %>% 

pre_post_data_bar <- pre_post_data_spread %>% 
  filter(str_detect(Status, "Medicaid")) %>% 
  ggplot(aes(x=Stage, y=post_ACA-pre_ACA)) +
  geom_bar(stat = "identity", aes(fill= Stage), width = 0.5) +
  facet_wrap(~Status) +
  coord_flip() +
  ylab("% of Cases after ACA")
pre_post_data_bar


pre_post_data_lolipop <- pre_post_data_spread %>% 
  filter(str_detect(Status, "Medicaid")) %>% 
  ggplot() +
  geom_segment(aes(x=Stage, xend=Stage, y=pre_ACA, yend=post_ACA), color="grey") +
  geom_point(aes(x=Stage, y=post_ACA), color=rgb(0.2,0.7,0.1,0.5), size=5) +
  geom_point(aes(x=Stage, y=pre_ACA), color= rgb(0.7,0.2,0.1,0.5), size=5) +
  coord_flip() +
  ylab("Percentage")
# +
#   facet_wrap(~Status)
ggplotly(pre_post_data_lolipop)
# lolipop <- pre_post_data_lolipop +
#   annotate("text", x = grep("Stage I", pre_post_data_spread$Stage), 
#            y = pre_post_data_spread$pre_ACA[which(pre_post_data_spread$Stage=="Stage_I")]*1.2, 
#            label = "Stage I is very impressive", 
#            color="orange", size=4 , angle=0, fontface="bold", hjust=0) 
#   lolipop

# Table
pre_post_data_table <- pre_post_data_spread %>% 
  filter(str_detect(Status, "Medicaid")) %>% 
  select(Stage, post_ACA, pre_ACA) %>%
  mutate(After_ACA = post_ACA - pre_ACA)

pre_post_data_table


# POST-ACA new bar graph
pre_ACA_bar <- pre_post_data_melt1 %>%
 filter(str_detect(ACA, "pre_ACA")) %>% 
  group_by(Status, Stage) %>%
  ggplot(aes(x=Status, y= Percentage)) +
  geom_col(aes(fill=Stage), width = 0.9)+ 
  theme_classic()
ggplotly(pre_ACA_bar)

#POST-ACA new bar graph
post_ACA_bar <- pre_post_data_melt1 %>%
  filter(str_detect(ACA, "post_ACA")) %>% 
  group_by(Status, Stage) %>%
  ggplot(aes(x=Status, y= Percentage)) +
  geom_col(aes(fill=Stage), width = 0.9)+ 
  theme_classic()
ggplotly(post_ACA_bar)

#+++++++++++++
#Five year survival plot
Stage <- c("Stage 0 & I", "Stage II", "Stage III", "Stage IV")
rate <- c("100",  )

# Y2K7_2K9 <- master_df %>% 
#   filter(Year == 2007 | Year == 2008 | Year == 2009 ) %>% 
#   filter(str_detect(Status, "Uninsured")) %>%
#   select(3:21) %>% 
#   colSums(na.rm = FALSE, dims = 1)
#   
# Y2K7_2K9 <- as.data.frame(Y2K7_2K9)
# 
# colnames(Y2K7_2K9)<- c("Uninsured")

#pre_post_ACA <- grid.arrange(pre_ACA_1, post_ACA_1, ncol = 2, nrow = 1)


###########  MOSAIC CHART ##########
#==== MOSAIC CHART  =====
Y2007 <- master_df %>%
  select(1, 2, 12:19, 21) %>%
  filter(Year == 2007) %>%
  arrange(desc(itype_pctg)) %>%
  mutate(xmax = cumsum(itype_pctg), xmin = xmax - itype_pctg)

##adding a few helper variables, converting into dataframe, and changing the important columns into Vectors before melting.
##Calculating cumulative width on x-axes, starting point of each column.
#Y2007 <- as.data.frame(Y2007)
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

#Mosaic Plot, finally
theme_set(theme_classic(base_size = 16))
mosaic_plot <- ggplot(Y2007mx, aes(ymin = ymin, ymax = ymax,
                                   xmin = xmin, xmax = xmax, fill = variable)) +
  geom_rect(color = I("grey")) +
  geom_text(aes(x = xtext, y = ytext,
                label = ifelse(Status == "Medicare", paste(variable, " - ", round(value, digits = 0), "%", sep = ""), 
                               paste(round(value, digits = 0), "%", sep = ""))), size = 3.5) +
  geom_text(aes(x = xtext, y = 100,
                label = paste(Status)), size = 4, angle=90, hjust = 0) +
  theme(plot.margin=unit(c(2,1,1,1),"cm"))+
  #coord_cartesian(ylim = c(0, 130))+
  coord_fixed(ratio=0.75, xlim = c(0, 100), ylim=c(0, 130))+
  theme_classic() +
  ggtitle(paste("Stage of cancer diagnosis by the status of insurance"), master_df$Year)+
  xlab("Type of Health Insurance (%)") + 
  ylab("Cancer Stage Diagnosed (%)")+
  labs(fill = "% of Cancer Stage")+
  theme(legend.position="bottom",
        plot.title = element_text(color="red", size=14, face="bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(face="bold",angle=0)
  ) # closing mosaic plot

mosaic_plot

#======== Extra  =====#===

# stage_df <- Crude_data2 %>%
#   group_by(Year) %>%
#   mutate(Stage_0 = S0/sum(S0)*100,
#          Stage_I = SI/sum(SI)*100,
#          Stage_II = SII/sum(SII)*100,
#          Stage_III = SIII/sum(SIII)*100,
#          Stage_IV = SIV/sum(SIV)*100,
#          Stage_OC = OC/sum(OC)*100,
#          Stage_NA = NApp/sum(NApp)*100,
#          Stage_UNK = UNK/sum(UNK)*100)

# stage_df1 <- stage_df %>%
#   select(1, 2, 12:19) %>%
#   group_by(Year, Status)

# stage_df1_melt <- melt(stage_df1, id=c("Year", "Status"))


#OVERALL X = Stages y = status

# all_years_stages <- stage_df1_melt %>%
#   arrange(desc(Year)) %>%
#   ggplot(aes(x=variable, y=value)) +
#   geom_col(aes(fill=Status), width = 0.9)+ 
#   theme_classic()
# 
# ggplotly(all_years_stages)

# # PRE-ACA
# pre_ACA_stages <- stage_df1_melt %>%
#   filter(Year == 2007:2010) %>%
#   arrange(desc(Year)) %>%
#   ggplot(aes(x=variable, y=value)) +
#   geom_col(aes(fill=Status), width = 0.9)+ 
#   theme_classic()
# 
# ggplotly(pre_ACA_stages)
# 
# # Post-ACA
# post_ACA_stages <- stage_df1_melt %>%
#   filter(Year == 2011:2016)%>%
#   arrange(desc(Year)) %>%
#   ggplot(aes(x=variable, y=value)) +
#   geom_col(aes(fill=Status), width = 0.9)+ 
#   theme_classic()
# 
# ggplotly(post_ACA_stages)
# 
# pre_ACA_post_ACA <- grid.arrange(pre_ACA_stages, post_ACA_stages, ncol = 2, nrow = 1)
# pre_ACA_post_ACA


#


# stage_graph <- stage_df1_melt %>%
#   group_by(Status)%>%
#   ggplot(aes(x=variable, y=value)) +
#   geom_col(aes(fill=Status), width = 0.9)+ 
#   theme_classic()
# 
# ggplotly(stage_graph)
# 
# stage_box <- stage_df1_melt %>%
#   group_by(Status)%>%
#   ggplot(aes(x=variable, y=value)) +
#   geom_boxplot(aes(color=Status))+ 
#   theme_classic()
# ggplotly(stage_box)

#ggplotly(stage_line)

# Year-wise Percentage of diagnosed cases and insurance type
# line_itypePctg_allyears <- master_df %>%
#   select(1, 2, 11, 20, 21) %>%
#   ggplot(aes(x=Year, y=itype_pctg, color=Status)) +
#   geom_line(size=1.5)+
#   geom_point(size=2)+
#   scale_x_continuous(breaks=seq(2007,2016,1))+
#   scale_y_continuous(labels=comma)+
#   theme_classic()
# ggplotly(line_itypePctg_allyears)
# 
# bar_itypePctg_allyears <- master_df %>%
#   select(1, 2, 11, 20, 21) %>%
#   ggplot(aes(x=Year, y=itype_pctg, fill=Status)) +
#   geom_bar(stat = "identity")+
#   scale_x_continuous(breaks=seq(2007,2016,1))+
#   theme_classic()
# ggplotly(bar_itypePctg_allyears)
# 
# # Year-wise Number of diagnosed cases and insurance type
# line_itypeNum_allyears <- master_df %>%
#   select(1, 2, 11, 20, 21) %>%
#   ggplot(aes(x=Year, y=itype_total, color=Status)) +
#   geom_line(size=1.5)+
#   geom_point(size=2)+
#   scale_y_continuous(labels=comma)+
#   theme_classic()
# # facet_grid(~Year)
# ggplotly(line_itypeNum_allyears)
# 
# bar_itypeNum_allyears <- master_df %>%
#   select(1, 2, 11, 20, 21) %>%
#   ggplot(aes(x=Year, y=itype_total)) +
#   geom_col(aes(fill=Status))+
#   scale_y_continuous(labels=comma)+
#   theme_classic()
# # facet_grid(~Year)
# ggplotly(bar_itypeNum_allyears)


# combined <- grid.arrange(line_itypeNum_allyears, bar_itypeNum_allyears, line_itypePctg_allyears, bar_itypePctg_allyears, 
#              ncol = 2, nrow = 2)
# combined