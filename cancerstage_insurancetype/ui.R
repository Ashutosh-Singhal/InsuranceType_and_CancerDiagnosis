# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

side_bar_1 <- sidebarLayout(
  
  sidebarPanel(
    selectInput(width = 350, 
                "stage", 
                label="Select Stage of Cancer:", 
                choices = c("0" = "Stage_0",
                            "I" = "Stage_I",
                            "II" = "Stage_II",
                            "III" = "Stage_III",
                            "IV" = "Stage_IV",
                            "OC" = "Stage_OC",
                            "NA" = "Stage_NA",
                            "UNK" = "Stage_UNK"),
                multiple = FALSE,
                selected = "Stage_0")
    
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Summary",
               wellPanel(p("test"))
      ) 
    )
  )
)#closing sidebar1

side_bar_2 <- sidebarLayout(
  
  sidebarPanel(
    selectInput("year", label="Select Year:", choices = year, selected = "2007")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", 
               wellPanel(p("test"))
               )
    )
  )
)#closing sidebar2

#Themes selection: cerulean, cosmo, cyborg, darkly, flatly, 
# lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.

header <- dashboardHeader(title = "Cancer Diagnosis & Health Insurance", titleWidth = 350)

ui <- navbarPage(header,
                theme = shinytheme("slate"),
                 
                 tabPanel("Year",
                          side_bar_2, 
                          plotlyOutput("itype_year_plt", width = 1000, height = 600)),
                
                tabPanel("Stage", 
                         side_bar_1, 
                         plotOutput("allyear_plt", width = 1200, height = 600)),
                
                 tabPanel("Affordable Care Act (ACA)", 
                          mainPanel(
              tabsetPanel(
                     tabPanel("Pre-Affordable Care Act", width = 12, plotlyOutput("pre_ACA_plt", width = 1000, height = 600)), 
                     tabPanel("Post-Affordable Care Act", plotlyOutput("post_ACA_plt", width = 1000, height = 600)), 
                     tabPanel("Pre- Vs Post-Affordable Care Act", plotlyOutput("pre_post_ACA_plt", width = 1000, height = 600)),
                     tabPanel("Difference", plotlyOutput("pre_post_ACA_barplt", width = 1000, height = 600))
                  
  
                   )
                 )),
                 tabPanel("Mosaic Plot", plotOutput("mosaic_plot", width = 1600, height = 600))
                 # navbarMenu("More",
                 #            tabPanel("Sub-Component A"),
                 #            tabPanel("Sub-Component B"))
)


#, tableOutput("table")
# , verbatimTextOutput("summary")
# , plotOutput("plot")

