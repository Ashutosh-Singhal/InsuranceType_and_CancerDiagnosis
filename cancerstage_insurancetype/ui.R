# Ashutosh


#SIDE BAR 1
side_bar_1 <- sidebarLayout(
  
  sidebarPanel(width = 2,
    selectInput("year", label="Select Year:", choices = year, selected = "2007")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Barplot", plotlyOutput("itype_year_plt", width = "100%", height = 600)) 
      # tabPanel("Summary", wellPanel(p("Add text here"))
      #)
    )
  )
)#closing sidebar1


#SIDE BAR 2
side_bar_2 <- sidebarLayout(

  sidebarPanel(width = 2,
    selectInput(
                "stage",
                label="Select Stage of Cancer:",
                choices = c("0 : Abnormal Cells" = "Stage_0",
                            "I : Localized" = "Stage_I",
                            "II : Large & Lymph Nodes" = "Stage_II",
                            "III : Larger & Spreaded" = "Stage_III",
                            "IV : Advanced/Metastatic" = "Stage_IV",
                            "OC : Other Category" = "Stage_OC",
                            "NA: Not Applicable" = "Stage_NA",
                            "UNK: Unknown" = "Stage_UNK"),
                multiple = FALSE,
                selected = "Stage_I")

  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Lineplot",  plotOutput("allyear_plt", width = 1100, height = 600))
      # tabPanel("Summary",
      #          wellPanel(p("test"))
      # )
    )
  )
)#closing sidebar2


# SIDE BAR 3
side_bar_3 <- sidebarLayout(
  sidebarPanel(width = 2,
    selectInput("status", label="Select Insurance Type:", choices = status, selected = "Medicaid")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cleveland Plot", plotlyOutput("difference_plot_lolipop", width = "100%", height = 600)),
        tabPanel("Barplot", plotlyOutput("difference_barplot", width = "100%", height = 600))
        # tabPanel("Summary", wellPanel(p("Add text here")))
        
        )
  )
)#closing sidebar3

#

#Themes selection: cerulean, cosmo, #cyborg, darkly, flatly, 
# lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.

header <- dashboardHeader(title = "Cancer Diagnosis & Health Insurance", titleWidth = 350)

ui <- navbarPage(header,
                
            theme = shinytheme("cyborg"),
            tabPanel("Year",
                          side_bar_1),
            tabPanel("Stage", 
                      # h4("Cancer stage diagnosis: 2007-2016"),
                          side_bar_2),
            tabPanel("Affordable Care Act (ACA)", 
                     # h4("Add text here"),
              mainPanel(
                  tabsetPanel(
                    tabPanel("Pre-ACA", plotlyOutput("pre_ACA_plt", width = 1200, height = 600)), 
                    tabPanel("Post-ACA", plotlyOutput("post_ACA_plt", width = 1200, height = 600)) 
                    # tabPanel("Summary", wellPanel(p("Add text here")))
                   )
                 )),
            
            tabPanel("After ~10 years",
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Year : 2007", plotOutput("mosaic_plot", width = 1200, height = 600)), 
                         tabPanel("Year : 2016", plotOutput("mosaic_plot2016", width = 1200, height = 600))
                         # tabPanel("Summary", wellPanel(p("Add text here")))
                         
                       )#tabsetPanel
                     )),         
            
            tabPanel("Difference After ACA", 
                     side_bar_3) # Closing the final page
          
                  
) # Closing navbar page of the UI


# Extra: if needed to add extra material in the end
# navbarMenu("More",
#            tabPanel("Sub-Component A"),
#            ("------"),
#            "Section header",
#           tabPanel("Sub-Component B"))


