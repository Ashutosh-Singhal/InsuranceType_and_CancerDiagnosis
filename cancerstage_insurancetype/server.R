#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
#all_year plot
  output$allyear_plt <- renderPlot({
      stage_itype_year_melt1 %>%
      group_by(Year, Status) %>%
      filter(variable == input$stage) %>% 
      ggplot(aes(x= Year, y = value, shape = factor(Status)), group=1) +
      geom_point(aes(colour = factor(Status)), group=1, size=4)+
      geom_line(aes(colour = factor(Status)), group=1)+
      facet_grid(~Status, scales = "free")+
      ggtitle("Diagnosis of Cancer Stages from 2007-2016")+
      xlab("") + 
      ylab("% Patients")+
      theme_bw(base_size = 24)+
      theme(
        #panel.grid = element_blank(),
        legend.position="none",
        plot.title = element_text(color="black", size=30, face="bold", hjust=0.5),
        axis.text.x = element_text(size=18, face="bold", angle=90),
        axis.text.y = element_text(size=20, face="bold")
        
      )# closing ggplot
  
  })# closing renderplot all_year plot
  
#++++++++ itype_year_plt
  output$itype_year_plt <- renderPlotly({
    stage_itype_year_melt1 %>% 
      filter(Year == input$year) %>% 
      group_by(Year, Status, variable)%>%
      ggplot(aes(x=Status, y=value)) +
      geom_col(aes(fill=variable), width = 0.8)+ 
      xlab("Type of Insurance") +
      ylab("% Patient Diagnosed") +
      labs(fill = "Cancer Stage")+
      theme_bw(base_size = 20, base_family = "helvetica")
    })# closing renderplot itype_year_plt
  
#+++++++++ OBAMA-CARE Plots
  output$pre_ACA_plt <- renderPlotly({
    pre_post_data_melt1 %>%
      filter(str_detect(ACA, "pre_ACA")) %>% 
      group_by(Status, Stage) %>%
      ggplot(aes(x=Status, y= Percentage)) +
      geom_col(aes(fill=Stage), width = 0.8)+ 
      theme_bw(base_size = 20, base_family = "helvetica")
  })# closing renderplot pre_ACA_plt
  
  output$post_ACA_plt <- renderPlotly({
    pre_post_data_melt1 %>%
      filter(str_detect(ACA, "post_ACA")) %>% 
      group_by(Status, Stage) %>%
      ggplot(aes(x=Status, y= Percentage)) +
      geom_col(aes(fill=Stage), width = 0.8)+ 
      theme_bw(base_size = 20, base_family = "helvetica")
  })# closing renderplot post_ACA_plt
  
  output$pre_post_ACA_plt <- renderPlotly({
    pre_post_data_spread %>% 
      #filter(str_detect(Status, "Medicaid")) %>% 
      ggplot() +
      geom_segment(aes(x=Stage, xend=Stage, y=pre_ACA, yend=post_ACA), color="grey") +
      geom_point(data = pre_post_data_melt1, aes(x=Stage, y=Percentage, color= ACA), size=5) +
      #geom_point(data = pre_post_data_melt1, aes(x=Stage, y=pre_ACA), color= "gold", size=5) +
      facet_wrap(~Status)+
      coord_flip() +
      theme_bw(base_size = 20, base_family = "helvetica") +
      ylab("Percentage")+
      xlab("")
      
  })# closing renderplot pre_post_ACA_plt (lolipop)
  
  
  output$pre_post_ACA_barplt <- renderPlotly({
    pre_post_data_spread %>% 
      #filter(str_detect(Status, "Medicaid")) %>% 
      ggplot(aes(x=Stage, y=post_ACA-pre_ACA)) +
      geom_bar(stat = "identity", aes(fill= Stage), width = 0.9) +
      facet_wrap(~Status) +
      coord_flip()+
      theme_bw(base_size = 16, base_family = "helvetica") +
      ylab("% of Cases after ACA")+
      xlab("")
  
  
  })# closing renderplot pre_post_ACA_barplt
  
  
  
#++++++++ Mosaic plot  
  output$mosaic_plot <- renderPlot({
  # Mosaic plot
    ggplot(Y2007mx, aes(ymin = ymin, ymax = ymax,
                        xmin = xmin, xmax = xmax, fill = variable)) +
      geom_rect(color = I("grey")) +
      geom_text(aes(x = xtext, y = ytext,
                    label = ifelse(Status == "Medicare", paste(variable, " - ", round(value, digits = 0), "%", sep = ""), 
                                   paste(round(value, digits = 0), "%", sep = ""))), size = 4) +
      geom_text(aes(x = xtext, y = 100,
                    label = paste(Status)), size = 6, angle=90, hjust = 0) +
      #theme(plot.margin=unit(c(2,1,1,1),"cm"))+
      #coord_cartesian(ylim = c(0, 130))+
      coord_fixed(ratio=0.5, xlim = c(0, 100), ylim=c(0, 130))+
      theme_classic() +
      ggtitle("Stage of cancer diagnosis by the status of insurance")+
      xlab("Type of Health Insurance (%)") + 
      ylab("% of Cancer Stage Diagnosed")+
      labs(fill = "Cancer Stage")+
      theme(legend.position="right",
            plot.title = element_text(color="black", size=25, hjust=0.5),
            axis.title.x = element_text(color="black", size=18),
            axis.title.y = element_text(color="black", size=18),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0),
            legend.text = element_text(size=16),
            legend.title = element_text(size=18)
           
      )# closing ggplot mosaic
    
  })#closing renderplot-mosaic
  
})# Closing ShinyServer

#

