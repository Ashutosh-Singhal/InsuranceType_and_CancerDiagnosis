# Ashutosh

shinyServer(function(input, output) {
   
#++++++++ Page:1, FIGURE 1: Select Stage from drop down, get percentage of that stage from Year 2007-2016
  output$itype_year_plt <- renderPlotly({
    stage_itype_year_melt1 %>% 
      filter(Year == input$year) %>% 
      group_by(Year, Status, variable)%>%
      ggplot(aes(x=Status, y=value)) +
      geom_col(aes(fill=variable),  color="black", width = 0.8)+ 
      labs(fill = "Cancer Stage", x= "Type Of Health Insurance", y= "% Cancer Stage Diagnosed", caption="Data Source: NCDB")+
      theme_bw(base_size = 16, base_family = "helvetica") +
      theme(
        plot.caption=element_text(size=15, face= "italic"),
        legend.title=element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        plot.margin = unit(c(4, 1, 3, 8), "lines") 
      )# Closing ggplot
    })# closing Figure 1
  
 #++++++++ Page: 2, Select YEAR from drop down, get percentage of all cancer stage in all Insurance type
  output$allyear_plt <- renderPlot({
    stage_itype_year_melt1 %>%
      group_by(Year, Status) %>%
      filter(variable == input$stage) %>% 
      ggplot(aes(x= Year, y = value, shape = factor(Status)), group=1) +
      geom_point(aes(colour = factor(Status)), group=1, size=6)+
      geom_line(aes(colour = factor(Status)), group=1, size= 2)+
      facet_wrap(~Status, scales = "free")+
      labs(title="Cancer Stage Diagnosis By Health Insurance Type: 2007-2017", 
           y="% Cancer Stage Diagnosed", 
           x="Year of Diagnosis", 
           caption="Data Source: NCDB")+
      theme_bw(base_size = 24)+
      theme(
        plot.caption=element_text(size=15, face= "italic"),
        legend.position="none",
        plot.title = element_text(color="black", size=20, face="bold", hjust=0.5, lineheight=2),
        axis.text.x = element_text(size=18, face="bold", angle=90),
        axis.text.y = element_text(size=20, face="bold")
        
      )# closing ggplot
    
  })# closing Figure 2
  
#+++++++++ Page 3: AFFORDABLE CARE ACT 
  output$pre_ACA_plt <- renderPlotly({
    pre_post_data_melt1 %>%
      filter(str_detect(ACA, "pre_ACA")) %>% 
      group_by(Status, Stage) %>%
      ggplot(aes(x=Status, y= Percentage)) +
      geom_col(aes(fill=Stage), color="black", width = 0.8)+ 
      theme_bw(base_size = 18, base_family = "helvetica")+
      labs(x = "Type Of Health Insurance", y = "% Cancer Stage Diagnosed", fill = "Cancer Stage")+
      theme(legend.position="right",
            legend.title=element_blank(),
            axis.title.x = element_text(color="black", size=18),
            axis.title.y = element_text(color="black", size=18),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0),
            legend.text = element_text(size=16),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
      )
  })# closing renderplot pre_ACA_plt
  
  output$post_ACA_plt <- renderPlotly({
    pre_post_data_melt1 %>%
      filter(str_detect(ACA, "post_ACA")) %>% 
      group_by(Status, Stage) %>%
      ggplot(aes(x=Status, y= Percentage)) +
      geom_col(aes(fill=Stage), color="black", width = 0.8)+ 
      theme_bw(base_size = 18, base_family = "helvetica")+
      labs(x = "Type Of Health Insurance", y = "% Cancer Stage Diagnosed", fill = "Cancer Stage")+
      theme(legend.position="right",
            legend.title=element_blank(),
            axis.title.x = element_text(color="black", size=18),
            axis.title.y = element_text(color="black", size=18),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0),
            legend.text = element_text(size=16),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
      )
    
    
  })# closing renderplot post_ACA_plt
  
#+++++++++++++++++++++Difference bar plot+++++++
  # Lollipop graph
  output$difference_plot_lolipop <- renderPlotly({
    
    pre_post_data_lolipop$hover <- with(pre_post_data_lolipop, paste('</br>Stage :', Stage, '</br>After ACA :',
                                                                     Post_ACA_difference))
    
    pre_post_data_lolipop %>% 
      filter(Status == input$status) %>% 
      ggplot(aes(x=Stage, y=Post_ACA_difference, label=Post_ACA_difference)) + 
      geom_point(aes(x=Stage, y=Post_ACA_difference), stat='identity', color= "deeppink4", size=10)  +
      geom_segment(aes(y = 0, x = Stage, yend = Post_ACA_difference, xend = Stage), color = "deeppink4") +
      #geom_text(color="white", size=2) +
      ylim(-10, 10) +
      coord_flip() +
      theme_bw()+
      theme_bw(base_family = "helvetica", base_size = 18) +
      labs(x= "", y= "% Difference In Cancer Stage After ACA")+
      theme(legend.position="right",
            legend.title=element_blank(),
            axis.title.x = element_text(color="black", size=18),
            axis.title.y = element_text(color="black", size=18),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0),
            legend.text = element_text(size=16),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
      )
    
  })#closing renderplot-difference_lolipop
  
  #
  output$difference_barplot <- renderPlotly({
    pre_post_data_new %>% 
      filter(Status == input$status) %>% 
      ggplot(aes(x=Stage, y=post_ACA-pre_ACA)) +
      geom_bar(stat = "identity", aes(fill= Stage), color="black", width = 0.9) +
      # facet_wrap(~ Status) +
      coord_flip()+
      theme_bw(base_family = "helvetica", base_size = 18) +
      labs(x= "", y= "% Difference In Cancer Stage After ACA")+
      theme(legend.position ="none",
            legend.title = element_blank(),
            axis.title.x = element_text(color="black", size=18),
            axis.title.y = element_text(color="black", size=18),
            axis.text.x = element_text(size=14, angle=0),
            axis.text.y = element_text(size=14, angle=0),
            legend.text = element_text(size=16),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
      )
    
  })#closing renderplot-difference_barplot
  
  
#++++++++ Page 4: Mosaic plot  
  output$mosaic_plot <- renderPlot({
    ggplot(Y2007mx, aes(ymin = ymin, ymax = ymax,
                        xmin = xmin, xmax = xmax, fill = variable)) +
      geom_rect(color = I("black")) +
      geom_text(aes(x = xtext, y = ytext,
                    label = ifelse(Status == "Medicare", paste(variable, " - ", round(value, digits = 0), "%", sep = ""), 
                                   paste(round(value, digits = 0), "%", sep = ""))), size = 4) +
      geom_text(aes(x = xtext, y = 100,
                    label = paste(Status)), size = 6, angle=90, hjust = 0) +
      #theme(plot.margin=unit(c(2,1,1,1),"cm"))+
      #coord_cartesian(ylim = c(0, 130))+
      coord_fixed(ratio=0.5, xlim = c(0, 100), ylim=c(0, 130))+
      theme_classic() +
      ggtitle("Stage Of Cancer Diagnosis By The Health Insurance Type: 2007")+
      xlab("Type Of Health Insurance (%)") + 
      ylab("% Cancer Stage Diagnosed")+
      labs(fill = "Cancer Stage")+
      theme(legend.position="right",
            plot.title = element_text(color="black", face = "bold", size=25, hjust=0.5),
            axis.title.x = element_text(color="black", face = "bold", size=20),
            axis.title.y = element_text(color="black", face = "bold", size=20),
            axis.text.x = element_text(size=16, face = "bold", angle=0),
            axis.text.y = element_text(size=16, face = "bold", angle=0),
            legend.text = element_text(size=18),
            legend.title = element_text(size=20,face = "bold"),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
           
      )# closing ggplot mosaic
    
  })#closing renderplot-mosaic
  
  
  output$mosaic_plot2016 <- renderPlot({
    ggplot(Y2016mx, aes(ymin = ymin, ymax = ymax,
                        xmin = xmin, xmax = xmax, fill = variable)) +
      geom_rect(color = I("black")) +
      geom_text(aes(x = xtext, y = ytext,
                    label = ifelse(Status == "Medicare", paste(variable, " - ", round(value, digits = 0), "%", sep = ""), 
                                   paste(round(value, digits = 0), "%", sep = ""))), size = 4) +
      geom_text(aes(x = xtext, y = 100,
                    label = paste(Status)), size = 6, angle=90, hjust = 0) +
      #theme(plot.margin=unit(c(2,1,1,1),"cm"))+
      #coord_cartesian(ylim = c(0, 130))+
      coord_fixed(ratio=0.5, xlim = c(0, 100), ylim=c(0, 130))+
      theme_classic() +
      ggtitle("Stage Of Cancer Diagnosis By The Health Insurance Type: 2016")+
      xlab("Type Of Health Insurance (%)") + 
      ylab("% Cancer Stage Diagnosed")+
      labs(fill = "Cancer Stage")+
      theme(legend.position="right",
            plot.title = element_text(color="black", face = "bold", size=25, hjust=0.5),
            axis.title.x = element_text(color="black", size=20,face = "bold"),
            axis.title.y = element_text(color="black", size=20,face = "bold"),
            axis.text.x = element_text(size=16, face = "bold", angle=0),
            axis.text.y = element_text(size=16, face = "bold", angle=0),
            legend.text = element_text(size=18),
            legend.title = element_text(size=20, face = "bold"),
            legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.margin = unit(c(4, 1, 3, 8), "lines")
            
      )# closing ggplot mosaic 2016
    
  })#closing renderplot-mosaic 2016
  
  
  
})# Closing ShinyServer

