##
##    Programme:  FFA_Revisions.r
##
##    Objective:  What are the revisions to the FFA 
##                Value of WCPFC-CA Tuna Fisheries data
##
##    Author:     James Hogan, FAME, 25 July 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/themes.r")
   ##
   ##    Load the cleaned data
   ##
      load('Data_Output/FFASummaryData.rda')
      load('Data_Output/FFANonSummaryData.rda')
   
      load('Data_Output/FFANonSummaryData_Revisions.rda')
      load('Data_Output/FFASummaryData_Revisions.rda')
                     
   ##
   ## Step 1: Lets have a look at the summary data
   ##
      Catch_Volume <- data.table(FFASummaryData[["Summary of catch"]])
      Catch_Value  <- data.table(FFASummaryData[["Summary of catch value"]])
      
      ##
      ##   Aggregate metrics
      ##
         Volume_Aggregates <- Catch_Volume[Data_Row == "4.2 CATCH BY SPECIES",
                                           list(Total_Metric_Tonnes = sum(value,na.rm = TRUE)),
                                           by = .(Measure, 
                                                  Year = as.numeric(Year), 
                                                  Spreadsheet)]
         p <-  ggplot(Volume_Aggregates[Measure != ""], 
                   aes(x = Year, 
                       y = Total_Metric_Tonnes/1000,
                       colour = Spreadsheet))  + 
                   facet_grid(. ~ Measure) +
                   geom_smooth(se = FALSE) +
                   geom_point(alpha = 0.2) +
                   scale_y_continuous(labels = comma) +
                   scale_colour_manual(values = SPCColours()) + 
                   labs(title = "Total Volume Caught, by Species", 
                        subtitle = "\nPercentage change\n",
                        caption = "FFA \nValue of WCPFC-CA Tuna Fisheries") +
                   xlab("\nYear\n") +
                   ylab("Total Metrics Tonnes\n(000)\n") +
                   theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,1,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "MyriadPro-Bold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
                         plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                         plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                         plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                         plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                         axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                         axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                         axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                         legend.key.width = unit(5, "mm"),
                         legend.spacing.y = unit(1, "mm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
                         
                         
            png(file='Graphical_Output/Change_in_Real_GDP_PC.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
               print(p)
            dev.off()



   ##
   ## Step 2: xxxxxxxxxxx
   ##
   
   
   ##
   ## Step 3: xxxxxxxxxxx
   ##



   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
