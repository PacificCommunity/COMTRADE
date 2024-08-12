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
      load('Data_Intermediate/FFASummaryData.rda')
      load('Data_Intermediate/FFANonSummaryData.rda')
   
      load('Data_Intermediate/FFANonSummaryData_Revisions.rda')
      load('Data_Intermediate/FFASummaryData_Revisions.rda')
                     
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
                   scale_colour_manual(values = JamesColours()) + 
                   labs(title = "Total Volume Caught, by Species", 
                        subtitle = "\nPercentage change\n",
                        caption = "FFA \nValue of WCPFC-CA Tuna Fisheries") +
                   xlab("\nYear\n") +
                   ylab("Total Metrics Tonnes\n(000)\n") +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = JamesColours("Light_Blue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = JamesColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = JamesColours("Light_Blue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = JamesColours("Red"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = JamesColours("Red"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = JamesColours("Blue")),
                         axis.text.x   = element_text(size = 14, colour = JamesColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = JamesColours("Red"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
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
