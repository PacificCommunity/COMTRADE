##
##    Programme:  Tuna_Supply_Chains.r
##
##    Objective:  Lets interogate the tuna supply chain data more closely
##
##    Author:     James Hogan, FAME - SPC, 23 January 2026
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
   ##    Load data from somewhere
   ##
      load("Data_Output/Import_Raw_Tuna.rda")
      load("Data_Output/Export_Raw_Tuna.rda")
      load("Data_Output/Export_Tinned_Tuna.rda")
      load("Data_Output/Import_Tinned_Tuna.rda")

      Everyone <- rbind(Import_Raw_Tuna, Export_Raw_Tuna, Import_Tinned_Tuna, Export_Tinned_Tuna)
      Everyone <- data.table::dcast(Everyone,
                                    reporter_desc + Year + variable ~ Source,
                                    value.var = "value")
      Everyone$Canning_Value <- Everyone$`Tinned Exports` - Everyone$`Raw Imports`
      Everyone <- Everyone[!is.na(Canning_Value)]
      
      Everyone[(reporter_desc == "Thailand")]

   
   ##
   ## Step 3: xxxxxxxxxxx
   ##
      Plot_Me <- data.table::melt(Everyone,
                                  id.var = c("reporter_desc","Year", "variable"),
                                  variable.name = "Source",
                                  variable.factor = FALSE)

      ggplot(Plot_Me[(Source == "Canning_Value") & (reporter_desc == "Thailand")], 
             aes(x = as.numeric(Year), 
                 y = value, 
                 colour = reporter_desc))     +
             geom_line(size =1) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             facet_wrap(~variable, scales="free") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/xxx.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
