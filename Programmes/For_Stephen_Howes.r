##
##    Programme:  For_Stephen_Howes.r
##
##    Objective:  What is this programme designed to do?
##


                  # From: Stephen Howes <stephen.howes@anu.edu.au> 
                  # Sent: Wednesday, 17 September 2025 1:43 pm
                  # To: James Hogan <jamesh@spc.int>
                  # Subject: Query re PNA tuna catch

                  # Hi James

                  # You contacted us recently re our GNDI paper. I hope you don’t mind if I ask you a question. I convene a course at the ANU on 
                  # Pacific economies. One of the topics I cover is fish stock management. I find this incredibly complex, but our own recent 
                  # work has confirmed the incredible importance of this source of income to the Pacific. Perhaps at some point you could give 
                  # a guest lecture for us. But for now, I will try to cover it in just a few slides as I normally do. 

                  # In preparing for the lecture, I came across a new article about PNA and the VDS, which has this graph, and basically 
                  # argues/implies that the PNA/VDS countries are making more money out of tuna because a lot more tuna is being caught in 
                  # their waters. The link for this article is https://onlinelibrary.wiley.com/doi/full/10.1002/app5.70042. 


                   
                  # This surprised me because I always read about PNA catches being mainly sustainable, so I did some digging round and came 
                  # over this from PNA itself. It shows no increasing trend for PNA catch. It is purse seine rather than total, but I understand 
                  # purse seine is the large majority of the catch. 

                  # https://www.pnatuna.com/sites/default/files/VDS-T_SC9%20WP.6a_PS%20TAE%20for%202021-2023%20Final_0.pdf


                  # I was hoping you could shed some light on this. Any comments or suggestions would be very welcome.

                  # Thanks and regards

                  # Stephen
                   
##
##    There's a decomposition of FFA data in COMTRADE\Documentation\Breakdown of FFA Statistics.xlsx
##    which breaks down what each spreadsheet tab means and how it relates to each other.
##
##    This programme replicates it, but with much finer disaggregation
##
##    Author:     James Hogan, FAME Economics, 19 Se
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions and colour palattes
   ##
      source("R/themes.r")
   ##
   ##    Grab the FFA data
   ##
      load('Data_Output/FFASummaryData.rda')
      load('Data_Output/FFANonSummaryData.rda')
      load('Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2024.rda')

      Spreadsheet <- "WCPFC-CA_tuna_fisheries_2024"
   ##
   ##    Estimate the value of the tuna fish extracted
   ##
      Catch_Volume_by_National_Water     <- FFANonSummaryData[["Catch by national waters"]] 
      Catch_Volume_by_Fleet              <- FFANonSummaryData[["Catch by fleet"]] 
      Catch_Volume_by_Fleet_in_Own_Water <- FFANonSummaryData[["Fl Cat own nat wat"]]       
      Catch_Volume_by_Fleet_in_FFA_Water <- FFANonSummaryData[["Fl cat nat wat FFA members"]]

      Catch_Volume_by_National_Water     <- Catch_Volume_by_National_Water[Catch_Volume_by_National_Water$Spreadsheet == Spreadsheet,]
      Catch_Volume_by_Fleet              <- Catch_Volume_by_Fleet[Catch_Volume_by_Fleet$Spreadsheet == Spreadsheet,]
      Catch_Volume_by_Fleet_in_Own_Water <- Catch_Volume_by_Fleet_in_Own_Water[Catch_Volume_by_Fleet_in_Own_Water$Spreadsheet == Spreadsheet,]
      Catch_Volume_by_Fleet_in_FFA_Water <- Catch_Volume_by_Fleet_in_FFA_Water[Catch_Volume_by_Fleet_in_FFA_Water$Spreadsheet == Spreadsheet,]

      
   ##
   ##    Change the value names
   ##
      names(Catch_Volume_by_Fleet_in_Own_Water)[names(Catch_Volume_by_Fleet_in_Own_Water) == "value"] = "Fleet_catch_in_own_national_waters"
      
      Catch_Volume_by_Fleet_in_Own_Water$Data_Row <- str_sub(Catch_Volume_by_Fleet_in_Own_Water$Data_Row, start = 5)
      Catch_Volume_by_Fleet_in_FFA_Water$Data_Row <- str_sub(Catch_Volume_by_Fleet_in_FFA_Water$Data_Row, start = 5)
      Catch_Volume_by_Fleet$Data_Row              <- str_sub(Catch_Volume_by_Fleet$Data_Row, start = 5)
      Catch_Volume_by_National_Water$Data_Row     <- str_sub(Catch_Volume_by_National_Water$Data_Row, start = 5)
      
   ##
   ##    Drop the totals
   ##
      Catch_Volume_by_Fleet_in_Own_Water <- Catch_Volume_by_Fleet_in_Own_Water[Catch_Volume_by_Fleet_in_Own_Water$Data_Row != "ALL GEARS", !(names(Catch_Volume_by_Fleet_in_Own_Water) %in% c("Tab", "Spreadsheet"))]
      Catch_Volume_by_Fleet_in_FFA_Water <- Catch_Volume_by_Fleet_in_FFA_Water[Catch_Volume_by_Fleet_in_FFA_Water$Data_Row != "ALL GEARS",!(names(Catch_Volume_by_Fleet_in_FFA_Water) %in% c("Spreadsheet", "Tab"))]
      Catch_Volume_by_Fleet              <- Catch_Volume_by_Fleet[Catch_Volume_by_Fleet$Data_Row != "ALL GEARS",!(names(Catch_Volume_by_Fleet) %in% c("Spreadsheet", "Tab"))]
      Catch_Volume_by_National_Water     <- Catch_Volume_by_National_Water[Catch_Volume_by_National_Water$Data_Row != "ALL GEARS",!(names(Catch_Volume_by_National_Water) %in% c("Spreadsheet", "Tab"))]

   ##
   ##    Rename Australia
   ##
      Catch_Volume_by_National_Water$Measure[str_detect(Catch_Volume_by_National_Water$Measure, "Australia")] <- "Australia"
      
     
     
   ##
   ##   Apply the logic in the COMTRADE\Documentation\Breakdown of FFA Statistics.xlsx spreadsheet
   ##
      Local_Metrics <- merge(Catch_Volume_by_Fleet_in_FFA_Water,
                             Catch_Volume_by_Fleet_in_Own_Water,
                             by = c("Species","Data_Row","Measure","Year"),
                             all = TRUE)
      Local_Metrics$Fleet_catch_in_national_waters_of_FFA_members <- ifelse(Local_Metrics$Measure %in% c('Australia','Cook Islands' ,'Fiji' ,
                                                                                                         'FSM' ,'Kiribati' ,'Marshall Islands' ,'Nauru' ,'New Zealand' ,
                                                                                                         'Niue' ,'PNG' ,'Palau' ,'Samoa' ,'Solomon  Islands' ,'Tokelau' ,
                                                                                                         'Tonga' ,'Tuvalu' ,'Vanuatu'), (Local_Metrics$value - Local_Metrics$Fleet_catch_in_own_national_waters), Local_Metrics$value)

   ##
   ##   Distant Water activity
   ##
      DWF_Metrics <- merge(Catch_Volume_by_Fleet,
                           Catch_Volume_by_National_Water,
                           by = c("Species","Data_Row","Measure","Year"),
                           all = TRUE)     
                           
      DWF_Metrics$value.x[is.na(DWF_Metrics$value.x)] <- 0
      DWF_Metrics$value.x[is.na(DWF_Metrics$value.y)] <- 0
      DWF_Metrics$Catch_by_Fleet <- DWF_Metrics$value.x
      DWF_Metrics$Catch_by_EEZ   <- DWF_Metrics$value.y
      
      DWF_Metrics <- merge(DWF_Metrics,
                           Local_Metrics,
                           by = c("Species","Data_Row","Measure","Year"),
                           all = TRUE)     
      DWF_Metrics$Fleet_catch_in_own_national_waters[is.na(DWF_Metrics$Fleet_catch_in_own_national_waters)] <- 0
      DWF_Metrics$Fleet_catch_in_national_waters_of_FFA_members[is.na(DWF_Metrics$Fleet_catch_in_national_waters_of_FFA_members)] <- 0
      
      DWF_Metrics$Fleet_catch_in_highseas_non_FFA_Member <- with(DWF_Metrics, (Catch_by_Fleet - Fleet_catch_in_own_national_waters - Fleet_catch_in_national_waters_of_FFA_members))

      DWF_Metrics$Other_Country_Catch_in_National_Waters <- with(DWF_Metrics, Catch_by_EEZ - Fleet_catch_in_own_national_waters)

      Totals <- with(DWF_Metrics[DWF_Metrics$Year == 1997,],
                 aggregate(list(Catch_by_Fleet = Catch_by_Fleet,
                                Catch_by_EEZ = Catch_by_EEZ,
                                Fleet_catch_in_national_waters_of_FFA_members = Fleet_catch_in_national_waters_of_FFA_members,
                                Fleet_catch_in_own_national_waters =Fleet_catch_in_own_national_waters,
                                Other_Country_Catch_in_National_Waters = Other_Country_Catch_in_National_Waters,
                                Fleet_catch_in_highseas_non_FFA_Member = Fleet_catch_in_highseas_non_FFA_Member),
                           list(Measure = Measure),
                         sum, 
                         na.rm = TRUE))
      Totals

      
   ##
   ##   Merge them all together
   ##
      Fisheries_Decomposition <- DWF_Metrics[,c("Species","Data_Row","Measure","Year","Catch_by_Fleet", "Catch_by_EEZ", 
                                                "Fleet_catch_in_own_national_waters", "Fleet_catch_in_national_waters_of_FFA_members", "Fleet_catch_in_highseas_non_FFA_Member", "Other_Country_Catch_in_National_Waters")]
      Fisheries_Decomposition$Year = as.numeric(Fisheries_Decomposition$Year)                                      


                                                              
   ##
   ##   Now add the PNA parties
   ##
      Fisheries_Decomposition$PNA <- ifelse(Fisheries_Decomposition$Measure %in% c("FSM","Kiribati","Marshall Islands","Nauru","Palau","PNG","Solomon  Islands","Tokelau","Tuvalu"), "PNA Member", 
                                       ifelse(Fisheries_Decomposition$Measure %in% c('H4','H5','I1','I2','I3','I4','I5','I6','I7','I8','I9','IW'), "High Seas", "Non-PNA Member"))
      Fisheries_Decomposition$FFA <- ifelse(Fisheries_Decomposition$Measure %in% c("Australia","Cook Islands","Fiji","FSM","Kiribati","Marshall Islands","Nauru","New Zealand","Niue","Palau","PNG","Samoa","Solomon  Islands","Tokelau","Tonga", "Tuvalu","Vanuatu"), "FFA Member",
                                       ifelse(Fisheries_Decomposition$Measure %in% c('H4','H5','I1','I2','I3','I4','I5','I6','I7','I8','I9','IW'), "High Seas", "Non-FFA Member"))
      Fisheries_Decomposition$SPC <- ifelse(Fisheries_Decomposition$Measure %in% c("Australia","Cook Islands","Fiji","FSM","Kiribati","Marshall Islands","Nauru","New Zealand","Niue","Palau","PNG","Samoa","Solomon  Islands","Tokelau","Tonga", 
                                                                                   "Tuvalu","Vanuatu","French Polynesia","Wallis and Futuna","New Caledonia", "Northern Mariana Islands","US", "US (includes territories, ex Am Samoa)", 
                                                                                   "US (includes territories, incl Am Samoa)","American Samoa","Mathew and Hunter" ), "SPC Member", 
                                       ifelse(Fisheries_Decomposition$Measure %in% c('H4','H5','I1','I2','I3','I4','I5','I6','I7','I8','I9','IW'), "High Seas","Non-SPC Member"))
      
      Volume_Aggregates <-  with(Fisheries_Decomposition,
                                    aggregate(list(Catch_by_Fleet   = Catch_by_Fleet,
                                                   Catch_by_EEZ     = Catch_by_EEZ,
                                                   Fleet_catch_in_own_national_waters        = Fleet_catch_in_own_national_waters,
                                                   Fleet_catch_in_national_waters_of_FFA_members = Fleet_catch_in_national_waters_of_FFA_members,
                                                   Fleet_catch_in_highseas_non_FFA_Member = Fleet_catch_in_highseas_non_FFA_Member,
                                                   Other_Country_Catch_in_National_Waters = Other_Country_Catch_in_National_Waters),
                                              list(Year = Year,
                                                   Country = ifelse(PNA == "PNA Member", "PNA Member",
                                                             ifelse(FFA == "FFA Member", "FFA Member", 
                                                             ifelse(SPC == "SPC Member", "SPC Member",
                                                             ifelse(SPC == "High Seas",  "High Seas", "DWFN"))))),
                                              sum,
                                              na.rm = TRUE))
   ##
   ##   Great! Now graph it
   ##
      Plot_Me <- reshape2::melt(Volume_Aggregates,
                                id.vars = c("Year", "Country"))
                                
                                
      ggplot(Plot_Me,
             aes(x = Year, 
                 y = value, 
                 colour = Country))     +
             geom_smooth() +
             geom_point() +
             geom_vline(xintercept = 2007) + 
             facet_wrap(~ str_replace_all(variable, "_", " "), scales="free") +
             labs(x = "\nTime Period", 
                  y = "Volume of Catch\nMetric Tonnes (000's)", 
                  title="Decomposition of FFA Catch Volumes - Split by PNA Member / non-PNA Member\n",
                  caption = "FAME\nThe Pacific Community (SPC)") +
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
                   legend.text   = element_text(size = 14, family = "MyriadPro-Regular"),
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
                   legend.position  = "bottom")                      
         ggsave("Graphical_Output/Decompose FFA Data.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

                                      ifelse(Fisheries_Decomposition$Measure %in% c('H4','H5','I1','I2','I3','I4','I5','I6','I7','I8','I9','IW'), "High Seas","Non-SPC Member"))
      
      Volume_Aggregates <-  with(Fisheries_Decomposition,
                                    aggregate(list(Catch_by_Fleet   = Catch_by_Fleet,
                                                   Catch_by_EEZ     = Catch_by_EEZ,
                                                   Fleet_catch_in_own_national_waters        = Fleet_catch_in_own_national_waters,
                                                   Fleet_catch_in_national_waters_of_FFA_members = Fleet_catch_in_national_waters_of_FFA_members,
                                                   Fleet_catch_in_highseas_non_FFA_Member = Fleet_catch_in_highseas_non_FFA_Member,
                                                   Other_Country_Catch_in_National_Waters = Other_Country_Catch_in_National_Waters),
                                              list(Year = Year),
                                              sum,
                                              na.rm = TRUE))
   ##
   ##   Great! Now graph it
   ##
      Plot_Me <- reshape2::melt(Volume_Aggregates,
                                id.vars = c("Year"))
                                
                           
      ggplot(Plot_Me,
             aes(x = Year, 
                 y = value))     +
             geom_smooth() +
             geom_point() +
             geom_vline(xintercept = 2007) + 
             facet_wrap(~ str_replace_all(variable, "_", " "), scales="free") +
             labs(x = "\nTime Period", 
                  y = "Volume of Catch\nMetric Tonnes (000's)", 
                  title="Total Catch Volumes",
                  caption = "FAME\nThe Pacific Community (SPC)") +
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
                   legend.text   = element_text(size = 14, family = "MyriadPro-Regular"),
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
                   legend.position  = "bottom")                      
         ggsave("Graphical_Output/Total Catch Volumes.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
                                                  
##
##    And we're done
##

              


