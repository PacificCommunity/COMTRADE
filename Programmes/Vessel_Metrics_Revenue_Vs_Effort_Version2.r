##
##    Programme:  Vessel_Metrics_Revenue_Vs_Effort.r
##
##    Objective: 
##
##    Author:     James Hogan, 3 December 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions and colour palattes
   ##
      source("R/functions.r")
      source("R/themes.r")

   ##
   ##    Grab the FFA data
   ##
      load('Data_Output/FFASummaryData.rda')
      load('Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2024.rda')
   
   
   ##
   ##    Grab some database information
   ##
      db1 <- odbcDriverConnect("driver=SQL Server;server=NOUFAMESQL4;database=FISH_MASTER")
      db2 <- odbcDriverConnect("driver=SQL Server;server=noufameSQL01;database=vms")
      db3 <- odbcDriverConnect("driver=SQL Server;server=noufameSQL01;database=tufman2")
      
      ##
      ##    Extract vessel activity metrics. 
      ##       tufman2.vms.vms_trips = a VMS-derived record of unique vessel IDs (based on VMS data), departing and returning to ports, 
      ##                               at specific datetimes.
      ##
      ##       tufman2.vms.vms_trip_efforts = a linked record of the time spent in different EEZ's, the number of days are sea[in the EEZ]
      ##                                      and the number of "day_fishing"
      ##
      ##                                      EEZ code spatially described here: [tufman2].[ref].[eez_definitions]
      ##
      ##       tufman2.ref.vessels = a linked record to the VMS-derived unique vessel_id which links to the vessel's gear. 
      ##                             Gear can derived from here: [tufman2].[ref].[gears]
      ##
      ##       tufman2.ref.vessel_instances = a point in time record of what is the vessel_id known by at different time, and how is it
      ##                                      flagged
      ##
      ##       FISH_MASTER.log.trips_ps = looks like an older datasource, containing older metrics of vessels, flags, departure and return ports
      ##                                 
      ##       FISH_MASTER.ref.vessel = gives basic details for what we knew about that vessel at that time. Note the link between vessel and trips_ps
      ##                               is through trips_ps.vfp_boat_id = vessel.BOAT_ID. 
      ##                               Also, it looks like FISH_MASTER.ref.vessel.ref2_guid is the same variable as tufman2.ref.vessels.vessel_id
      ##                               since Tiffany later appends them together 
      ##
      ##       FISH_MASTER.log.sets_ps = Looks to be a specific Purse seine table (there's also sets_ll, sets_pl, and sets_tr) with a catch-all "in_wcpfc_area"
      ##                                variable. 
      ##
         Dayz <- data.table(sqlQuery(db1,
                                        "SELECT YY,
                                                eez.country_name as EEZ,
                                                sum(days) as days
                                          FROM [FISH_MASTER].[ace].[A_ACE_EZ] a
                                             INNER JOIN [ref].[countries] eez    ON 
                                                case 
                                                   when a.eez_code in ('GL','LN','PX') then 'KI'
                                                   when a.eez_code in ('I1','I2','I3','I4','I5','I6','I7','I8','I9','H4','H5') then 'IW'
                                                else a.eez_code end  = eez.country_code 
                                          where gear_code = 'S'
                                             and ocean_code = 'WX' 
                                             and yy >= 2008
                                              and not (flag_code= 'PH' and a.eez_code in ('ID','I1','PH','PW','I3','I4'))       -- and not Phillipino flagged vessels located in Indonesian, Palau, Phillipine or international waters
                                              and flag_code not in ('ID','VN','BN','SG')                                         -- and not flagged to Brunei, Indonesia, Singapore or Vietnam
                                              and a.eez_code not in ('I6','JP')                                                -- and not in Japanese or international waters
                                              and not (a.eez_code in ('I7','AU','NZ'))                                              -- and not in Australian or international waters
                                             and in_arch <> 'T'
                                          group  by 
                                                YY,
                                                 eez.country_name
                                          order by 1,2"))


         
         Vessels <- data.table(sqlQuery(db1,
                   "SELECT  year(logdate) as YY,
                           eez.country_name as EEZ,
                           count(distinct t.vessel_id) as vessels
                     FROM [FISH_MASTER].log.trips_ps t inner join [FISH_MASTER].log.sets_ps s on t.log_trip_id = s.log_trip_id 
                        INNER JOIN [FISH_MASTER].[ref].[countries] eez    ON 
                           case 
                              when s.eez_code in ('GL','LN','PX') then 'KI'
                              when s.eez_code in ('I1','I2','I3','I4','I5','I6','I7','I8','I9','H4','H5') then 'IW'
                           else s.eez_code end  = eez.country_code 
                        left join [FISH_MASTER].[ref].[vessel_instances] v on t.vessel_id =  v.vessel_id
                     where in_wcpfc_area = 1
                        and year(logdate) >= 2008
                         and not (v.flag_id= 'PH' and s.eez_code in ('ID','I1','PH','PW','I3','I4'))      -- and not Phillipino flagged vessels located in Indonesian, Palau, Phillipine or international waters
                         and v.flag_id not in ('ID','VN','BN','SG')                                       -- and not flagged to Brunei, Indonesia, Singapore or Vietnam
                         and s.eez_code not in ('I6','JP')                                                -- and not in Japanese or international waters
                         and not (s.eez_code in ('I7','AU','NZ'))                                         -- and not in Australian or international waters
                        and coalesce(effort_factor,0) > 0
                        and in_AWs <> 1
                     group  by 
                           year(logdate),
                           eez.country_name
                     order by 1,2"))
                               
         vms <- merge(Dayz,
                      Vessels,
                      by = c("YY", "EEZ"))
                      
                     
                     
        
        Unique_Vessels_Count <- vms[,
                                   list(Number_of_Unique_Vessels = sum(vessels),
                                        Days_Fishing = sum(days)),
                                   by = .(Country_Name = EEZ,
                                          Year = YY)]    
                                          
        Unique_Vessels_Count[Country_Name == "PALAU"]         
        unique(Unique_Vessels_Count$Country_Name)

   ##
   ##    Plot them... 
   ##
      showtext_auto()
      ggplot() + 
         geom_bar(data=Unique_Vessels_Count,
                 aes(factor(Year), Number_of_Unique_Vessels, fill=factor(Country_Name)), 
                 stat='identity', 
                 col='black', 
                 alpha = 0.2,
                 linewidth=0.1) +
         facet_wrap( ~ Country_Name,scales = "free") +
                 
         
         theme_bw(base_size=12, base_family =  "Calibri") %+replace%
         theme(legend.title.align=0.5,
             plot.margin = unit(c(1,3,1,1),"mm"),
             panel.border = element_blank(),
             strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
             strip.text = element_text(colour = "white", 
                                       family = "MyriadPro-Bold",
                                       size =  12,
                                       margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
             panel.spacing = unit(1, "lines"),                                              
             legend.text   = element_text(family = "MyriadPro-Regular"),
             plot.title    = element_text(colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
             plot.subtitle = element_text(colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(colour = SPCColours("Red")),
             axis.title    = element_text(colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size =  8,colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size =  12,colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "none")
    ggsave("Graphical_Output/Vessel_Numbers_by_EEZ.png", height =(1.5*16.13), width = (2*20.66), dpi = 165, units = c("cm"))
   
   ##
   ##    Lets have a look at days fishing
   ##
      showtext_auto()
      ggplot() + 
         geom_bar(data=Unique_Vessels_Count,
                 aes(factor(Year), Days_Fishing, fill=factor(Country_Name)), 
                 stat='identity', 
                 col='black', 
                 alpha = 0.2,
                 linewidth=0.1) +
         facet_wrap( ~ Country_Name,scales = "free") +
                 
         
         theme_bw(base_size=12, base_family =  "Calibri") %+replace%
         theme(legend.title.align=0.5,
             plot.margin = unit(c(1,3,1,1),"mm"),
             panel.border = element_blank(),
             strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
             strip.text = element_text(colour = "white", 
                                       family = "MyriadPro-Bold",
                                       size =  12,
                                       margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
             panel.spacing = unit(1, "lines"),                                              
             legend.text   = element_text(family = "MyriadPro-Regular"),
             plot.title    = element_text(colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
             plot.subtitle = element_text(colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(colour = SPCColours("Red")),
             axis.title    = element_text(colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size =  8,colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size =  12,colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "none")
    ggsave("Graphical_Output/Days_by_EEZ.png", height =(1.5*16.13), width = (2*20.66), dpi = 165, units = c("cm"))
   

##
##    Match to licensing revenue
##
   Revenue <- data.frame(FFA_Compendium_of_Economic_and_Development_Statistics_2024[Metrics == "Licence and access fee revenue"])

   unique(Unique_Vessels_Count$Country_Name)
   unique(Revenue$Country)

   Revenue$MatchCountry <- toupper(Revenue$Country)

   Revenue_and_Effort <- data.frame(merge(Unique_Vessels_Count,
                                          Revenue[, c("MatchCountry", "Country", "Year", "Value")],
                                          by.x = c("Country_Name", "Year"),
                                          by.y = c("MatchCountry", "Year")))
                               
   Revenue_and_Effort$Per_Day_Price <- (Revenue_and_Effort$Value / Revenue_and_Effort$Days_Fishing)*1000000
   Revenue_and_Effort$PNA_Member <- ifelse(Revenue_and_Effort$Country %in% c('Federated States of Micronesia','Kiribati','Marshall Islands',
                                                                                  'Nauru','Palau','Papua New Guinea','Solomon Islands','Tokelau','Tuvalu'),1,0)

   Revenue_and_Effort$Same_Different <- ifelse(Revenue_and_Effort$Country %in% c('Palau'),"Palau",
                                        ifelse(Revenue_and_Effort$Country %in% c('Marshall Islands','Tokelau'),"Marshall Islands, Tokelau","Everyone Else"))
    
   Revenue_and_Effort[Revenue_and_Effort$PNA_Member == 1,]

   Revenue_and_Effort[Revenue_and_Effort$Country == 'Palau',]


   ggplot(data=Revenue_and_Effort[Revenue_and_Effort$PNA_Member == 1,],
           aes(x= Year, 
               y = Per_Day_Price, 
               colour=Country)) + 
      geom_smooth(se = FALSE) +
      geom_point(alpha = 0.3) +
      facet_wrap( ~ Same_Different,scales = "free") +
      scale_y_continuous(labels = scales::label_dollar()) +
      scale_x_continuous(breaks = seq(from = 2008, to = 2021, by =1)) +
      scale_colour_manual(values = SPCColours()) + 
      #geom_text_repel(aes(label=Country_Name), size=2,segment.size = 0.2, segment.colour ="red") +
      labs(title = "Estimates of VDS Price?",
           subtitle = "\nGovt Revenue divided by Total Fished Days\n") +
      ylab("$USD\n") +
      xlab("Year") +
      theme_bw(base_size=12, base_family =  "Calibri") %+replace%
      theme(legend.title.align=0.5,
          plot.margin = unit(c(1,3,1,1),"mm"),
          panel.border = element_blank(),
          strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
          strip.text = element_text(colour = "white", 
                                    family = "MyriadPro-Bold",
                                    size =  18,
                                    margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
          panel.spacing = unit(1, "lines"),                                              
          legend.text   = element_text(size =  18,family = "MyriadPro-Regular"),
          plot.title    = element_text(size =  28,colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
          plot.subtitle = element_text(size =  18,colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
          plot.caption  = element_text(colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
          plot.tag      = element_text(colour = SPCColours("Red")),
          axis.title    = element_text(size =  18,colour = SPCColours("Dark_Blue")),
          axis.text.x   = element_text(size =  18,colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
          axis.text.y   = element_text(size =  18,colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
          legend.key.width = unit(1, "cm"),
          legend.spacing.y = unit(1, "cm"),
          legend.margin = margin(10, 10, 10, 10),
          legend.position  = "bottom")
    ggsave("Graphical_Output/Maybe_VDS_DayTrading.png", height =(1.5*16.13), width = (2*20.66), dpi = 165, units = c("cm"))

 
##
##    And we're done
##


                                       

