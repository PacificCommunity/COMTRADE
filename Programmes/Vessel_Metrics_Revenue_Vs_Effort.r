##
##    Programme:  Vessel_Metrics_Revenue_Vs_Effort.r
##
##    Objective:  Based on Tiffany Vidal code, this code replicates figure 3.1 
##                from here: https://meetings.wcpfc.int/node/23098
##
##    Author:     Tiffany Vidal, modified by James Hogan
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
      load('Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2022.rda')
   
   
   ##
   ##    Grab some database information
   ##
      db1 <- odbcDriverConnect("driver=SQL Server;server=nouSQL03;database=LOG_MASTER")
      db2 <- odbcDriverConnect("driver=SQL Server;server=noufameSQL01;database=vms")
      db3 <- odbcDriverConnect("driver=SQL Server;server=noufameSQL01;database=tufman2")
      
      ##
      ##    Grab the ocean names
      ##
         World_Oceans = sqlQuery(db1,"SELECT *
                                        FROM ace.oceans")

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
      ##       log_master.log.trips_ps = looks like an older datasource, containing older metrics of vessels, flags, departure and return ports
      ##                                 
      ##       log_master.ref.vessel = gives basic details for what we knew about that vessel at that time. Note the link between vessel and trips_ps
      ##                               is through trips_ps.vfp_boat_id = vessel.BOAT_ID. 
      ##                               Also, it looks like log_master.ref.vessel.ref2_guid is the same variable as tufman2.ref.vessels.vessel_id
      ##                               since Tiffany later appends them together 
      ##
      ##       log_master.log.sets_ps = Looks to be a specific Purse seine table (there's also sets_ll, sets_pl, and sets_tr) with a catch-all "in_wcpfc_area"
      ##                                variable. 
      ##
         vms = data.table(sqlQuery(db3,
                   "SELECT year(departure_date) as Year, 
                           case  
                              when eez.ez_desc in ('Gilbert Islands','Line Islands','Jarvis','Phoenix Islands','Northern Islands') then 'Kiribati'
                              when eez.ez_desc in ('Marshall','Marshall Islands') then 'Marshall Islands'
                              when eez.ez_desc in ('Federated states of Micronesia') then 'Federated States of Micronesia'
                              else eez.ez_desc
                           end as Country_Name,
                           v.vessel_id,
                           sum(e.day_at_sea)  as Days_At_Sea,
                           sum(e.day_fishing) as Days_Fishing,
                           1 as Count
                           
                     FROM vms.vms_trips t 
                        INNER JOIN vms.vms_trip_efforts e  ON (t.vms_trip_id = e.vms_trip_id)
                        INNER JOIN ref.vessels v           ON (v.vessel_id   = t.vessel_id)
                        INNER JOIN ref.vessel_instances vi ON (vi.vessel_id  = v.vessel_id)
                                                                  AND 
                                                              (t.departure_date BETWEEN vi.start_date AND vi.calculated_end_date)
                        INNER JOIN [ref].[eez_definitions] eez    ON (e.eez_code   = eez.eez_code)
                     where v.gear = 'S'                                                                  -- Purse seine gear
                        and not (flag_id = 'PH' and e.eez_code in ('ID','I1','PH','PW','I3','I4'))       -- and not Phillipino flagged vessels located in Indonesian, Palau, Phillipine or international waters
                        and flag_id not in ('ID','VN','BN','SG')                                         -- and not flagged to Brunei, Indonesia, Singapore or Vietnam
                        and e.eez_code not in ('I6','JP')                                                -- and not in Japanese or international waters
                        and not (e.eez_code in ('I7','AU'))                                              -- and not in Australian or international waters
                        and day_fishing > 0                                                              -- and having spent some time fishing there
                     group by year(departure_date), 
                           case  
                              when eez.ez_desc in ('Gilbert Islands','Line Islands','Jarvis','Phoenix Islands','Northern Islands') then 'Kiribati'
                              when eez.ez_desc in ('Marshall','Marshall Islands') then 'Marshall Islands'
                              when eez.ez_desc in ('Federated states of Micronesia') then 'Federated States of Micronesia'
                              else eez.ez_desc
                           end,
                           v.vessel_id
                     order by year(departure_date), 
                              case  
                              when eez.ez_desc in ('Gilbert Islands','Line Islands','Jarvis','Phoenix Islands','Northern Islands') then 'Kiribati'
                              when eez.ez_desc in ('Marshall','Marshall Islands') then 'Marshall Islands'
                              when eez.ez_desc in ('Federated states of Micronesia') then 'Federated States of Micronesia'
                              else eez.ez_desc
                           end,
                              v.vessel_id
                     "))
        
        Unique_Vessels_Count <- vms[,
                                   list(Number_of_Unique_Vessels = sum(Count),
                                        Days_At_Sea = sum(Days_At_Sea),
                                        Days_Fishing = sum(Days_Fishing)),
                                   by = .(Country_Name,
                                          Year)]    
                                          
        Unique_Vessels_Count[Country_Name == "Palau"]         
        unique(Unique_Vessels_Count$Country_Name)
        
      ##
      ##    This should sum back up to the number of distinct vessels - and it does ;)
      ##
            sqldf("Select distinct Year, count(vessel_id)
                     from (Select distinct vessel_id, Year
                              from vms)
                     group by Year")

                                          
        
      ##
      ##    Ok, reaching into the ace.A_ACE table to calculate the total number of vessels operating
      ##       over a long piece of time
      ##

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
   Revenue <- data.frame(FFA_Compendium_of_Economic_and_Development_Statistics_2022[Metrics == "Licence and access fee revenue"])

   unique(Unique_Vessels_Count$Country_Name)
   unique(Revenue$Country)


   Revenue_and_Effort <- data.frame(merge(Unique_Vessels_Count,
                                          Revenue[, c("Country", "Year", "Value")],
                                          by.x = c("Country_Name", "Year"),
                                          by.y = c("Country", "Year")))
                               
   Revenue_and_Effort$Per_Day_Price <- (Revenue_and_Effort$Value / Revenue_and_Effort$Days_Fishing)*1000000
   Revenue_and_Effort$PNA_Member <- ifelse(Revenue_and_Effort$Country_Name %in% c('Federated States of Micronesia','Kiribati','Marshall Islands',
                                                                                  'Nauru','Palau','Papua New Guinea','Solomon Islands','Tokelau','Tuvalu'),1,0)

   Revenue_and_Effort$Same_Different <- ifelse(Revenue_and_Effort$Country_Name %in% c('Marshall Islands','Palau','Tokelau'),"Marshall Islands, Palau, Tokelau","Everyone Else")
    
   Revenue_and_Effort[Revenue_and_Effort$PNA_Member == 1,]

   Revenue_and_Effort[Revenue_and_Effort$Country_Name == 'Palau',]


   ggplot(data=Revenue_and_Effort[Revenue_and_Effort$PNA_Member == 1,],
           aes(x= Year, 
               y = Per_Day_Price, 
               colour=Country_Name)) + 
      geom_smooth(se = FALSE) +
      geom_point(alpha = 0.3) +
      facet_wrap( ~ Same_Different,scales = "free") +
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


                                       

