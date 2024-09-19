##
##    Programme:  Vessel_Metrics.r
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
      load('Data_Intermediate/FFASummaryData.rda')
   
   
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
         vms = sqlQuery(db3,
                   "SELECT year(departure_date) as yy, 
                           v.vessel_id
                     FROM vms.vms_trips t 
                        INNER JOIN vms.vms_trip_efforts e  ON (t.vms_trip_id = e.vms_trip_id)
                        INNER JOIN ref.vessels v           ON (v.vessel_id   = t.vessel_id)
                        INNER JOIN ref.vessel_instances vi ON (vi.vessel_id  = v.vessel_id)
                                                                  AND 
                                                              (t.departure_date BETWEEN vi.start_date AND vi.calculated_end_date)
                     where v.gear = 'S'                                                                  -- Purse seine gear
                        and not (flag_id = 'PH' and e.eez_code in ('ID','I1','PH','PW','I3','I4'))       -- and not Phillipino flagged vessels located in Indonesian, Palau, Phillipine or international waters
                        and flag_id not in ('ID','VN','BN','SG')                                         -- and not flagged to Brunei, Indonesia, Singapore or Vietnam
                        and e.eez_code not in ('I6','JP')                                                -- and not in Japanese or international waters
                        and not (e.eez_code in ('I7','AU'))                                              -- and not in Australian or international waters
                        and day_fishing > 0                                                              -- and having spent some time fishing there
                        and year(departure_date) >= 2014                                                 -- anytime after 2013
                     ")
        PS_Log_VMS = sqlQuery(db1,
                   "SELECT year(first_logdate) as yy, 
                           v.ref2_guid as vessel_id 
                     FROM log_master.log.trips_ps st
                        inner join log_master.ref.vessel v   on (st.vfp_boat_id = v.BOAT_ID)             
                        inner join log_master.log.sets_ps sr on (st.log_trip_id = sr.log_trip_id)
                     WHERE YEAR(first_logdate) >= 2000                                                                        -- anytime after 2000
                        AND st.catch_flag_code+coalesce(vfp_fleet_id,'  ') not in ('PHPH','IDID','VN  ','IDDW','ID  ','VNVN') -- Im trusting this is knocking out Phillipino, Indonesian and Vietnam... umm..
                        and in_wcpfc_area = 1                                                                                 -- For activity in wcpfc
                    ")
        Unique_Vessels <- data.table(unique(rbind(vms, PS_Log_VMS)))
        
        Unique_Vessels_Count <- Unique_Vessels[,
                                         list(vess = length(vessel_id)),
                                         by = .(yy)]                    

        #PS_Log_VMS %<>% rbind(vms) %>% group_by(yy) %>% summarise(vess = n_distinct(vessel_id))
        
      ##
      ##    Ok, reaching into the ace.A_ACE table to calculate the total number of vessels operating
      ##       over a long piece of time
      ##

# From ACE/vessel reporting

## 
## The JP vess_cat1_n is vessels fishing north of the tropical waters and should be excluded
## 
   ##
   ##    Grab the vessel data
   ##
      Vessels = data.table(sqlQuery(db1,"SELECT *
                                            FROM ace.A_ACE
                                           WHERE ocean_code = 'WX'"))


      Vessels <- Vessels[((GEAR_CODE == 'S') &
                          (paste0(FLAG_CODE,FLEET_CODE) %nin% c('PHPH','IDID','JPCS','VNVN')) &
                          (FLAG_CODE %nin%  c('AU','VN','EP'))),]
                         
      Vessels$Fleet   <- ifelse(Vessels$FLAG_CODE %in% c('FM','KI','MH','PG','SB','TV','VU','NR','CK','PW'),'Domestic Fleet', 'Distant Water Fleet')
      Vessels$vessels <- ifelse(Vessels$Fleet      == 'Domestic Fleet', Vessels$VESS_N,
                          ifelse(Vessels$FLAG_CODE == 'NZ', Vessels$VESS_CAT2_N + Vessels$VESS_CAT3_N + Vessels$VESS_CAT4_N, 
                            ifelse(Vessels$FLAG_CODE == 'JP', Vessels$VESS_N - Vessels$VESS_CAT1_N, Vessels$VESS_N) ) )
      Vessels$jp_200grt <- ifelse(Vessels$FLAG_CODE   == 'JP', Vessels$VESS_CAT1_N, NA)

      Vessels <- Vessels[,
                          list(vessels = sum(vessels,na.rm = TRUE)),
                          by = .(YY, 
                                 Fleet=factor(Fleet, levels=c('Distant Water Fleet','Domestic Fleet')))]
      Vessels <- Vessels[order(Vessels$Fleet, Vessels$YY)]
      
   ##
   ##    Estimate the vessel growth between 1972 - 1977
   ##
      Exponential_Growth_PreResearch <- (log(Vessels$vessels[Vessels$YY == 1977]) - log(Vessels$vessels[Vessels$YY == 1972]) )/ (1977 - 1972)
      
      Exponential_Growth_PreResearch <- (log(sum(Vessels$vessels[(Vessels$YY == 2023)])) - 
                                         log(Vessels$vessels[(Vessels$YY == 1972) & (Vessels$Fleet == 'Distant Water Fleet')]) ) / (2023 - 1972)
                                         
      Additive_Growth_PreResearch    <- (Vessels$vessels[Vessels$YY == 1977] - Vessels$vessels[Vessels$YY == 1972])/ (1977 - 1972)
      
      Vessels$Exponential_Growth_PreResearch <- sapply(1:nrow(Vessels), function(r)
                                                       {
                                                         Vessels$vessels[Vessels$YY == 1972] * exp(Exponential_Growth_PreResearch * (Vessels$YY[r] - 1972))
                                                       })
      Vessels$Additive_Growth_PreResearch <- sapply(1:nrow(Vessels), function(r)
                                                       {
                                                         Vessels$vessels[Vessels$YY == 1972] + (Additive_Growth_PreResearch * (Vessels$YY[r] - 1972))
                                                       })

   ##
   ##    Plot them... 
   ##
      showtext_auto()
      ggplot() + 
         geom_bar(data=Vessels[YY >= 1972],
                 aes(factor(YY), vessels, fill=factor(Fleet)), 
                 stat='identity', 
                 col='black', 
                 alpha = 0.2,
                 linewidth=0.1) +

         scale_fill_manual(values = SPCColours(),name = "Vessel Type") +    
         
         geom_line(data=Vessels[(YY >= 1972) & (Fleet == "Distant Water Fleet")], 
                   aes(factor(YY), Additive_Growth_PreResearch ,group=1), 
                   stat='identity', 
                   linewidth=1.3, 
                   linetype=2, 
                   colour = SPCColours("Red")) +   
         
         geom_line(data=Vessels[(YY >= 1972) & (Fleet == "Distant Water Fleet")], 
                   aes(factor(YY), Exponential_Growth_PreResearch ,group=1), 
                   stat='identity', 
                   linewidth=1.3, 
                   linetype=2, 
                   colour = SPCColours("Gold")) +   

         annotate("text", x=2, y=280, label = "Exponential Growth from 2023 - 1972", family ="MyriadPro-Light", hjust = 0.0,colour = SPCColours("Gold"), size = 7) +              
         annotate("text", x=2, y=260, label = "Additive Growth from 1977 - 1972",family ="MyriadPro-Light", hjust = 0.0, colour = SPCColours("Red"), size = 7) +              

                   
         scale_x_discrete(breaks = seq(from = 1972, to = 2025, by =5)) +
         scale_y_continuous(breaks = seq(from = 0, to = 320, by =20)) +
         labs(title = "Number of Purse Seine Vessels Operating within the WCPFC",
            caption  = "Data Source: The Pacific Community (SPC)") +
         xlab("") +
         ylab("Number of Purse Seine Vessels") +
         
         geom_vline(xintercept = c(1977), colour = SPCColours("Green")) +
         
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
             plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
             plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
             axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "bottom")

          ggsave("Graphical_Output/Number_of_Purse_Seine_Vessels_Operating_within_the_WCPFC.png", height =16.13, width = 20.66, dpi = 165, units = c("cm"))
          
          
      ggplot() + 
         geom_bar(data=Vessels[YY >= 1972],
                 aes(factor(YY), vessels, fill=factor(Fleet)), 
                 stat='identity', 
                 col='black', 
                 alpha = 0.2,
                 linewidth=0.1) +
         scale_fill_manual(values = SPCColours(),name = "Vessel Type") +    
         
         geom_line(data=Vessels[(YY >= 1972) & (Fleet == "Distant Water Fleet")], 
                   aes(factor(YY), Additive_Growth_PreResearch ,group=1), 
                   stat='identity', 
                   linewidth=1.3, 
                   linetype=2, 
                   colour = SPCColours("Red")) +   
         
         geom_line(data=Vessels[(YY >= 1972) & (Fleet == "Distant Water Fleet")], 
                   aes(factor(YY), Exponential_Growth_PreResearch ,group=1), 
                   stat='identity', 
                   linewidth=1.3, 
                   linetype=2, 
                   colour = SPCColours("Gold")) +   

         annotate("text", x=2, y=280, label = "Exponential Growth from 2023 - 1972", family ="MyriadPro-Light", hjust = 0.0,colour = SPCColours("Gold"), size = 7) +              
         annotate("text", x=2, y=260, label = "Additive Growth from 1977 - 1972",family ="MyriadPro-Light", hjust = 0.0, colour = SPCColours("Red"), size = 7) +              

                   
         scale_x_discrete(breaks = seq(from = 1972, to = 2025, by =5)) +
         scale_y_continuous(breaks = seq(from = 0, to = 320, by =20)) +
         xlab("") +
         ylab("Number of Purse Seine Vessels") +
         geom_vline(xintercept = c(1977), colour = SPCColours("Green")) +
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
             plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
             plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
             axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "bottom")

          ggsave("Graphical_Output/Number_of_Purse_Seine_Vessels_Operating_within_the_WCPFC_NoTitles.png", height =16.13, width = 20.66, dpi = 165, units = c("cm"))


##
##    And we're done
##
