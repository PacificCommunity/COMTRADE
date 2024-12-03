     rm(list=ls(all=TRUE))
      options(scipen = 999)
   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
      library(RODBC)
      
      library(sysfonts)
      library(showtext)
            
   ##
   ##    Project-specific libraries
   ##
      library(comtradr)
      library(curl)
      library(XML)   
      library(RJSONIO)   
      library(data.table)
      library(ggrepel)

      library(strucchange)
      library(lmtest)
      library(dynlm)
      library(systemfit)
      library(tseries)
      library(cluster)
      library(nlme)
      library(plm)
      library(splines)
      library(systemfit)
      library(forecast)   
   ##
   ##    Set working directory
   ##
      setwd("C:/Work_Related_Projects/COMTRADE")
      
   ##
   ##    Grab some database information
   ##
      db1 <- odbcDriverConnect("driver=SQL Server;server=nouSQL03;database=LOG_MASTER")
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

                                          