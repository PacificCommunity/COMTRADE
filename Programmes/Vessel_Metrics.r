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
##    Load some generic functions and colour palettes
##
source("R/functions.r")
source("R/themes.r")


library(odbc)
library(tidyverse)
dat=PS_Log_VMS
# Fleet_id... a bit of a thorn at the moment
get.fleet <- function(dat) {
  dat <- dat |> 
    mutate(
      fleet_id = case_when(
        flag_id %in% c('CK', 'KI','FJ', 'FM', 'MH', 'NR', 'PG', 'SB','TO','TV', 'TK','VU','WS') ~ NA_character_,
        flag_id == 'PH' & dep_yy < 2018 ~ 'DW',
        flag_id == 'PH' & dep_yy >= 2018 & grepl("tufman2", source, ignore.case = TRUE) &
          return_port_id %in% c(
            'E91A0DFA-CCE5-EAB3-36A6-39D163297A7B',
            'F8D486E5-9362-A16E-B968-39D163297A7B',
            '930A973D-68AE-57FC-DD15-39D163297A7B'
          ) ~ 'PH',
        flag_id == 'PH' & dep_yy >= 2018 & grepl("tufman2", source, ignore.case = TRUE) &
          !return_port_id %in% c(
            'E91A0DFA-CCE5-EAB3-36A6-39D163297A7B',
            'F8D486E5-9362-A16E-B968-39D163297A7B',
            '930A973D-68AE-57FC-DD15-39D163297A7B'
          ) ~ 'DW',
        flag_id == 'ID' & eez_code != 'ID' ~ 'DW',
        TRUE ~ NA_character_
      )
    )
  return(dat)
}


##
##    Grab some database information
##
db1 <- dbConnect(odbc::odbc(), 
                 .connection_string="driver=SQL Server;server=OFP_DBS;database=FISH_MASTER")
db2 <- dbConnect(odbc::odbc(), 
                 .connection_string="driver=SQL Server;server=noufamesql01;database=vms")
db3 <- dbConnect(odbc::odbc(), 
                 .connection_string="driver=SQL Server;server=nouesql6;database=tufman2")

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
vms = dbGetQuery(db3,
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

# Adding extra fields for the fleet_id function

PS_Log_VMS = dbGetQuery(db1,
                        "SELECT year(depart_date) as dep_yy, return_port_id, year(first_logdate) as yy, 
                           flag_id, v.vessel_id, st.source, eez_code
                     FROM FISH_MASTER.log.trips_ps st
                        inner join FISH_MASTER.ref.vessel_instances v on st.vessel_id = v.vessel_id
                          and depart_date between start_date and calculated_end_date
                        inner join FISH_MASTER.log.sets_ps sr on (st.log_trip_id = sr.log_trip_id)
                     WHERE YEAR(first_logdate) >= 2000                                                                        -- anytime after 2000
                        and in_wcpfc_area = 1                                                                                 -- For activity in wcpfc
                    ")

PS_Log_VMS <- get.fleet(PS_Log_VMS)

PS_Log_VMS <-
  PS_Log_VMS |> 
  mutate(fleet_id = ifelse(is.na(fleet_id), '', fleet_id),
         id = paste0(flag_id,fleet_id)) |> 
  filter(!paste0(flag_id,fleet_id) %in% c('PHPH','IDID','VN  ','IDDW','ID  ','VNVN') ) |> 
  select(yy, vessel_id)
#Im trusting this is knocking out Phillipino, Indonesian and Vietnam... umm..
# Yes, as vessel numbers could be drastically inflated, but they are very small vessels
# I believe that is the logic 

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
Vessels = data.table(dbGetQuery(db1,"SELECT *
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

Exponential_Growth_PreResearch <- (log(sum(Vessels$vessels[(Vessels$YY == 2024)])) - 
                                     log(Vessels$vessels[(Vessels$YY == 1972) & (Vessels$Fleet == 'Distant Water Fleet')]) ) / (2024 - 1972)

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
  
  annotate("text", x=2, y=280, label = "Exponential Growth from 2024 - 1972", family ="MyriadPro-Light", hjust = 0.0,colour = SPCColours("Gold"), size = 7) +              
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

 ggsave("Graphical_Output/Number_of_Purse_Seine_Vessels_Operating_within_the_WCPFC_with_Projection.png", height =16.13, width = 20.66, dpi = 165, units = c("cm"))


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
  
  annotate("text", x=2, y=280, label = "Exponential Growth from 2024 - 1972", family ="MyriadPro-Light", hjust = 0.0,colour = SPCColours("Gold"), size = 7) +              
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

 ggsave("Graphical_Output/Number_of_Purse_Seine_Vessels_Operating_within_the_WCPFC_NoTitles_with_Projection.png", height =16.13, width = 20.66, dpi = 165, units = c("cm"))


##
##
##

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
##    Save the datasets
##
save(Vessels, file = "Data_Output/Vessels.rda")


##
##    And we're done
##


