##
##    Programme:  Present_Value_of_Harvest_Volumes.r
##
##    Objective:  SPCs Skipjack Survey and Assessment Programme (SSAP) ran from 1977 – 1981 and opened up the 
##                Pacific skipjack fisheries. Consequently, large volumes of fishing vessels came in 
##                and the volume of fish that has since been extract is huge. One of the questions Neville wants
##                me to answer is what's the present value of the historical volumes, and how does that compare
##                to the cost of the science which opened the tuna fishery.
##
##                The same issues also pop up for the value of the tuna catch to the member countries - can we 
##                go back in time and estimate those?
##
##                What makes this a complicated problem is that the volume data goes back to 1972, but doesn't come
##                with any value data. So I'm struggling for a metric that converts volumes into values.
##
##                Solutions?
##                1. Look at the long-term average price metrics from the FFA and extrapolate the price trend back
##                   in time. This assumes that the long term average price trend was stable. When it probably isnt and 
##                   wasnt.
##
##                2. Scour the internet for ANY historical measures of the value of sales from the tuna fisheries
##                   and interpolate a spline trend through those points. At least we've got things to interpolate a 
##                   line through, even if we have no idea how the historical data matches to the current day metrics.
##
##                3. Estimate the relationship between FFA average tuna prices, and its secondary product price, like tinned
##                   tuna CPI or the PPI. This assumes "pass-through" effects: that the value of rawtuna pass-through 
##                   into the value of tinned tuna, and prices changes in raw inputs are carried into price changes in
##                   tinned outputs. But if a relationship can be found, then I can use the historical tinned CPI to 
##                   estimate the raw tuna prices.
##
##                   Again, a long bow maybe, given the price dynamics - tuna cpi/ppi decreased because pacific tuna flooded 
##                   the market. If anything it meant that historical caught tuna prices should have been falling, 
##                   offsetting the benefits of volume growth.
##
##
##                   https://www.ffa.int/download/economic-development-indicators-and-statistics/
##
##
##    Author:     James Hogan, FAME - The Pacific Community (SPC)
##
##    Peer     :  
##    Reviewer :
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
      ##
      ##    FAO data on historical volumes
      ##
         load("Data_Intermediate/RAWDATA_ASFIS_spXXASFIS_sp_2023.rda")
         load("Data_Intermediate/RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized.rda")

         ASFIS <- data.table(RAWDATA_ASFIS_spXXASFIS_sp_2023)
         FAO   <- data.table(RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized)

      ##
      ##    FFA data on current volumes and values
      ##
         load('Data_Intermediate/FFASummaryData.rda')
         load('Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2022.rda')
         
      ##
      ##    Grab data on CPI
      ##
         load('Data_Intermediate/RAWDATA_Spliced IndexXXSplited_Price_Indexes.rda')
         Splited_Price_Indexes <- `RAWDATA_Spliced IndexXXSplited_Price_Indexes`
         Splited_Price_Indexes <- Splited_Price_Indexes[,c("X.12","X.13")]
         names(Splited_Price_Indexes) <- c("TimePeriod", "Composite_Price")
         Splited_Price_Indexes$TimePeriod <- as.Date(Splited_Price_Indexes$TimePeriod, "%d/%m/%Y")
         Splited_Price_Indexes$Composite_Price <- as.numeric(Splited_Price_Indexes$Composite_Price)
         Splited_Price_Indexes <- Splited_Price_Indexes[!is.na(Splited_Price_Indexes$TimePeriod),]


                     
   ##
   ## Step 1: Clean up and enrich the FAO Data
   ##
      FAO$time_start <- as.Date(FAO$time_start, "%Y-%m-%d")
      FAO$time_end   <- as.Date(FAO$time_end, "%Y-%m-%d")
      FAO$measurement_value <- as.numeric(FAO$measurement_value)
      FAO$gear_type <- as.factor(FAO$gear_type)

      FAO <- merge(FAO,
                   ASFIS[, c("ISSCAAP_Group","Taxonomic_Code","Alpha3_Code","Scientific_Name","English_name")],
                   by.x = c("species"),
                   by.y = c("Alpha3_Code"),
                   all.x= TRUE)

      Source <- data.table(source_authority = c('UN-FAO','IRD','CCSBT','IATTC','ICCAT','IOTC','WCPFC'),
                           Source_Authority = c('Food and Agriculture Organization of the United Nations',
                                                'French National Research Institute for Sustainable Development',
                                                'Commission for the Conservation of Southern Bluefin Tuna',
                                                'Inter-American Tropical Tuna Commission',
                                                'International Commission for the Conservation of Atlantic Tunas',
                                                'Indian Ocean Tuna Commission',
                                                'Western and Central Pacific Fisheries Commission'))
      FAO <- merge(FAO,
                   Source,
                   by = c("source_authority"))
                   
   ##
   ## Calculate Core Tuna
   ##
      FAO_Core_Tuna <- FAO[English_name %in% c("Albacore", "Black skipjack", "Skipjack tuna", "Bigeye tuna", "Yellowfin tuna")]
      FAO_Core_Tuna$Measure <- ifelse(FAO_Core_Tuna$English_name %in% c("Black skipjack", "Skipjack tuna"), "Skipjack",
                               ifelse(FAO_Core_Tuna$English_name %in% c("Bigeye tuna"), "Bigeye",
                               ifelse(FAO_Core_Tuna$English_name %in% c("Yellowfin tuna"), "Yellowfin",FAO_Core_Tuna$English_name)))
                               
      FAO_Core_Tuna <- FAO_Core_Tuna[source_authority == 'WCPFC',
                                      list(FAO_Catch_Volume = sum(measurement_value,na.rm = TRUE)),
                                      by = .(Year = year(time_start), 
                                             Measure)]

   ##
   ## Calculate FFA Tuna
   ##
      Catch_Value  <- FFASummaryData[["Summary of catch value"]]
      Catch_Volume <- FFASummaryData[["Summary of catch"]]

      Catch_Value  <- data.table(Catch_Value[Catch_Value$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])
      Catch_Volume <- data.table(Catch_Volume[Catch_Volume$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])

      Value_by_Species  <- Catch_Value[Catch_Value$Data_Row == "5.2 VALUE OF CATCH BY SPECIES",c("Measure","Year","value")]
      Volume_by_Species <- Catch_Volume[Catch_Volume$Data_Row == "4.2 CATCH BY SPECIES",c("Measure","Year","value")]
      
      Value_by_Species$Value   <- (Value_by_Species$value)*1000
      Volume_by_Species$Volume <- Volume_by_Species$value
      
      FFA_Core_Tuna <-merge(Value_by_Species[,c("Measure","Year","Value")],
                            Volume_by_Species[,c("Measure","Year","Volume")],
                            by = c("Measure","Year"))


   ##
   ##    How well does the FFA and FAO metrics line up?
   ##
   ##       Looks pretty good.
   ##
   ##
      FAO_FFA_Comparison <- merge(FAO_Core_Tuna,
                                  FFA_Core_Tuna,
                                  by = c("Measure","Year"),
                                  all= TRUE)

      FAO_FFA_Comparison <- data.table::melt(FAO_FFA_Comparison,
                                             id.vars = c("Measure","Year"),
                                             measure.vars = c("FAO_Catch_Volume","Value","Volume"))
      FAO_FFA_Comparison$variable <- ifelse(as.character(FAO_FFA_Comparison$variable) == "Volume", "FFA_Volume", as.character(FAO_FFA_Comparison$variable))


      showtext_auto()

      ggplot(FAO_FFA_Comparison, 
#      ggplot(FAO_FFA_Comparison[variable != "Value"], 
             aes(x = Year, 
                 y = value/1000,
                 colour = variable))  + 
             geom_line() +
#             geom_point(alpha = 0.1) +
             facet_wrap( ~ Measure,scales = "free") +
             
             scale_colour_manual(values = SPCColours(c(1,3,5))) + 
             labs(title = "Comparison in Tuna Catch Volumes (FFA vs FAO), and FFA Value",
                  subtitle = "\nWestern and Central Pacific Fisheries Commission Area\n",
                  caption  = "Data Sources: FFA, WCPFC-CA tuna fisheries 2023 (1).xlsx, https://www.ffa.int/download/wcpfc-area-catch-value-estimates/ \nUNFAO https://zenodo.org/records/11410529") +
             ylab("Metrics Tonnes\n(000)") +
             xlab("Year") +
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
#    ggsave("Graphical_Output/FAO_FFA_Comparison_With_Titles.png", height =(.77*16.13), width = (.77*20.66), dpi = 165, units = c("cm"))
    ggsave("Graphical_Output/FAO_FFA_Comparison_With_Titles.png", height =(1.5*16.13), width = (1.5*20.66), dpi = 165, units = c("cm"))
    
      ggplot(FAO_FFA_Comparison[variable != "Value"], 
             aes(x = Year, 
                 y = value/1000,
                 colour = variable))  + 
             geom_line() +
#             geom_point(alpha = 0.1) +
             facet_wrap( ~ Measure,scales = "free") +
             
             scale_colour_manual(values = SPCColours(c(1,5))) + 
             ylab("Metrics Tonnes\n(000)") +
             xlab("Year") +
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
    ggsave("Graphical_Output/FAO_FFA_Comparison_Without_Titles.png", height =(.77*16.13), width = (.77*20.66), dpi = 165, units = c("cm"))


   ##
   ##    Try solution 1: Look at the long-term average price metrics from the FFA and extrapolate the price trend back
   ##                    in time using the UNFAO data.
   ##
   ##                    Goah... I don't feel bold using these prices to estimate any trend to extrapolate back...
   ##
   ##                    I'm also deeply suspicious about these prices the FFA are using to estimate catch value. I don't think
   ##                    they are ... "right"
   ##
      FAO_FFA_Comparison <- data.table::dcast(FAO_FFA_Comparison,
                                              Measure + Year ~ variable)
                                              
      FAO_FFA_Comparison$Average_Price_PerTonne <- (FAO_FFA_Comparison$Value*1000000) / FAO_FFA_Comparison$FFA_Volume  
      
      ggplot(FAO_FFA_Comparison[!is.na(Average_Price_PerTonne)], 
             aes(x = Year, 
                 y = Average_Price_PerTonne))  + 
             geom_line() +
#             geom_point(alpha = 0.1) +
             facet_wrap( ~ Measure,scales = "free") +
             scale_y_continuous(labels = scales::label_dollar()) +
             
             scale_colour_manual(values = SPCColours(c(1,5))) + 
             ylab("Average Price\nPer Tonne") +
             xlab("Year") +
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


























     showtext_auto()
      ##
      ##    Lets save two pictures - one with titles and one without
      ##       First with titles for showing people
      ##
      ggplot(Core_Tuna[Year > 1949], 
             aes(x = Year, 
                 y = measurement_value/1000,
                 colour = Fishery))  + 
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.1) +
             
             geom_vline(xintercept = c(1977), colour = SPCColours("Light_Blue"), alpha = 0.2, linewidth = 2) +
             annotate("text", x=1976, y=1750, label = "Catch Volumes pre-PSSAP",family ="MyriadPro-Light", hjust = 1.0) +              
             annotate("text", x=1978, y=1750, label = "Catch Volumes post-PSSAP",family ="MyriadPro-Light", hjust = 0.0) +              
             
             scale_y_continuous(breaks = seq(from = 0, to = 2500, by =250),
                                labels = scales::label_comma()) +
             scale_x_continuous(breaks = seq(from = 1950, to = 2025, by =5)) +
             scale_colour_manual(values = SPCColours(3:7)) + 
             labs(title = "Global Annual Skipjack Tuna Catch Volumes",
                  caption  = "Data Source: UNFAO https://zenodo.org/records/11410529") +
             xlab("") +
             ylab("Metrics Tonnes\n(000)") +
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
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")
    ggsave("Graphical_Output/World_Volumes_Titles.png", height =16.13, width = 20.66, dpi = 165, units = c("cm"))
      ##
      ##       Now without titles for including in paper - Word scales it down by 23%
      ##
      Core_Tuna$Fishery <- str_wrap(Core_Tuna$Source_Authority, width = 25)
      
      ggplot(Core_Tuna[Year > 1949], 
             aes(x = Year, 
                 y = measurement_value/1000,
                 colour = Fishery))  + 
#             facet_grid(. ~ Measure) +
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.1) +
             
             geom_vline(xintercept = c(1977), colour = SPCColours("Light_Blue"), alpha = 0.2, linewidth = 2) +
             annotate("text", x=1976, y=1750, label = "Catch Volumes pre-PSSAP",family ="MyriadPro-Light", hjust = 1.0, size = 7) +              
             annotate("text", x=1978, y=1750, label = "Catch Volumes post-PSSAP",family ="MyriadPro-Light", hjust = 0.0, size = 7) +              
             # geom_text(aes(x=1978, y=2750, label="First PSSAP Project",  family ="MyriadPro-Light"), size=10)+
             # geom_text(aes(x=1983, y=2750000, label="1981",  family ="MyriadPro-BoldItalics"), show_guide = F, size=3.5)+
             # geom_text(aes(x=1996, y=2750000, label="1994",  family ="MyriadPro-BoldItalics"), show_guide = F, size=3.5)+
             
             scale_y_continuous(breaks = seq(from = 0, to = 2500, by =250),
                                labels = scales::label_comma()) +
             scale_x_continuous(breaks = seq(from = 1950, to = 2025, by =5)) +
             scale_colour_manual(values = SPCColours(3:7)) + 
#             labs(title = "Global Annual Tuna Catch Volumes",
#                  caption  = "Data Source: UNFAO https://zenodo.org/records/11410529") +
             xlab("") +
             ylab("Metrics Tonnes\n(000)") +
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
    ggsave("Graphical_Output/World_Volumes_WithoutTitles.png", height =(.77*16.13), width = (.77*20.66), dpi = 165, units = c("cm"))
  
   ##
   ## Step 3: Growth and Inflections
   ##
      Volume <- data.table::dcast(Core_Tuna,
                                  measurement_unit + Year ~ source_authority,
                                  value.var = "measurement_value")
      Volume <- Volume[!is.na(WCPFC)]
   ##
   ##    Estimate a bog standard long run growth model, and then calculate its breakpoints 
   ##
      OLS <- lm(log(WCPFC) ~ Year,
                data = Volume)
      summary(OLS)
         

      ocus <- efp(log(WCPFC) ~ Year, 
                  type="OLS-CUSUM", 
                  data=Volume)
      plot(ocus)
      
        me <- efp(log(WCPFC) ~ Year, 
                  type="ME", 
                  data=Volume, 
                  h=0.2)
      plot(me)
   ##
   ##    Date the growth breaks
   ##
      Breaks <- breakpoints(log(WCPFC) ~ Year, 
                            data=Volume)
      
      Volume[Breaks$breakpoints]


      ggplot(Volume, 
             aes(x = Year, 
                 y = WCPFC))  + 
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.2) +
             
             geom_vline(xintercept = Volume$Year[Breaks$breakpoints][1]) +
             #geom_vline(xintercept = Volume$Year[Breaks$breakpoints][2]) +
             #geom_vline(xintercept = Volume$Year[Breaks$breakpoints][3]) +
             
             scale_y_continuous(labels = comma) +
#             scale_colour_manual(values = SPCColours()) + 
             labs(title = "Total Tuna Catch Volumes") +
             xlab("\nYear\n") +
             ylab("Total Metrics Tonnes\n") +
             theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "Open Sans Semibold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "Open Sans Light"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Blue"),                   family = "Raleway SemiBold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), hjust = 0.5, family = "Raleway SemiBold"),
                   plot.caption  = element_text(size = 11, colour = SPCColours("Red"),     hjust = 1.0, family = "Open Sans"),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red"),     hjust = 0.0, face = "italic" ),
                   axis.title    = element_text(size = 14, colour = SPCColours("Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Blue"),  angle = 90),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Red"), angle = 00),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")

   ##
   ##    Estimate the short run growth functions
   ##
      Volume$`1950-1959` <- ifelse(Volume$Year <= 1959, 1, 0)
      Volume$`1960-1992` <- ifelse(((Volume$Year > 1959) & (Volume$Year <= 1992)), 1, 0)
      Volume$`1992-2006` <- ifelse(((Volume$Year > 1992) & (Volume$Year <= 2006)), 1, 0)
      Volume$`2006-Current` <- ifelse(Volume$Year > 2006, 1, 0)
      Volume$Constrained_Logistic <- ifelse(Volume$Year > 1977, 1,0)
      
      Short_Run <- lm(log(WCPFC) ~ `1960-1992`*Year + `1992-2006`*Year + `2006-Current`*Year,
                      data = Volume)
      summary(Short_Run)
         
     ##
     ##  Test for Autocorrelation:  Extract the residuals and check out their autocorrelation function
     ##        Autocorrelation in errors, looks like a AR(1) process
     ##
     ##     Autocorrelation underestimates the true variance of the estimates:  t-values are overstated 
     ##
         acf(OLS$residuals)
         pacf(OLS$residuals)
         dwtest(OLS)
  
     ##
     ##     Try Peter's recommendation on estimating a logistic regression
     ##
         fit <- nls(WCPFC ~ SSlogis(Year, a, b, c), data = Volume)
         summary(fit)
         p <- coef(fit)


         
         fit <- nls(WCPFC ~ SSlogis(Year*Constrained_Logistic, a, b, c), data = Volume)
         summary(fit)
         c <- coef(fit)



         Volume$Unconstrained_Logistic <- SSlogis(Volume$Year,p["a"],p["b"],p["c"])
         Volume$Constrained_Logistic   <- SSlogis(Volume$Year,c["a"],c["b"],c["c"])
         Volume <- data.table::melt(Volume,
                                   id.var = c("Year"),
                                   measure.vars = c("Unconstrained_Logistic","Constrained_Logistic", "WCPFC"))
                              
      ggplot(Volume, 
             aes(x = Year, 
                 y = value,
                 colour = variable))  + 
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.2) +
             
             geom_vline(xintercept = p["b"]) +
             
             scale_y_continuous(labels = comma) +
#             scale_colour_manual(values = SPCColours()) + 
             labs(title = "Total Skipjack Tuna Catch Volumes") +
             xlab("\nYear\n") +
             ylab("Total Metrics Tonnes\n") +
             theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "Open Sans Semibold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "Open Sans Light"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Blue"),                   family = "Raleway SemiBold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), hjust = 0.5, family = "Raleway SemiBold"),
                   plot.caption  = element_text(size = 11, colour = SPCColours("Red"),     hjust = 1.0, family = "Open Sans"),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red"),     hjust = 0.0, face = "italic" ),
                   axis.title    = element_text(size = 14, colour = SPCColours("Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Blue"),  angle = 90),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Red"), angle = 00),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")

  