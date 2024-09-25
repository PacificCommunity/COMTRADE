##
##    Programme:  FAO_Analysis.r
##
##    Objective:  I'm testing a hypothesis that the Pacific tuna fisheries really kicked off 
##                with this piece of SPC work. https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/fa/fac64d3014aa2c05fc2deff5f9d2ff46.pdf?sv=2015-12-11&sr=b&sig=Ke20VzcwjxqBrp8Bnqc6aeQSrkNmJzMaEdsW8F8lPXI%3D&se=2025-01-21T12%3A28%3A41Z&sp=r&rscc=public%2C%20max-age%3D864000%2C%20max-stale%3D86400&rsct=application%2Fpdf&rscd=inline%3B%20filename%3D%22FishNews150_61_Judd.pdf%22
##                If the science which estimated the fisheries stocks in 1977 really did launch 
##                the pacific tuna industry, then I would expect to see as structural break in 
##                the long-term growth rate of fishing activity around the late 1970s, with growth 
##                accelerating after this period. 
##
##                After chatting with Peter Ellis, I'm going to test this hypothesis in a number 
##                of ways:
##                   (1)  Estimate a long run growth rate and test for structure change around 1977.
##                   (2)  Estimate a logistic regression which has initially expotential growth rates,
##                        then diminishing growth rates after a point of inflection.
##
##                For that hypothesis I’d suggest two things – one I using nlme and a logistic growth 
##                path etc with a dummy variable for all years from 1977 onwards as per the below; and 
##                the other would be to do some forecasts for future growth based on just the data up to 1976. 
##                I guess you could use nlme for this as well, or a simpler approach with forecast::auto.arima 
##                (it won’t predict the flattening that started in the 2000s as the catch got up towards 
##                its limits, but will probably just predict ongoing exponential growth forever). 
##                This second way of doing it is a great way of communicating, very intuitive for people 
##                if you can say “we pretended we were forecasting from 1976 onwards, and found that in 
##                fact the growth that happened is well within the range of what you would have expected anyway” 
##                (assuming that’s correct of course).
##
##    Author:     James Hogan, FAME - The Pacific Community (SPC)
##
##    Peer     :  Peter Ellis, SDD - The Pacific Community (SPC)
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
      load("Data_Intermediate/RAWDATA_ASFIS_spXXASFIS_sp_2023.rda")
      load("Data_Intermediate/RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized.rda")
      
      ASFIS <- data.table(RAWDATA_ASFIS_spXXASFIS_sp_2023)
      FAO   <- data.table(RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized)
                     
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
   ## Step 2: Calculate Core Tuna, and estimate a long term growth model
   ##
      Core_Tuna <- FAO[English_name %in% c("Albacore", "Black skipjack", "Skipjack tuna", "Bigeye tuna", "Yellowfin tuna", "Pacific bluefin tuna")]
      Core_Tuna <- FAO[English_name %in% c("Black skipjack", "Skipjack tuna")]
      Core_Tuna <- Core_Tuna[,
                          list(measurement_value = sum(measurement_value,na.rm = TRUE)),
                          by = .(measurement_unit, 
                                 Year = year(time_start), 
                                 source_authority,
                                 Source_Authority)]
      Core_Tuna$Fishery <- str_wrap(Core_Tuna$Source_Authority, width = 40)

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
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.1) +
             
             geom_vline(xintercept = c(1977), colour = SPCColours("Light_Blue"), alpha = 0.2, linewidth = 2) +
             annotate("text", x=1976, y=1750, label = "Catch Volumes pre-PSSAP",family ="MyriadPro-Light", hjust = 1.0, size = 5) +              
             annotate("text", x=1978, y=1750, label = "Catch Volumes post-PSSAP",family ="MyriadPro-Light", hjust = 0.0, size = 5) +              
             
             scale_y_continuous(breaks = seq(from = 0, to = 2500, by =250),
                                labels = scales::label_comma()) +
             scale_x_continuous(breaks = seq(from = 1950, to = 2025, by =5)) +
             scale_colour_manual(values = SPCColours(3:7)) + 
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

  