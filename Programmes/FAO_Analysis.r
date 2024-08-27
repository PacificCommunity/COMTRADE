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
      Core_Tuna <- Core_Tuna[,
                          list(measurement_value = sum(measurement_value,na.rm = TRUE)),
                          by = .(measurement_unit, 
                                 Year = year(time_start), 
                                 source_authority,
                                 Source_Authority)]
   
      ggplot(Core_Tuna, 
             aes(x = Year, 
                 y = measurement_value,
                 colour = Source_Authority))  + 
#             facet_grid(. ~ Measure) +
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.2) +
             
             geom_vline(xintercept = c(1977, 1981.5, 1994.25)) +
             geom_text(aes(x=1976, y=2750000, label="1997",  family ="Open Sans"),show_guide = F, size=3.5)+
             geom_text(aes(x=1983, y=2750000, label="1981",  family ="Open Sans"), show_guide = F, size=3.5)+
             geom_text(aes(x=1996, y=2750000, label="1994",  family ="Open Sans"), show_guide = F, size=3.5)+
   
             scale_y_continuous(labels = comma) +
#             scale_colour_manual(values = JamesColours()) + 
             labs(title = "World Tuna Catch Volumes") +
             xlab("\nYear\n") +
             ylab("Total Metrics Tonnes\n") +
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
                   legend.text   = element_text(size = 10, family = "Open Sans Light"),
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
   
   ##
   ## Step 3: Growth and Inflections
   ##
      Volume <- data.table::dcast(Core_Tuna,
                                  measurement_unit + Year ~ source_authority)
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
             geom_vline(xintercept = Volume$Year[Breaks$breakpoints][2]) +
             geom_vline(xintercept = Volume$Year[Breaks$breakpoints][3]) +
             
             scale_y_continuous(labels = comma) +
#             scale_colour_manual(values = JamesColours()) + 
             labs(title = "Total Tuna Catch Volumes") +
             xlab("\nYear\n") +
             ylab("Total Metrics Tonnes\n") +
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
                   legend.text   = element_text(size = 10, family = "Open Sans Light"),
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
#             scale_colour_manual(values = JamesColours()) + 
             labs(title = "Total Tuna Catch Volumes") +
             xlab("\nYear\n") +
             ylab("Total Metrics Tonnes\n") +
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
                   legend.text   = element_text(size = 10, family = "Open Sans Light"),
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

  