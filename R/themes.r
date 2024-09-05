##    Programme:  themes.r
##
##    Objective:  Graphical themes 
##
##      Author:   James Hogan, FAME - SPC, 30 August 2024
##

#------------------SPC Colour Palette-------------------------

font_paths(paste0(getwd(), "/R"))
font_add(family = "MyriadPro-Bold",       regular = "/R/MYRIADPRO-BOLD.OTF")
font_add(family = "MyriadPro-Light",       regular = "/R/MyriadPro-Light.OTF")
font_add(family = "MyriadPro-Regular",     regular = "/R/MYRIADPRO-REGULAR.OTF")
font_add(family = "MyriadPro-BoldItalics", regular = "/R/MYRIADPRO-BOLDCONDIT.OTF")
font_add(family = "MyriadPro-Italics",     regular = "/R/MYRIADPRO-CONDIT.OTF")



# Create a vector to store the colours
SPC_Colours <- c(
   Light_Blue = rgb(0,  176, 202, maxColorValue=255),
   Dark_Blue  = rgb(0,   70, 173, maxColorValue=255),
   Green      = rgb(88, 166,  24, maxColorValue=255),
   Gold       = rgb(223,122,   0, maxColorValue=255),
   Red        = rgb(198, 12,  48, maxColorValue=255),
   Purple     = rgb(124, 16, 154, maxColorValue=255)
)

# Create a function for easy reference to combinations of Sense_Colours
SPCColours <- function(x=1:12){
   if(x[1]=="Duo1")  x <- c(1,2)
   if(x[1]=="Duo2")  x <- c(2,3)
   if(x[1]=="Duo3")  x <- c(2,5)
   if(x[1]=="Duo4")  x <- c(3,5)
   if(x[1]=="Trio1") x <- c(1,2,3)
   if(x[1]=="Trio2") x <- c(1,3,5)
   if(x[1]=="Trio3") x <- c(1,2,4)
   if(x[1]=="Quad1") x <- c(1,2,3,4)
   if(x[1]=="Quad2") x <- c(1,2,4,5)
   if(x[1]=="Quad3") x <- c(1,3,4,5)
   if(x[1]=="Quad4") x <- c(1,2,3,5)
   as.vector(SPC_Colours[x])
}

gg.theme <- theme(axis.line = element_line(color="black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(size=16),
                  axis.title = element_text(size=18),
                  strip.background =element_rect(fill="white"),
                  strip.text = element_text(size=14),
                  legend.box.background = element_rect(colour = "black"),
                  legend.background = element_blank(),
                  legend.text = element_text(size=14),
                  legend.title=element_blank(),
                  legend.position = 'top',
                  panel.background = element_rect(fill = NA, color = "black"))  