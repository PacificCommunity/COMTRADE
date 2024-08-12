##    Programme:  themes.r
##
##    Objective:  Graphical themes 
##
##      Author:   James Hogan, 7 August 2021
##

#------------------Sense Partners Colour Palette-------------------------

# Create a vector to store the colours
James_Colours <- c(
   Red        = rgb(84, 15, 18, maxColorValue=255),
   Dark_Blue  = rgb(15, 13, 21, maxColorValue=255),
   Blue       = rgb( 8, 21, 34, maxColorValue=255),
   Light_Blue = rgb(54, 84, 96, maxColorValue=255),
   Teal       = rgb(14, 62, 54, maxColorValue=255),
   Brown      = rgb(24,  8,  9, maxColorValue=255),
   Yellow     = rgb(96, 91,  0, maxColorValue=255),
   Olive      = rgb(48, 60,  9, maxColorValue=255),
   Crimson    = rgb(68, 60, 17, maxColorValue=255)
)

# Create a function for easy reference to combinations of Sense_Colours
JamesColours <- function(x=1:12){
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
   as.vector(James_Colours[x])
}
