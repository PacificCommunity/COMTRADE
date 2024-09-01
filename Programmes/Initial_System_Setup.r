##
##    Programme:  Initial_System_Setup.r
##
##    Objective:  Part of R Fundamentals for Reading, Manipulating and Writing
##                data in R.
##
##                The main objective of this programme is to set up each person's systems
##                to use their corporate fonts
##
##
##    Author:     James Hogan, Sense Partners, 15 March 2022
##
##
   ##
   ##    Clear the memory and grab some case data and lab data
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load in some colours and functions 
   ##
      source("R/themes.r")

   ##
   ##    Under extrafont, we need to import the font library, but we only need to do that once.
   ##
      font_import()
   ##
   ##    Everyone time we use extrafont, we need to load the fonts like below
   ##
      loadfonts(device = "win")

   ##
   ##    Fonts available under extrafont
   ##
      fonts()

   ##
   ##    For example....
   ##

      a <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
                 ggtitle("Fuel Efficiency of 32 Cars") +
                 xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
                 theme(text=element_text(size=16,  family="Elephant"))
      print(a)

      
   ##
   ##    Unfortunately, IRD fonts have to be separately loaded by library showtext
   ##
      font_paths("C:\\Jameshogan_Ltd\\Sense Partners Associate\\IRD Training\\Training\\IRD_Training\\Fundamentals, Graphics and Reporting\\R")
      font_add("Cronos-Pro", regular = "Cronos-Pro_12459.ttf", bold = "Cronos-Pro-Bold_12435.ttf", italic = "Cronos-Pro-Italic_12440.ttf")
      font_add("Proxima Nova", regular = "Proxima Nova Font.otf")
      font_add("Verdana", regular = "Verdana.ttf", bold = "verdanab.ttf")

   ##
   ##    To use the IRD fonts, we need to turn library showtext on
   ##       ... like this ...
   ##
   ##    And to go back to windows fonts, we need to turn showtext off
   ##
      showtext_begin()
      
      ##
      ##    In ggplot with IRD Fonts
      ##
         a <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
                     ggtitle("Fuel Efficiency of 32 Cars") +
                     xlab("Weight (x1000 lb)") + 
                     ylab("Miles per Gallon") +
                     theme(text=element_text(size=16,  family="Cronos-Pro"))
         print(a)

      showtext_end()

   ##
   ##    Close IRD Fonts, and now we use windows fonts, again
   ##       ... like this ...

      a <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
                  ggtitle("Fuel Efficiency of 32 Cars") +
                  xlab("Weight (x1000 lb)") + 
                  ylab("Miles per Gallon") +
                  theme(text=element_text(size=16,  family="Ink Free"))
      print(a)
##
##   End of programme
##
