# Loading the libraries we need for this assignment
<<<<<<< HEAD

library(shiny)

library(tidyverse)

library(bslib)

library(thematic)

library(plotly)

library(DT)

library(ggmosaic)



# The darkly theme along with filter bars is obtained from garrett gmann who works for posit cloud : https://github.com/garrettgman/shiny-styling-demo/blob/main/finished-app/app.R

ggplot2::theme_set(ggplot2::theme_minimal())

thematic_shiny()



#Defining the UI theme provided by Garrett gman

ui <- navbarPage(
  
  title = "Digitalis Investigation Group (DIG) Trial",
  
  theme = bs_theme(bootswatch = "darkly",
                   
                   base_font = font_google("Lato"),
                   
                   heading_font = font_google("Raleway")),
  
  
  #Adding multiple panels to the shiny page : https://www.youtube.com/watch?v=V8ne6_F7lIM
  
  
  
  #Introduction Tab to the DIG trial
  
  tabPanel("Introduction to the DIG Trial", fluidPage(tags$head( tags$style(HTML("

.about-page {background-image: url('https://www.ucsfhealth.org/-/media/project/ucsf/ucsf-health/medical-tests/hero/digoxin-test-2x.jpg?rev=ae7227c07f7e4a338f1df1628fbcee89');background-size: cover;

background-position: center;

text-align: center;

color: white;

padding: 50px;}
 
#adding more
.about-header { display: flex;justify-content: center;align-items: center;gap: 20px;}

.about-header img { width: 70px;height: 70px;border-radius: 50%;border: 3px solid white;object-fit: cover;}

.intro-box {background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent background */color: white;padding: 20px;margin: 20px 0;border-radius: 10px; text-align: left;}

.image-container { display: flex;justify-content: center;align-items: center;gap: 20px;}

.circle-image { width: 350px;height: 350px;object-fit: cover;border-radius: 50%;}"))),
                                                      
                                                      
 
=======
library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(plotly)
library(DT)
library(ggmosaic)



# The darkly theme along with filter bars is obtained from garrett gmann who works for posit cloud : https://github.com/garrettgman/shiny-styling-demo/blob/main/finished-app/app.R
ggplot2::theme_set(ggplot2::theme_minimal())
thematic_shiny()

#Defining the UI theme provided by Garrett gman
ui <- navbarPage(
  title = "Digitalis Investigation Group (DIG) Trial",
  theme = bs_theme(bootswatch = "darkly",
                   base_font = font_google("Lato"),
                   heading_font = font_google("Raleway")),
  
  
  #Adding multiple panels to the shiny page : https://www.youtube.com/watch?v=V8ne6_F7lIM
  
  #Introduction Tab to the DIG trial
  tabPanel("Introduction to the DIG Trial", fluidPage(tags$head( tags$style(HTML("
.about-page {background-image: url('https://www.ucsfhealth.org/-/media/project/ucsf/ucsf-health/medical-tests/hero/digoxin-test-2x.jpg?rev=ae7227c07f7e4a338f1df1628fbcee89');background-size: cover;
background-position: center;
text-align: center;
color: white;
padding: 50px;}

<<<<<<< HEAD
.about-header { display: flex;justify-content: center;align-items: center;gap: 20px;}
.about-header img { width: 70px;height: 70px;border-radius: 50%;border: 3px solid white;object-fit: cover;}
.intro-box {background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent background */color: white;padding: 20px;margin: 20px 0;border-radius: 10px; text-align: left;}
.image-container { display: flex;justify-content: center;align-items: center;gap: 20px;}
.circle-image { width: 350px;height: 350px;object-fit: cover;border-radius: 50%;}"))),
                                                      
=======
>>>>>>> 3b630ed2f07b2619461d47e4eec8748e769d711f
>>>>>>> 1d4064cae512e9c1b3913919320214cbda06a17b
