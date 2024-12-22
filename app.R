# Loading the libraries we need for this assignment

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

