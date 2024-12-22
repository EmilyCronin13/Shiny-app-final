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
                   heading_font = font_google("Raleway")))

