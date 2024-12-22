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

.about-header { display: flex;justify-content: center;align-items: center;gap: 20px;}
.about-header img { width: 70px;height: 70px;border-radius: 50%;border: 3px solid white;object-fit: cover;}
.intro-box {background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent background */color: white;padding: 20px;margin: 20px 0;border-radius: 10px; text-align: left;}
.image-container { display: flex;justify-content: center;align-items: center;gap: 20px;}
.circle-image { width: 350px;height: 350px;object-fit: cover;border-radius: 50%;}"))),
                                                      
#Next we are adding our giphs and images to our app method is curtesy of the posit tutorial for images : https://shiny.posit.co/r/articles/build/images/         
#All giphs included are from GIPHY and are not coprighted: https://www.youtube.com/watch?v=Ka2pWqXS1WA
                                                      
                                                      
 div(class = "about-page",
 div( class = "about-header",img(src = "https://media.giphy.com/media/ToMjGpAibV4AwKryEXS/giphy.gif"),h1("Introduction to the Digitalis Investigation Group (DIG) Trial"),img(src = "https://media.giphy.com/media/l3YSj6Oirgkb18AkE/giphy.gif") ),
 div( class = "intro-box", #Add the text to the box we generated - intro to the dig study. The summary text is from the actual DIG study : https://biolincc.nhlbi.nih.gov/studies/dig/
                                                               
p("The Digitalis Investigation Group (DIG) trial was the first large simple trial conducted by the National Heart, Lung, and Blood Institute in conjunction with the Department of Veterans Affair."),
p("The Digitalis Investigation Group (DIG) study was a prospective, randomized clinical trial involving 7,788 patients with HF randomized to digoxin or placebo and followed for an average of 37 months."),
                                                               p("Interactions between age and the following clinical outcomes were examined: total mortality, all-cause hospitalizations, HF hospitalizations, the composite of HF death or HF hospitalizations, hospitalization for suspected digoxin toxicity and withdrawal from therapy because of side effects."),
                                                               p("A total of 302 centers in the United States and Canada enrolled 7,788 patients between February 1991 and September 1993.")),
                                                          
                                                          div(class = "image-container",img(src = "https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExazBtamw1NWtjeHVoa2Z1ejJybGxyMnQ2czhjejVrNjRxcXYyZHR0NyZlcD12MV9naWZzX3NlYXJjaCZjdD1n/3o85xm0pDVY4EkKdFe/giphy.gif", 
                                                                                            class = "circle-image"))))), #This is the gif but in a circle form 
  
  
  #The next tab is the visualization tab which includes the filter slider from garrett gmans tutorial
  
  tabPanel("Data Exploration: Visualisation of DIG Trial outcomes",
           fluidPage( div(class = "visualization-info",h3("Visual Data Exploration: Results of DIG Trial"),
                          p("This section displays various visualizations based on the Digitalis Investigation Group (DIG) trial data. You can explore the relationship between different clinical variables, treatment groups, and patient characteristics through scatter plots, mosaic plots, and bar charts. Use the controls to filter data by age, gender, and other factors.")),
                      sidebarLayout( sidebarPanel(sliderInput("age_range", "Age Range", min = 30, max = 100, value = c(30, 100)),
                                                  selectizeInput("gender", "Gender", choices = c("All", "Male", "Female"), selected = "All"),
                                                  selectizeInput("variable", "Variable",choices = c("BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS"), selected = "BMI"),
                                                  
                                                  #Download button so user can get the filtered Data : https://shiny.posit.co/r/reference/shiny/1.3.1/downloadbutton
                                                  downloadButton("download_data", "Download DIG Trial Data"),
                                                  
                                                  #Add a logo: The DIG group logo : code from https://forum.posit.co/t/how-to-add-a-my-company-logo-in-r-shiny-material/35256
                                                  
                                                  tags$img(src = "https://www.digitalis-cdc.com/digitalisshowcase/ui/img/digitalis-flat-1200x800.png", 
                                                           style = "width: 100%; height: auto; margin-top: 20px;")),
                                     
                                     #Main sub panel for the visualizations 
                                     mainPanel(tabsetPanel(
                                       tabPanel("Interactive Scatter Plot", plotlyOutput("scatter_plot")),
                                       tabPanel("Mosaic Plot", plotlyOutput("mosaic_plot")),
                                       tabPanel("Static Bar Charts",fluidRow(column(6, plotlyOutput("mortality_plot")),column(6, plotlyOutput("cvd_mortality_plot")) )),
                                       tabPanel("Static Survival Plot", #Put in small description about survival plot as it gives a broad overview of study
                                                fluidPage(div(class = "static-survival-plot",h3("Static Survival Plot"),
                                                              p("This plot shows the risk of mortality over time by treatment group and the presence or absence of cardiovascular disease (CVD). It helps to visualize how CVD affects the mortality risk in patients under different treatments."),
                                                              img(src = "survival.png", style = "width: 100%; height: auto; margin-top: 20px;"))))))))),
  
  #Summary panel page 
  tabPanel("Summary of Findings",
           fluidPage(tags$head(tags$style(HTML(".summary-table-section {background-color: #2C3E50;padding: 20px;color: white;border-radius: 10px;margin-bottom: 20px;}
.summary-title {text-decoration: underline;font-size: 1.8em;}
.summary-table-container {background-color: white;border-radius: 10px;padding: 10px;}
.info-box {background-color: #2C3E50;color: white;border: 1px solid #ddd;border-radius: 10px;padding: 15px;margin-bottom: 20px;display: flex;align-items: center;gap: 10px;}
.info-icon {font-size: 20px;color: #3498db;}"))),
                     
                     div(class = "summary-table-section",
                         h2("Summary of Findings of DIG Trial", class = "summary-title", style = "text-align: center;")),
                     div(class = "info-box",
                         tags$span(class = "info-icon", "ℹ️"),
                         p("This is the summary data collected during this study for both Digoxin and Placebo groups. Increasing age is associated with progressively worse clinical outcomes in patients with HF. However, the beneficial effects of digoxin in reducing all-cause admissions, HF admissions, and HF death or hospitalization are independent of age. Thus, digoxin remains a useful agent to the adjunctive treatment of HF due to impaired left ventricular systolic function in patients of all ages.")),
                     div(
                       class = "summary-table-container",
                       DTOutput("summary_table"))))) #Closing all brackets

# Now defining our server 
server <- function(input, output, session) {
  
  #Now adding summary table data that you can then filter - filtering a reactive table code from stackoverflow adapted for this app - https://stackoverflow.com/questions/72398800/filtering-a-reactive-table-in-shiny
  dig.df <- read.csv("DIG.csv") %>%
    mutate(SEX = factor(SEX, labels = c("Male", "Female")),
           TRTMT = ifelse(TRTMT == 0, "Placebo", "Digoxin"),
           DEATH_BINARY = ifelse(DEATH == "Death", 1, 0))
  
  #Making the table interactive so you can search : Using datatables https://shiny.posit.co/r/articles/build/datatables/
  filtered_data <- reactive({
    data <- dig.df %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2])
    if (input$gender != "All") {
      data <- data %>% filter(SEX == input$gender)}
    if (nrow(data) == 0) {
      showNotification("No data available for selected filters.", type = "warning")} # Adding a warning if someone tries to search for something else
    data})
  
  #Output summary to user
  output$summary_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = "Bfrtip"))})
  
  #Visualizing our graphs pink for female and blue for male
  #Scatterplot from assignment 4
  output$scatter_plot <- renderPlotly({
    gg <- filtered_data() %>%
      ggplot(aes_string(x = "AGE", y = input$variable, color = "SEX")) +
      geom_point(alpha = 1, size = 3) +
      scale_color_manual(values = c("Female" = "#FF69B4", "Male" = "#BFEFFF")) +
      labs(title = paste("Age vs", input$variable), x = "Age", y = input$variable)
    ggplotly(gg)})
  
  #Mosaic plot from assignment 4
  output$mosaic_plot <- renderPlotly({
    mosaic <- ggplot(filtered_data()) +
      geom_mosaic(aes(x = product(TRTMT, SEX), fill = SEX)) + scale_fill_manual(values = c("Male" = "#BFEFFF", "Female" = "#FF69B4")) +
      labs(title = "Distribution of Sex within Treatment Groups",
           x = "Treatment Group",
           y = "Proportion of Patients",
           fill = "Sex") +
      theme_minimal()
    ggplotly(mosaic)})
  
  #Patient mortality
  output$mortality_plot <- renderPlotly({
    mortality <- dig.df %>%
      group_by(TRTMT, DEATH) %>%
      summarise(Count = n(), .groups = "drop")
    plot_ly(mortality, x = ~factor(TRTMT, levels = c("Placebo", "Digoxin")),
            y = ~Count,
            color = ~factor(DEATH, labels = c("Alive", "Dead")),
            type = 'bar',
            text = ~Count) %>%
      layout(title = "Mortality by Treatment Group",
             barmode = "group",
             xaxis = list(title = "Treatment Group"),
             yaxis = list(title = "Number of Patients"))})
  
  #Adding mortality plot for CVD
  output$cvd_mortality_plot <- renderPlotly({
    cvd <- dig.df %>%
      group_by(CVD, DEATH) %>%
      summarise(Count = n(), .groups = "drop")
    plot_ly(cvd, x = ~factor(CVD, labels = c("No CVD", "CVD")),
            y = ~Count,
            color = ~factor(DEATH, labels = c("Alive", "Dead")),
            type = 'bar',
            text = ~Count) %>%
      layout(title = "CVD Influence on Mortality",
             barmode = "group",
             xaxis = list(title = "Cardiovascular Disease"),
             yaxis = list(title = "Number of Patients"))})
  
  #Downloading data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Filtered_DIG_Data-", Sys.Date(), ".csv", sep = "")},
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)})}

# Run the final app
shinyApp(ui, server)

