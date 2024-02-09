#Installing and reading packages
#install.packages("shinyjs")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("shinydashboard")
library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(shinydashboard)
library(plotly)
library(shinyjs)
#getting work directory
#getwd()

#reading csv file
Covid19 <- read_csv("covid19.csv")


#print first 6 rows of dataset
head(Covid19)

#Number of observations
nrow(Covid19)

#making sure data doesn't contain Nulls
is.null(Covid19$Date) 
is.null(Covid19$Continent_Name)
is.null(Covid19$Two_Letter_Country_Code)
is.null(Covid19$Country_Region)
is.null(Covid19$Province_State)
is.null(Covid19$positive)
is.null(Covid19$hospitalized)
is.null(Covid19$recovered)
is.null(Covid19$death)
is.null(Covid19$total_tested)
is.null(Covid19$hospitalizedCurr)
is.null(Covid19$daily_tested)
is.null(Covid19$daily_positive)
is.null(Covid19$active)

#Checking for Duplicates
sum(duplicated(Covid19))

Covid19$Country_Region[Covid19$Country_Region == "Israel"] <- "Palestine"

#Some statistics
summary(Covid19)

str(Covid19)

print("Variance : ")
cat("Positive cases : ",sd(Covid19$positive))
print("")
cat("Hospitalized cases : ",sd(Covid19$hospitalized))
print("")
cat("Recovered cases : ",sd(Covid19$recovered))
print("")
cat("Death cases : ",sd(Covid19$death))
print("")
cat("Total tested : ",sd(Covid19$total_tested))
print("")
cat("Daily tested : ",sd(Covid19$daily_tested))
print("")
cat("Daily positive : ",sd(Covid19$daily_positive))
print("")
cat("Hospitalized on that day : ",sd(Covid19$hospitalizedCurr))
print("")
cat("Active cases : ",sd(Covid19$active))
print("")


# Scale data between 0 and 1
#Covid19$positive <- scale(Covid19$positive, center = min(Covid19$positive), scale = diff(range(Covid19$positive)))
#Covid19$hospitalized <- scale(Covid19$hospitalized, center = min(Covid19$hospitalized), scale = diff(range(Covid19$hospitalized)))
#Covid19$death <- scale(Covid19$death, center = min(Covid19$death), scale = diff(range(Covid19$death)))
#Covid19$daily_tested <- scale(Covid19$daily_tested, center = min(Covid19$daily_tested), scale = diff(range(Covid19$daily_tested)))
#Covid19$total_tested <- scale(Covid19$total_tested, center = min(Covid19$total_tested), scale = diff(range(Covid19$total_tested)))
#Covid19$daily_positive <- scale(Covid19$daily_positive, center = min(Covid19$daily_positive), scale = diff(range(Covid19$daily_positive)))
#Covid19$hospitalizedCurr <- scale(Covid19$hospitalizedCurr, center = min(Covid19$hospitalizedCurr), scale = diff(range(Covid19$hospitalizedCurr)))
#Covid19$active <- scale(Covid19$active, center = min(Covid19$active), scale = diff(range(Covid19$active)))
#Covid19$recovered <- scale(Covid19$recovered, center = min(Covid19$recovered), scale = diff(range(Covid19$recovered)))



#############################################################################################

data <- Covid19 %>% select(positive,hospitalized, recovered, death, Date,Continent_Name)

sample_data_long <- pivot_longer(data, cols = c('positive', 'hospitalized', 'recovered', 'death'), names_to = "Category", values_to = "Value")

##############################################################################
Covid19$Date <- as.Date(Covid19$Date)
province_states <- filter(Covid19, Province_State != "All States")
ui1 <- dashboardPage(
  dashboardHeader(title = "COVID-19 Tested vs Positive"),
  dashboardSidebar(
    selectInput("continent", "Select Continent:",
                choices = sort(unique(Covid19$Continent_Name)),
                selected = ""),
    selectInput("country", "Select Country:",
                choices = sort(unique(Covid19$Country_Region)),
                selected = ""),
    selectInput("province", "Select Province State:",
                choices = sort(unique(province_states$Province_State)),
                selected = "")
  ),
  dashboardBody(
    tags$div(
      style = "padding: 20px;",
      h3("Scatterplots and Lineplots to show number of people who took the test and number of people who tested positive for Covid over time"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    ),
    fluidRow(
      box(
        plotlyOutput("scatterPlotContinent"),
        title = "Scatter Plot of Positive Cases Over Time by Continent"
      ),
      box(
        plotlyOutput("linePlotContinent"),
        title = "Line Plot of Total Tested Cases Over Time by Continent"
      )
    ),
    fluidRow(
      box(
        plotlyOutput("scatterPlotCountry"),
        title = "Scatter Plot of Positive Cases Over Time by Country"
      ),
      box(
        plotlyOutput("linePlotCountry"),
        title = "Line Plot of Total Tested Cases Over Time by Country"
      )
    ),
    fluidRow(
      box(
        plotlyOutput("scatterPlotProvince"),
        title = "Scatter Plot of Positive Cases Over Time by Province State"
      ),
      box(
        plotlyOutput("linePlotProvince"),
        title = "Line Plot of Total Tested Cases Over Time by Province State"
      )
    )
  )
)
#####################################################################################################################################################################################
ui2 <- dashboardPage(
  dashboardHeader(title = "Postive cases vs Death, Recovered and Hospitalized cases"),
  dashboardSidebar(
    checkboxGroupInput("selected_columns", "Select Columns to Display:",
                       choices = c("positive", "death", "recovered", "hospitalized"),
                       selected = c("positive")),
    selectInput("continent", "Select Continent:",
                choices = unique(data$Continent_Name),
                selected = "Asia")
  ),
  dashboardBody(
    plotOutput("stacked_bar_chart"),
    tags$div(
      style = "padding: 20px;",
      h3("Stacked bar chart to show Relation between Positive,Death, Recovered and Hospitalized cases"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    )
  )
)

#############################################################################################
#Help
ui3 <- dashboardPage(
  dashboardHeader(title = "Instructions"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "dashboardTabs",
      menuItem("Help Section", tabName = "dashboard1")
    )
  ),
  
  dashboardBody(
    textOutput("sentence")
  )
)

############################################################################
ui4 <- dashboardPage(
  dashboardHeader(title = "Positive Cases over time"),
  
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("dateRange", "Select Date Range",
                     start = min(Covid19$Date),
                     end = max(Covid19$Date),
                     min = min(Covid19$Date),
                     max = max(Covid19$Date))
    )
  ),
  
  dashboardBody(
    tags$div(
      style = "padding: 20px;",
      h2("Areaplot to show Positive cases in specific period of time"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    ),
    plotOutput("covidPlot")
  )
)

#################################################################################
ui5 <- dashboardPage(
  dashboardHeader(title = "Tested cases over time"),
  dashboardSidebar(
    radioButtons("data_type", "Select Data Type:",
                 choices = c("Daily", "Cumulative"),
                 selected = "Daily"),
    selectInput("country", "Select Country:",
                choices = unique(Covid19$Country_Region),
                selected = "United States")
  ),
  dashboardBody(
    tags$div(
      style = "padding: 20px;",
      h2("Boxplot and jitteredplot to show tested cases over time"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    ),
    box(plotOutput("boxplot")),
    box(plotOutput("jitter"))
  )
)

##################################################################################################
#show/hide
ui6 <- dashboardPage(
  dashboardHeader(title = "Histograms Dashboard"),
  dashboardSidebar(
    checkboxGroupInput("selected_charts", "Select Charts to Display:",
                       choices = c("Positive", "Death", "Recovered", "Hospitalized"),
                       selected = c("Positive", "Death", "Recovered", "Hospitalized")),
    selectInput("unit_type", "Select Unit Type:",
                choices = c("Absolute", "Percentage Change"),
                selected = "Absolute"),
    useShinyjs()
  ),
  dashboardBody(
    tags$div(
      style = "padding: 20px;",
      h2("Histograms to compare between Posivtive, Death, Recovered, Hospitalized cases"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    ),
    fluidRow(
      box(id = "positive_box", plotOutput("positive_histogram"), width = 6),
      box(id = "death_box", plotOutput("death_histogram"), width = 6),
      box(id = "recovered_box", plotOutput("recovered_histogram"), width = 6),
      box(id = "hospitalized_box", plotOutput("hospitalized_histogram"), width = 6)
    )
  )
)

##########################################################################################
ui7 <- dashboardPage(
  dashboardHeader(title = "Country Selection Histogram"),
  dashboardSidebar(
    radioButtons("continent", "Select Continent", choices = unique(Covid19$Continent_Name)),
    hr(),
    checkboxGroupInput("countries", "Select Countries", choices = NULL)
  ),
  dashboardBody(
    tags$div(
      style = "padding: 20px;",
      h2("Histogram to show Positive cases in each Country"),
      data_toggle = "tooltip",
      data_container = "body",
      title = "This is a tooltip"
    ),
    plotOutput("covidPlot11"),
    textOutput("active_cases_text")
  )
)

#####################################################################################################
ui <- fluidPage(
  #Buttons and Darkmode
  actionButton("myButton1", "Tested vs Positive"),
  actionButton("myButton", "Postive vs All"),
  actionButton("myButton2", "Positive vs Time"),
  actionButton("myButton4", "Tested over time"),
  actionButton("myButton5", "Histograms"),
  actionButton("myButton6", "Active Histograms"),
  actionButton("myButton3", "Help"),
  fluidRow(
    column(
      width = 4,
      checkboxInput("darkModeToggle", "Dark Mode", FALSE)
    )
  ),uiOutput("dashboard"),
  tags$head(
    tags$style(HTML("
      body.dark-mode {
        background-color: #1a1a1a;
        color: #ffffff;
      }
      
      .navbar-default {
        background-color: #333333;
        border-color: #080808;
      }
      
      .sidebar {
        background-color: #2c2c2c;
        color: #ffffff;
      }
      
      .main-content {
        background-color: #1a1a1a;
        color: #ffffff;
      }
    "))
  ),
  tags$head(
    tags$script(HTML("
    $(document).ready(function() {
      $('#darkModeToggle').on('change', function() {
        $('body').toggleClass('dark-mode', $(this).prop('checked'));
      });
    });
  "))
  )
)
################################################################################################################

server <- function(input, output, session) {
  
  ##########################################################  
  #Help
  output$sentence <- renderText({
    "There are many filters that you can use to visualize Specific criteria or in specific period of time using
      the dropdown menus to select different Continent, Countries or States, the radio buttons to display daily or cumulative,
      the date range to select specific period of time, the checkbox to compare values, checkbox to show or hide charts, Dropdown
      menu to switch from percentage change to absolute values and you can change to dark mode"
  })
  #######################################################################
  #show/hide
  # Define a reactive for the selected unit type
  
  
  # Define selected_charts reactive
  selected_charts <- reactive({
    input$selected_charts
  })
  
  # Function to check if the data is valid
  is_valid_data <- function(data) {
    all(is.finite(data)) && !any(is.na(data))
  }
  
  # Function to calculate percentage change
  calculate_percentage_change <- function(data) {
    c(0, diff(data) / lag(data) * 100)
  }
  
  # Define selected_unit_type reactive
  selected_unit_type <- reactive({
    input$unit_type
  })
  
  # Render positive histogram
  output$positive_histogram <- renderPlot({
    if ("Positive" %in% selected_charts() && is_valid_data(Covid19$positive)) {
      data <- switch(selected_unit_type(),
                     "Absolute" = Covid19$positive,
                     "Percentage Change" = calculate_percentage_change(Covid19$positive))
      
      hist(data, main = "Positive", col = "#75AADB", border = "white", xlab = "Values")
    } else {
      plot(NULL)
    }
  })
  
  # Render death histogram
  output$death_histogram <- renderPlot({
    if ("Death" %in% selected_charts() && is_valid_data(Covid19$death)) {
      data <- switch(selected_unit_type(),
                     "Absolute" = Covid19$death,
                     "Percentage Change" = calculate_percentage_change(Covid19$death))
      
      hist(data, main = "Death", col = "#75AADB", border = "white", xlab = "Values")
    } else {
      plot(NULL)
    }
  })
  
  # Render recovered histogram
  output$recovered_histogram <- renderPlot({
    if ("Recovered" %in% selected_charts() && is_valid_data(Covid19$recovered)) {
      data <- switch(selected_unit_type(),
                     "Absolute" = Covid19$recovered,
                     "Percentage Change" = calculate_percentage_change(Covid19$recovered))
      
      hist(data, main = "Recovered", col = "#75AADB", border = "white", xlab = "Values")
    } else {
      plot(NULL)
    }
  })
  
  # Render hospitalized histogram
  output$hospitalized_histogram <- renderPlot({
    if ("Hospitalized" %in% selected_charts() && is_valid_data(Covid19$hospitalized)) {
      data <- switch(selected_unit_type(),
                     "Absolute" = Covid19$hospitalized,
                     "Percentage Change" = calculate_percentage_change(Covid19$hospitalized))
      
      hist(data, main = "Hospitalized", col = "#75AADB", border = "white", xlab = "Values")
    } else {
      plot(NULL)
    }
  })
  
  # Use shinyjs to hide/show charts dynamically
  observe({
    shinyjs::toggle(id = "positive_box", condition = "Positive" %in% selected_charts())
    shinyjs::toggle(id = "death_box", condition = "Death" %in% selected_charts())
    shinyjs::toggle(id = "recovered_box", condition = "Recovered" %in% selected_charts())
    shinyjs::toggle(id = "hospitalized_box", condition = "Hospitalized" %in% selected_charts())
  })
  
  
  
  ###############################################################################################
  # Update country choices based on selected continent
  observe({
    selected_continent11 <- input$continent
    # Filter out repeated countries using unique
    updateCheckboxGroupInput(session, "countries",
                             choices = unique(Covid19$Country_Region[Covid19$Continent_Name == selected_continent11]))
  })
  
  # Reactive function to filter data based on selected countries
  filtered_data <- reactive({
    Covid19 %>%
      filter(Country_Region %in% input$countries)
  })
  
  # Reactive function to create the histogram based on selected countries
  output$covidPlot11 <- renderPlot({
    ggplot(filtered_data(), aes(x = positive, fill = Country_Region)) +
      geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
      labs(title = "Histogram of Positive Cases by Country",
           x = "Positive Cases",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Dummy text output for testing
  output$active_cases_text <- renderText({
    paste("Selected countries:", paste(input$countries, collapse = ", "))
  })
  ###########################################################################
  filtered_data_continent <- reactive({
    Covid19 %>%
      filter(Continent_Name == input$continent)
  })
  
  #filter data based on selected country
  filtered_data_country <- reactive({
    Covid19 %>%
      filter(Country_Region == input$country)
  })
  
  #filter data based on selected province state
  filtered_data_province <- reactive({
    Covid19 %>%
      filter(Province_State == input$province)
  })
  
  #create the scatter plot by continent
  output$scatterPlotContinent <- renderPlotly({
    plot_ly(
      data = filtered_data_continent(),
      x = ~Date,
      y = ~positive,
      color = ~Continent_Name,
      type = "scatter",
      mode = "markers",
      text = ~paste("Continent: ", Continent_Name, "<br>Date: ", Date, "<br>Positive Cases: ", positive)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Cases"))
  })
  
  #create the line plot by continent
  output$linePlotContinent <- renderPlotly({
    plot_ly(
      data = filtered_data_continent(),
      x = ~Date,
      y = ~total_tested,
      color = ~Continent_Name,
      type = "scatter",
      mode = "lines+markers",
      text = ~paste("Continent: ", Continent_Name, "<br>Date: ", Date, "<br>Total Tested Cases: ", total_tested)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total Tested Cases"))
  })
  
  #create the scatter plot by country
  output$scatterPlotCountry <- renderPlotly({
    plot_ly(
      data = filtered_data_country(),
      x = ~Date,
      y = ~positive,
      color = ~Country_Region,
      type = "scatter",
      mode = "markers",
      text = ~paste("Country: ", Country_Region, "<br>Date: ", Date, "<br>Positive Cases: ", positive)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Cases"))
  })
  
  #create the line plot by country
  output$linePlotCountry <- renderPlotly({
    plot_ly(
      data = filtered_data_country(),
      x = ~Date,
      y = ~total_tested,
      color = ~Country_Region,
      type = "scatter",
      mode = "lines+markers",
      text = ~paste("Country: ", Country_Region, "<br>Date: ", Date, "<br>Total Tested Cases: ", total_tested)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total Tested Cases"))
  })
  
  #create the scatter plot by province state
  output$scatterPlotProvince <- renderPlotly({
    plot_ly(
      data = filtered_data_province(),
      x = ~Date,
      y = ~positive,
      color = ~Province_State,
      type = "scatter",
      mode = "markers",
      text = ~paste("Province State: ", Province_State, "<br>Date: ", Date, "<br>Positive Cases: ", positive)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Cases"))
  })
  
  #create the line plot by province state
  output$linePlotProvince <- renderPlotly({
    plot_ly(
      data = filtered_data_province(),
      x = ~Date,
      y = ~total_tested,
      color = ~Province_State,
      type = "scatter",
      mode = "lines+markers",
      text = ~paste("Province State: ", Province_State, "<br>Date: ", Date, "<br>Total Tested Cases: ", total_tested)
    ) %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total Tested Cases"))
  })
  ##################################################################################################
  #filterdata
  selected_data <- reactive({
    df <- sample_data_long %>%
      filter(Category %in% input$selected_columns, Continent_Name == input$continent)
  })
  
  # Render the dynamic stacked bar chart
  output$stacked_bar_chart <- renderPlot({
    ggplot(selected_data(), aes(x = Date, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Dynamic Stacked Bar Chart",
           x = "Date",
           y = "Value",
           fill = "Category") +
      theme_minimal()
  })
  ################################################################
  # Filter data based on the selected date range
  filteredDataArea <- reactive({
    filter(Covid19, between(Date, input$dateRange[1], input$dateRange[2]))
  })
  
  # Update the plot based on the filtered data
  output$covidPlot <- renderPlot({
    ggplot(filteredDataArea(), aes(x = Date, y = positive, fill = "COVID-19 Cases")) +
      geom_area(alpha = 0.7) +
      labs(title = "COVID-19 Cases Over Time",
           x = "Date",
           y = "COVID-19 Cases") +
      theme_minimal()
  })
  
  #################################################################################
  filtered <- reactive({
    if (input$data_type == "Daily") {
      # Filter daily data
      Covid19 %>%
        filter(Country_Region == input$country) %>%
        select(Date, daily_positive, daily_tested, hospitalizedCurr) %>%
        filter(daily_positive >= 0)
    } else {
      # Use cumulative data directly
      Covid19 %>%
        filter(Country_Region == input$country) %>%
        select(Date, positive, total_tested, hospitalized)
    }
  })
  
  output$boxplot <- renderPlot({
    ggplot(filtered(), aes(x = Date, y = if (input$data_type == "Daily") daily_tested else total_tested)) +
      geom_boxplot() +
      scale_y_log10() +  # Add a logarithmic scale to the y-axis
      labs(title = "Boxplot of Tested Cases Over Time",
           x = "Date",
           y = "Tested Cases")
  })
  
  # Jitter plot
  output$jitter <- renderPlot({
    ggplot(filtered(), aes(x = if (input$data_type == "Daily") daily_positive else positive, y = Date)) +
      geom_jitter() +
      labs(title = "Jitter Plot of Tested Cases Over Time",
           x = "Tested Cases",
           y = "Date")
  })
  ############################################################################################################  
  #first dashboard
  output$dashboard <- renderUI({
    ui1
  })
  observeEvent(input$myButton, {
    output$dashboard <- renderUI({
      ui2
    })
  })
  observeEvent(input$myButton1, {
    output$dashboard <- renderUI({
      ui1
    })
  })
  observeEvent(input$myButton3, {
    output$dashboard <- renderUI({
      ui3
    })
  })
  observeEvent(input$myButton2, {
    output$dashboard <- renderUI({
      ui4
    })
  })
  observeEvent(input$myButton4, {
    output$dashboard <- renderUI({
      ui5
    })
  })
  observeEvent(input$myButton5, {
    output$dashboard <- renderUI({
      ui6
    })
  })
  observeEvent(input$myButton6, {
    output$dashboard <- renderUI({
      ui7
    })
  })
}

shinyApp(ui = ui, server = server)