# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(shinythemes)
library(maps)
library(countrycode)

# Load the dataset
qol_data <- read.csv("Quality_of_Life.csv")

# Check the data types of all columns
str(qol_data)

# Convert "Quality.of.Life.Value" and "Property.Price.to.Income.Value" to numeric if needed
if (is.character(qol_data$Quality.of.Life.Value)) {
  # First clean any non-numeric characters
  qol_data$Quality.of.Life.Value <- gsub("[^0-9.-]", "", qol_data$Quality.of.Life.Value)
  qol_data$Quality.of.Life.Value <- as.numeric(qol_data$Quality.of.Life.Value)
}

if (is.character(qol_data$Property.Price.to.Income.Value)) {
  qol_data$Property.Price.to.Income.Value <- as.numeric(as.character(qol_data$Property.Price.to.Income.Value))
}

# Print range to see actual values
cat("Range of Quality.of.Life.Value:", 
    range(qol_data$Quality.of.Life.Value, na.rm = TRUE), "\n")

# Check for NAs and range
cat("Number of NAs in Quality.of.Life.Value:", sum(is.na(qol_data$Quality.of.Life.Value)), "\n")

# Create a scaled version of Quality of Life for better visualization
# Scale values by multiplying by 100 if max value is too small
if(max(qol_data$Quality.of.Life.Value, na.rm = TRUE) < 10) {
  qol_data$Scaled_Quality_of_Life <- qol_data$Quality.of.Life.Value * 100
  cat("Created scaled version of Quality of Life (multiplied by 100)\n")
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Global Quality of Life Explorer"),
  
  tabsetPanel(
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 # Updated selectInput with conditional inclusion of Scaled_Quality_of_Life
                 selectInput("indicator", "Select Quality of Life Indicator:",
                             choices = if(exists("Scaled_Quality_of_Life", where = qol_data)) {
                               c("Scaled_Quality_of_Life", "Quality.of.Life.Value", "Purchasing.Power.Value", 
                                 "Safety.Value", "Health.Care.Value", "Climate.Value", "Cost.of.Living.Value",
                                 "Property.Price.to.Income.Value", "Traffic.Commute.Time.Value", "Pollution.Value")
                             } else {
                               c("Quality.of.Life.Value", "Purchasing.Power.Value", "Safety.Value", "Health.Care.Value", 
                                 "Climate.Value", "Cost.of.Living.Value", "Property.Price.to.Income.Value", 
                                 "Traffic.Commute.Time.Value", "Pollution.Value")
                             },
                             selected = if(exists("Scaled_Quality_of_Life", where = qol_data)) 
                               "Scaled_Quality_of_Life" else "Quality.of.Life.Value"),
                 
                 # Dynamic slider values based on selected indicator
                 sliderInput("score_filter", "Filter by Score Range:",
                             min = 0,
                             max = 200,
                             value = c(0, 200)),
                 
                 helpText("This application allows you to explore quality of life indicators across countries based on Numbeo data.")
               ),
               
               mainPanel(
                 plotlyOutput("indicator_chart", height = "600px"),
                 br(),
                 DT::dataTableOutput("countries_table")
               )
             )
    ),
    
    tabPanel("Country Comparison",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("countries", "Select Countries to Compare:",
                                    choices = sort(unique(qol_data$country)),
                                    selected = c("United States", "Canada", "Germany", "Japan", "Australia")),
                 
                 helpText("Select countries to compare across all quality of life indicators.")
               ),
               
               mainPanel(
                 plotlyOutput("comparison_plot", height = "600px"),
                 textOutput("comparison_text")
               )
             )
    ),
    
    tabPanel("Relationships",
             sidebarLayout(
               sidebarPanel(
                 # Also update relationships tab to include scaled quality of life
                 selectInput("xvar", "X-Axis Variable:",
                             choices = if(exists("Scaled_Quality_of_Life", where = qol_data)) {
                               c("Scaled_Quality_of_Life", "Quality.of.Life.Value", "Purchasing.Power.Value", 
                                 "Safety.Value", "Health.Care.Value", "Climate.Value", "Cost.of.Living.Value",
                                 "Property.Price.to.Income.Value", "Traffic.Commute.Time.Value", "Pollution.Value")
                             } else {
                               c("Quality.of.Life.Value", "Purchasing.Power.Value", "Safety.Value", "Health.Care.Value", 
                                 "Climate.Value", "Cost.of.Living.Value", "Property.Price.to.Income.Value", 
                                 "Traffic.Commute.Time.Value", "Pollution.Value")
                             },
                             selected = "Purchasing.Power.Value"),
                 
                 selectInput("yvar", "Y-Axis Variable:",
                             choices = if(exists("Scaled_Quality_of_Life", where = qol_data)) {
                               c("Scaled_Quality_of_Life", "Quality.of.Life.Value", "Purchasing.Power.Value", 
                                 "Safety.Value", "Health.Care.Value", "Climate.Value", "Cost.of.Living.Value",
                                 "Property.Price.to.Income.Value", "Traffic.Commute.Time.Value", "Pollution.Value")
                             } else {
                               c("Quality.of.Life.Value", "Purchasing.Power.Value", "Safety.Value", "Health.Care.Value", 
                                 "Climate.Value", "Cost.of.Living.Value", "Property.Price.to.Income.Value", 
                                 "Traffic.Commute.Time.Value", "Pollution.Value")
                             },
                             selected = "Safety.Value"),
                 
                 helpText("Explore relationships between different quality of life indicators.")
               ),
               
               mainPanel(
                 plotlyOutput("scatter_plot", height = "600px"),
                 verbatimTextOutput("correlation")
               )
             )
    ),
    
    tabPanel("About",
             fluidRow(
               column(12,
                      h3("About This Application"),
                      p("This Shiny application visualizes quality of life indicators across countries based on data from Numbeo."),
                      p("The dataset includes metrics such as purchasing power, safety, health care, climate, cost of living, property prices, traffic, pollution, and overall quality of life."),
                      p("The application was created as part of a portfolio project for a statistics visualization course."),
                      h4("Data Source"),
                      p("The data was collected from Numbeo, which aggregates user-contributed data from individuals worldwide.")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Update slider range dynamically based on selected indicator
  observe({
    indicator_col <- input$indicator
    
    # Get min and max values for the selected indicator
    if (!is.null(indicator_col) && indicator_col %in% names(qol_data)) {
      min_val <- min(qol_data[[indicator_col]], na.rm = TRUE)
      max_val <- max(qol_data[[indicator_col]], na.rm = TRUE)
      
      # Update slider range
      updateSliderInput(session, "score_filter",
                        min = min_val,
                        max = max_val,
                        value = c(min_val, max_val))
    }
  })
  
  # Create reactive filtered dataset
  filtered_data <- reactive({
    # Get current indicator value
    indicator_col <- input$indicator
    
    # Create a copy of the data
    filter_data <- qol_data
    
    # Filter based on slider
    filter_data <- filter_data %>%
      filter(!is.na(get(indicator_col))) %>%
      filter(get(indicator_col) >= input$score_filter[1],
             get(indicator_col) <= input$score_filter[2])
    
    return(filter_data)
  })
  
  # Overview chart visualization
  output$indicator_chart <- renderPlotly({
    # Get current data
    plot_data <- filtered_data()
    indicator_col <- input$indicator
    
    # Handle potential empty data
    if(nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No data available with current filters") +
                        theme_minimal()))
    }
    
    # Handle special case for original Quality of Life Value (not scaled)
    if(indicator_col == "Quality.of.Life.Value") {
      # Check if values are meaningful
      max_val <- max(plot_data[[indicator_col]], na.rm = TRUE)
      if(max_val < 1) {
        # Use Quality of Life Category instead
        cat_data <- plot_data %>%
          filter(Quality.of.Life.Category != "None") %>%
          count(Quality.of.Life.Category) %>%
          arrange(desc(n))
        
        p <- ggplot(cat_data, aes(x = reorder(Quality.of.Life.Category, n), y = n,
                                  text = paste("Category:", Quality.of.Life.Category, "<br>",
                                               "Number of Countries:", n))) +
          geom_col(aes(fill = Quality.of.Life.Category), show.legend = FALSE) +
          coord_flip() +
          labs(x = "Quality of Life Category", 
               y = "Number of Countries",
               title = "Distribution of Countries by Quality of Life Category") +
          theme_minimal()
        
        return(ggplotly(p, tooltip = "text"))
      }
    }
    
    # Sort by selected indicator and get top countries
    plot_data <- plot_data %>%
      arrange(desc(get(indicator_col))) %>%
      head(30)  # Limit to top 30 for readability
    
    # Create bar chart
    p <- ggplot(plot_data, aes(x = reorder(country, get(indicator_col)), 
                               y = get(indicator_col),
                               text = paste("Country:", country, "<br>",
                                            "Value:", round(get(indicator_col), 2)))) +
      geom_col(aes(fill = get(indicator_col)), show.legend = FALSE) +
      coord_flip() +
      scale_fill_viridis_c() +
      labs(x = "Country", 
           y = gsub("\\.", " ", indicator_col),
           title = paste("Top Countries by", gsub("\\.", " ", indicator_col))) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Countries table
  output$countries_table <- renderDT({
    # Get current data
    table_data <- filtered_data()
    indicator_col <- input$indicator
    
    # Handle special case for Scaled_Quality_of_Life
    category_col <- if(indicator_col == "Scaled_Quality_of_Life") {
      "Quality.of.Life.Category"  # Use the original category
    } else {
      paste0(gsub("\\.Value$", "", indicator_col), ".Category")
    }
    
    # Check if category column exists
    if(category_col %in% names(table_data)) {
      table_data %>%
        select(country, all_of(indicator_col), all_of(category_col)) %>%
        arrange(desc(get(indicator_col))) %>%
        rename("Country" = country,
               "Value" = all_of(indicator_col),
               "Category" = all_of(category_col))
    } else {
      table_data %>%
        select(country, all_of(indicator_col)) %>%
        arrange(desc(get(indicator_col))) %>%
        rename("Country" = country,
               "Value" = all_of(indicator_col))
    }
  })
  
  # Comparison plot
  output$comparison_plot <- renderPlotly({
    # Check if countries are selected
    if(length(input$countries) == 0) {
      return(ggplotly(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "Please select at least one country") +
                        theme_minimal()))
    }
    
    # Filter to selected countries
    compare_data <- qol_data %>%
      filter(country %in% input$countries)
    
    # Select value columns - include scaled version if it exists
    value_cols <- c("Purchasing.Power.Value", "Safety.Value", "Health.Care.Value",
                    "Climate.Value", "Cost.of.Living.Value",
                    "Traffic.Commute.Time.Value", "Pollution.Value",
                    "Property.Price.to.Income.Value")
    
    # Use Scaled_Quality_of_Life instead of Quality.of.Life.Value if it exists
    if("Scaled_Quality_of_Life" %in% names(qol_data)) {
      value_cols <- c(value_cols, "Scaled_Quality_of_Life")
    } else {
      value_cols <- c(value_cols, "Quality.of.Life.Value")
    }
    
    # Select data for plotting
    compare_data <- compare_data %>%
      select(country, all_of(value_cols))
    
    # Reshape for plotting
    compare_long <- tidyr::pivot_longer(compare_data,
                                        cols = -country,
                                        names_to = "indicator",
                                        values_to = "value")
    
    # Clean indicator names for display
    compare_long$indicator <- gsub("^Scaled_", "", compare_long$indicator)
    compare_long$indicator <- gsub("\\.Value$", "", compare_long$indicator)
    compare_long$indicator <- gsub("\\.", " ", compare_long$indicator)
    
    # Create plot
    p <- ggplot(compare_long, aes(x = indicator, y = value, fill = country, group = country)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Indicator", y = "Value", fill = "Country",
           title = "Comparison of Quality of Life Indicators")
    
    ggplotly(p)
  })
  
  # Comparison text
  output$comparison_text <- renderText({
    num_countries <- length(input$countries)
    if (num_countries == 0) {
      return("Please select at least one country to compare.")
    } else if (num_countries == 1) {
      return(paste("Showing data for", input$countries))
    } else {
      return(paste("Comparing", num_countries, "countries across quality of life indicators."))
    }
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    # Get variables
    x_var <- input$xvar
    y_var <- input$yvar
    
    # Remove NA values
    plot_data <- qol_data %>%
      filter(!is.na(get(x_var)), !is.na(get(y_var)))
    
    if(nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No data available for these variables") +
                        theme_minimal()))
    }
    
    # Create scatter plot
    p <- ggplot(plot_data, aes(x = get(x_var),
                               y = get(y_var),
                               text = paste("Country:", country, "<br>",
                                            gsub("\\.", " ", x_var), ":", round(get(x_var), 2), "<br>",
                                            gsub("\\.", " ", y_var), ":", round(get(y_var), 2)))) +
      geom_point(aes(color = Quality.of.Life.Category), size = 3, alpha = 0.7) +
      labs(x = gsub("\\.", " ", x_var),
           y = gsub("\\.", " ", y_var),
           title = paste("Relationship between", gsub("\\.", " ", x_var), "and", gsub("\\.", " ", y_var)),
           color = "Quality of Life Category") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Correlation output
  output$correlation <- renderPrint({
    # Get variables
    x_var <- input$xvar
    y_var <- input$yvar
    
    # Calculate correlation
    cor_value <- cor(qol_data[[x_var]], qol_data[[y_var]], use = "complete.obs")
    cat("Correlation between", gsub("\\.", " ", x_var), "and", gsub("\\.", " ", y_var), ":", round(cor_value, 3))
    
    # Add interpretation
    cat("\n\nInterpretation:")
    if(abs(cor_value) < 0.3) {
      cat("\nWeak correlation")
    } else if(abs(cor_value) < 0.7) {
      cat("\nModerate correlation")
    } else {
      cat("\nStrong correlation")
    }
    
    if(cor_value > 0) {
      cat(" (positive)")
    } else {
      cat(" (negative)")
    }
  })
}

# Run the application (use this in a final code chunk)
shinyApp(ui = ui, server = server)