# Global Quality of Life Explorer
This repository contains a Shiny application that visualizes quality of life indicators across countries based on data from Numbeo. The application allows users to explore various metrics such as purchasing power, safety, healthcare, climate, cost of living, property prices, traffic, pollution, and overall quality of life.

## Features
- **Overview Tab**: View top-ranked countries by any selected indicator with an interactive bar chart and data table.
- **Country Comparison Tab**: Compare multiple countries across all quality of life indicators in a side-by-side bar chart.
- **Relationships Tab**: Explore correlations between different indicators with interactive scatter plots and correlation statistics.
- **About Tab**: Information about the application and data source.

## Interactive Elements
The application includes multiple dynamic queries:
- Dropdown menus for selecting different quality of life indicators.
- Range slider for filtering countries by score values.
- Checkboxes for selecting countries to compare.
- Interactive visualizations with tooltips and zooming capabilities.

## Getting Started
### Prerequisites
Ensure you have **R (version 4.0 or higher recommended)** installed along with the following R packages:
- shiny
- ggplot2
- dplyr
- plotly
- DT
- shinythemes

### Running the App
To run the application locally:
1. Clone this repository
2. Open R or RStudio
3. Set your working directory to the repository folder
4. Run the following command:
   ```R
   source("global_quality_of_life_app.R")
   ```
## Data Source
The data was collected from [Kaggle]([https://www.numbeo.com/](https://www.kaggle.com/datasets/ahmedmohamed2003/quality-of-life-for-each-country)), which aggregates user-contributed data from individuals worldwide on various quality of life metrics.

## Project Context
This application was created as part of a portfolio project for a statistics visualization course (STAT-436) at the University of Wisconsin-Madison.
