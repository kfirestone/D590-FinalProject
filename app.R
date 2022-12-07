#Load necessary libraries
library(shiny)
library(shinyWidgets)
library(bslib)
library(thematic)
library(showtext)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
library(forecast)
library(prophet)
library(countrycode)
library(aws.s3)

#Name of AWS S3 bucket cloud data is in
s3bucket = 'kfd590finalproject'

#Define light and dark mode via bslib
light <- bs_theme(bootswatch = 'minty', bg = '#fff', fg = '#000', base_font = font_google('Montserrat'))
dark <- bs_theme(bootswatch = 'superhero', base_font = font_google('Montserrat'))

#Make plots and table copy theme set by bslib
thematic_shiny(font = 'auto')

#Define UI elements and layout
ui <- fluidPage(
  theme = dark,
  
  fluidRow(
    column(3,
           awesomeCheckbox('light_mode', 'Light Mode')
           )
  ),
  
  sidebarLayout(fluid = FALSE,
    sidebarPanel(width = 3,
      selectInput('dataset', 'Data Set', choices = c('Global', 'By Country', 'By State')),
      uiOutput('plot_type'),
      uiOutput('year_slider'),
      uiOutput('filter')
    ),
    mainPanel(width = 9,
      uiOutput('panel')
      )
    )
  )

#Setup server, configure data sets, create dynamic ui elements and generate plots
server <- shinyServer(function(input, output, session){
#===============================DATA PROCESSING=================================
  #bs_themer()
  #Sets up switch between light and dark mode
  observe(session$setCurrentTheme(if (isTRUE(input$light_mode)) light else dark))
  
  #Clean global temperature data set
  global_total <- s3read_using(FUN = read.csv, bucket = s3bucket, object = 'GlobalTemperatures.csv')
  global_total[['dt']] <- strptime(global_total[['dt']], format = '%Y-%m-%d')
  global_total[['Year']] <- format(global_total[['dt']], format = '%Y')
  global_total <- subset(global_total, select = -(dt))
  global_total <- global_total %>% group_by(Year) %>% 
                  summarise(across(.cols = everything(), .fns = ~mean(.x, na.rm = TRUE))) %>% 
                  mutate(across(where(is.numeric), round, digits = 3))
  global_total$Year <- as.numeric(global_total$Year)
  
  #Remove unnecessary information from global temp data and perform forecasting via Prophet
  global_truncated <- global_total[c('Year', 'LandAverageTemperature')]
  global_truncated <- subset(global_truncated, Year >= 1850)
  global_truncated <- transform(global_truncated, ds = as.Date(as.character(Year), '%Y'))
  lambda = BoxCox.lambda(global_truncated$LandAverageTemperature, method = 'loglik')
  global_truncated$y = BoxCox(global_truncated$LandAverageTemperature, lambda)
  m <- prophet(global_truncated)
  future <- make_future_dataframe(m, periods = 50, freq = 'year')
  forecast <- predict(m, future)
  forecast <- slice_tail(forecast, n = 50)
  forecast$LandAverageTemperature = InvBoxCox(forecast$yhat, lambda)
  forecast$lower = InvBoxCox(forecast$yhat_lower, lambda)
  forecast$upper = InvBoxCox(forecast$yhat_upper, lambda)
  global_truncated <- global_truncated[c('Year', 'LandAverageTemperature')]
  global_truncated$lower <- NA
  global_truncated$upper <- NA
  forecast$Year <- format(forecast[['ds']], format = '%Y')
  forecast <- forecast[c('Year', 'LandAverageTemperature', 'lower', 'upper')]
  forecast <- rbind(global_truncated, forecast)
  forecast$Year <- as.numeric(forecast$Year)
  
  #Clean country temperature data set
  countries <- s3read_using(FUN = read.csv, bucket = s3bucket, object = 'GlobalLandTemperaturesByCountry.csv')
  countries$dt <- strptime(countries$dt, format = '%Y-%m-%d')
  countries$Year <- format(countries$dt, format = '%Y')
  countries <- countries[c('Year', 'Country', 'AverageTemperature')]
  countries <- countries %>% group_by(Year, Country) %>% summarise(across(.cols = everything(), .fns = ~mean(.x, na.rm = TRUE))) %>%
                mutate(across(where(is.numeric), round, digits = 3))
  countries <- subset(countries, Year >= 1850)
  countries$Year <- as.numeric(countries$Year)
  countries$Code <- countrycode(countries$Country, 'country.name', 'iso3c')
  countries <- na.omit(countries)
  duplicate_countries = c('Denmark (Europe)', 'France (Europe)', 'Netherlands (Europe)', 'United Kingdom (Europe)')
  countries <- subset(countries, !(Country %in% duplicate_countries))
  
#==============REMOVED FOR PERFORMANCE REASONS - TRAINING ALL COUNTRIES VERY SLOW==============
#=====================WILL LOAD FROM CSV FILE CREATED FROM TRAINING BELOW======================
  # #Remove unnecessary information from country temp data and perform forecasting via Prophet
  # countries_truncated <- countries[c('Year', 'AverageTemperature', 'Country')]
  # countries_truncated <- transform(countries_truncated, ds = as.Date(as.character(Year), '%Y'))
  # countries_forecast <- data.frame(matrix(ncol = 5, nrow = 0))
  # colnames(countries_forecast) <- c('Year', 'Country', 'AverageTemperature', 'lower', 'upper')
  # for(i in 1:227){
  #   country_truncated <- subset(countries_truncated, Country == unique(countries_truncated$Country)[i])
  #   if(all(country_truncated$AverageTemperature > 0, na.rm = TRUE)){
  #     lambda = BoxCox.lambda(country_truncated$AverageTemperature, method = 'loglik')
  #   }
  #   else{
  #     lambda = BoxCox.lambda(country_truncated$AverageTemperature, method = 'guerrero')
  #   }
  #   country_truncated$y = BoxCox(country_truncated$AverageTemperature, lambda)
  #   m <- prophet(country_truncated)
  #   future <- make_future_dataframe(m, periods = 50, freq = 'year')
  #   country_forecast <- predict(m, future)
  #   country_forecast <- slice_tail(country_forecast, n = 50)
  #   country_forecast$AverageTemperature = InvBoxCox(country_forecast$yhat, lambda)
  #   country_forecast$lower = InvBoxCox(country_forecast$yhat_lower, lambda)
  #   country_forecast$upper = InvBoxCox(country_forecast$yhat_upper, lambda)
  #   country_forecast$Year <- format(country_forecast[['ds']], format = '%Y')
  #   country_forecast$Country <- unique(countries_truncated$Country)[i]
  #   country_forecast <- country_forecast[c('Year', 'Country', 'AverageTemperature', 'lower', 'upper')]
  #   countries_forecast <- rbind(countries_forecast, country_forecast)
  #   print(i)
  #   print(unique(countries_truncated$Country)[i])
  # }
  # countries_forecast$Year <- as.numeric(countries_forecast$Year)
  # countries_truncated <- countries_truncated[c('Year', 'Country', 'AverageTemperature')]
  # countries_truncated$lower <- NA
  # countries_truncated$upper <- NA
  # countries_forecast <- rbind(countries_forecast, countries_truncated)
  # countries_forecast <- countries_forecast[order(countries_forecast$Country, countries_forecast$Year), ]
  # 
  # rm(countries_truncated)
  # rm(country_truncated)
  # rm(m)
  # rm(future)
  # rm(country_forecast)
  # 
  # write.csv(countries_forecast, 'GlobalLandTemperaturesByCountryForecast.csv', row.names = FALSE) 
  
  countries_forecast <- s3read_using(FUN = read.csv, bucket = s3bucket, object = 'GlobalLandTemperaturesByCountryForecast.csv')
  
  #Clean state temperature data set
  states <- s3read_using(FUN = read.csv, bucket = s3bucket, object = 'GlobalLandTemperaturesByState.csv')
  states <- subset(states, Country == 'United States')
  states$dt <- strptime(states$dt, format = '%Y-%m-%d')
  states$Year <- format(states$dt, format = '%Y')
  states <- states[c('Year', 'AverageTemperature', 'State')]
  states <- states %>% group_by(Year, State) %>% summarise(across(.cols = everything(), .fns = ~mean(.x, na.rm = TRUE))) %>%
            mutate(across(where(is.numeric), round, digits = 3))
  states <- subset(states, Year >= 1850)
  states$Year <- as.numeric(states$Year)
  states$State[states$State == 'Georgia (State)'] <- 'Georgia'
  states$Code <- NA
  state_codes <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID',
                   'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS',
                   'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK',
                   'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA',
                   'WV', 'WI', 'WY')
  for(i in 1:50){
    states$Code[states$State == state.name[i]] <- state_codes[i]
  }
  states$Code[states$State == 'District Of Columbia'] <- 'DC' #DC not in R built-in state.name
  # 
  # #Remove unnecessary information from state temp data and perform forecasting via Prophet
  # states_truncated <- states[c('Year', 'AverageTemperature', 'State')]
  # states_truncated <- transform(states_truncated, ds = as.Date(as.character(Year), '%Y'))
  # state_truncated <- subset(states_truncated, State == 'District Of Columbia')
  # lambda = BoxCox.lambda(state_truncated$AverageTemperature, method = 'loglik')
  # state_truncated$y = BoxCox(state_truncated$AverageTemperature, lambda)
  # m <- prophet(state_truncated)
  # future <- make_future_dataframe(m, periods = 50, freq = 'year')
  # state_forecast <- predict(m, future)
  # state_forecast <- slice_tail(state_forecast, n = 50)
  # state_forecast$AverageTemperature = InvBoxCox(state_forecast$yhat, lambda)
  # state_forecast$lower = InvBoxCox(state_forecast$yhat_lower, lambda)
  # state_forecast$upper = InvBoxCox(state_forecast$yhat_upper, lambda)
  # state_forecast$Year <- format(state_forecast[['ds']], format = '%Y')
  # state_forecast$State <- 'District Of Columbia'
  # states_forecast <- state_forecast[c('Year', 'State', 'AverageTemperature', 'lower', 'upper')]
  # 
  # for(i in 1:50){
  #   state_truncated <- subset(states_truncated, State == state.name[i])
  #   if(all(state_truncated$AverageTemperature > 0)){
  #     lambda = BoxCox.lambda(state_truncated$AverageTemperature, method = 'loglik')
  #   }
  #   else{
  #     lambda = BoxCox.lambda(state_truncated$AverageTemperature, method = 'guerrero')
  #   }
  #   state_truncated$y = BoxCox(state_truncated$AverageTemperature, lambda)
  #   m <- prophet(state_truncated)
  #   future <- make_future_dataframe(m, periods = 50, freq = 'year')
  #   state_forecast <- predict(m, future)
  #   state_forecast <- slice_tail(state_forecast, n = 50)
  #   state_forecast$AverageTemperature = InvBoxCox(state_forecast$yhat, lambda)
  #   state_forecast$lower = InvBoxCox(state_forecast$yhat_lower, lambda)
  #   state_forecast$upper = InvBoxCox(state_forecast$yhat_upper, lambda)
  #   state_forecast$Year <- format(state_forecast[['ds']], format = '%Y')
  #   state_forecast$State <- state.name[i]
  #   state_forecast <- state_forecast[c('Year', 'State', 'AverageTemperature', 'lower', 'upper')]
  #   states_forecast <- rbind(states_forecast, state_forecast)
  # }
  # states_forecast$Year <- as.numeric(states_forecast$Year)
  # states_truncated <- states_truncated[c('Year', 'State', 'AverageTemperature')]
  # states_truncated$lower <- NA
  # states_truncated$upper <- NA
  # states_forecast <- rbind(states_forecast, states_truncated)
  # states_forecast <- states_forecast[order(states_forecast$State, states_forecast$Year), ]
  # 
  # rm(states_truncated)
  # rm(state_truncated)
  # rm(m)
  # rm(future)
  # rm(state_forecast)
  # 
  # write.csv(states_forecast, 'GlobalLandTemperaturesByStateForecast.csv', row.names = FALSE)
  
  states_forecast <- s3read_using(FUN = read.csv, bucket = s3bucket, object = 'GlobalLandTemperaturesByStateForecast.csv')
 
  #================================UI ELEMENTS==================================
  
  #Sets up dynamically populated plot type drop down box
  output$plot_type <- renderUI({
    if(input$dataset == 'Global'){
      plot_choices <- c('Time Series', 'Time Series with Forecasting')
    }
    else if(input$dataset == 'By Country'){
      plot_choices <- c('Time Series', 'Time Series with Forecasting', 'Choropleth')
    }
    else if(input$dataset == 'By State'){
      plot_choices <- c('Time Series', 'Time Series with Forecasting', 'Choropleth')
    }
    
    selectInput('plot', 'Plot Type', choices = plot_choices, selected = 'Time Series')
  })
  
  #Sets up dynamically adjust year slider for data set filtering
  output$year_slider <- renderUI({
    if(input$dataset == 'Global'){
      if(input$plot == 'Time Series'){
        sliderInput('year_range', 'Year Range', min = 1750, max = 2015, value = c(1750, 2015),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
      else if(input$plot == 'Time Series with Forecasting'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2065, value = c(1850, 2065),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
    }
    else if(input$dataset == 'By Country'){
      if(input$plot == 'Time Series'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2013, value = c(1850, 2013),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
      else if(input$plot == 'Time Series with Forecasting'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2063, value = c(1850, 2063),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
      else if(input$plot == 'Choropleth'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2013, value = 2013,
                    sep = '', animate = animationOptions(interval = 250))
      }
    }
    else if(input$dataset == 'By State'){
      if(input$plot == 'Time Series'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2013, value = c(1850, 2013),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
      else if(input$plot == 'Time Series with Forecasting'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2063, value = c(1850, 2063),
                    animate = animationOptions(interval = 250), sep = '', dragRange = TRUE)
      }
      else if(input$plot == 'Choropleth'){
        sliderInput('year_range', 'Year Range', min = 1850, max = 2013, value = 2013,
                    sep = '', animate = animationOptions(interval = 250))
      }
    }
  })
  
  #Dynamically generates country/state selection filters when 'By Country' or 'By Statee' data sets are selected
  output$filter <- renderUI({
    if(input$dataset == 'By Country'){
      pickerInput('country_selector', 'Select Country(ies)', choices = sort(unique(countries$Country)), multiple = TRUE,
                  options = list('actions-box' = TRUE))
    }
    else if(input$dataset == 'By State'){
      pickerInput('state_selector', 'Select State(s)', choices = sort(unique(states$State)), multiple = TRUE,
                  options = list('actions-box' = TRUE), selected = sort(unique(states$State)))
    }
  })
  
  #Generates time series plots based on selection
  output$plot <- renderPlot({
    if(input$dataset == 'Global'){
      if(input$plot == 'Time Series'){
        p <- ggplot(global_total, aes(x = Year, y = LandAverageTemperature)) + geom_line() + ylim(0, NA) + 
              xlim(input$year_range[1], input$year_range[2]) + ggtitle('Average Yearly Global Land Temperature (C)') + ylab('Avg Temp') +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                    axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
      else if(input$plot == 'Time Series with Forecasting'){
        p <- ggplot(forecast, aes(x = Year, y = LandAverageTemperature)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'white', alpha = 0.25) + 
                    ylim(0, NA) + xlim(input$year_range[1], input$year_range[2]) + ggtitle('Forecasted Average Yearly Global Land Temperature (C)') + ylab('Avg Temp') +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
    }
    else if(input$dataset == 'By Country'){
      if(input$plot == 'Time Series'){
        p <- ggplot(subset(countries, Country %in% input$country_selector), aes(x = Year, y = AverageTemperature, color = Country)) + geom_line() + 
          xlim(input$year_range[1], input$year_range[2]) + 
          ggtitle('Average Yearly State Land Temperature (C)') + ylab('Avg Temp') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
      else if(input$plot == 'Time Series with Forecasting'){
        p <- ggplot(subset(countries_forecast, Country %in% input$country_selector), aes(x = Year, y = AverageTemperature, color = Country)) + geom_line() + 
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = Country), color = NA, alpha = 0.25) + xlim(input$year_range[1], input$year_range[2]) + 
          ggtitle('Forecasted Average Yearly Country Land Temperature (C)') + ylab('Avg Temp') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
    }
    else if(input$dataset == 'By State'){
      if(input$plot == 'Time Series'){
        p <- ggplot(subset(states, State %in% input$state_selector), aes(x = Year, y = AverageTemperature, color = State)) + geom_line() + 
          xlim(input$year_range[1], input$year_range[2]) + 
          ggtitle('Average Yearly State Land Temperature (C)') + ylab('Avg Temp') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
      else if(input$plot == 'Time Series with Forecasting'){
        p <- ggplot(subset(states_forecast, State %in% input$state_selector), aes(x = Year, y = AverageTemperature, color = State)) + geom_line() + 
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = State), color = NA, alpha = 0.25) + xlim(input$year_range[1], input$year_range[2]) + 
          ggtitle('Forecasted Average Yearly State Land Temperature (C)') + ylab('Avg Temp') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(color = 'black'), plot.title = element_text(hjust = 0.5))
      }
    }
    p
  })
  
  #Creates choropleth plots
  output$plotly_plot <- renderPlotly({
    if(input$dataset == 'By Country'){
      p <- plot_geo(subset(countries, Year == input$year_range & Country %in% input$country_selector))
      p <- p %>% add_trace(z = ~AverageTemperature, color = ~AverageTemperature, colors = 'RdYlBu', text = ~Country, locations = ~Code,
                           reversescale = TRUE, zmin = -20, zmax = 30)
      if(input$light_mode == TRUE){
        options <- list(showframe = FALSE, showcountries = TRUE, showcoastlines = FALSE, projects = list(type = 'Mercator'), bgcolor = '#ffffff')
        p <- p %>% colorbar(title = list(text = 'Avg\nTemp', font = list(color = '#000000')), tickfont = list(color = '#000000'), len = 1) %>% config(displayModeBar = FALSE)
        p <- p %>% layout(title = 'Average Temperature (C) by Country', geo = options, margin = list(b = 20, l = 20, r = 20, t = 50),
                          paper_bgcolor = '#ffffff', font = list(color = '#000000'), height = 300)
      }
      else{
        options <- list(showframe = FALSE, showcountries = TRUE, showcoastlines = FALSE, projects = list(type = 'Mercator'), bgcolor = '#0f2537')
        p <- p %>% colorbar(title = list(text = 'Avg\nTemp', font = list(color = '#ffffff')), tickfont = list(color = '#ffffff'), len = 1) %>% config(displayModeBar = FALSE)
        p <- p %>% layout(title = 'Average Temperature (C) by Country', geo = options, margin = list(b = 20, l = 20, r = 20, t = 50),
                          paper_bgcolor = '#0f2537', font = list(color = '#ffffff'), height = 300)
      }
    }
    else if(input$dataset == 'By State'){
      options <- list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = FALSE, bgcolor = '#0f2537')
      p <- plot_geo(subset(states, Year == input$year_range & State %in% input$state_selector), locationmode = 'USA-states')
      p <- p %>% add_trace(z = ~AverageTemperature, locations = ~Code, color = ~AverageTemperature, colors = 'RdYlBu', 
                           reversescale = TRUE, zmin = -10, zmax = 20)
      if(input$light_mode == TRUE){
        options <- list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = FALSE, bgcolor = '#ffffff')
        p <- p %>% colorbar(title = list(text = 'Avg\nTemp', font = list(color = '#000000')), tickfont = list(color = '#000000'), len = 1) %>% config(displayModeBar = FALSE)
        p <- p %>% layout(title = 'Average Temperature (C) by State', geo = options, margin = list(b = 20, l = 20, r = 20, t = 50), 
                        paper_bgcolor = '#ffffff', font = list(color = '#000000'), height = 300)
      }
      else{
        options <- list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = FALSE, bgcolor = '#0f2537')
        p <- p %>% colorbar(title = list(text = 'Avg\nTemp', font = list(color = '#ffffff')), tickfont = list(color = '#ffffff'), len = 1) %>% config(displayModeBar = FALSE)
        p <- p %>% layout(title = 'Average Temperature (C) by State', geo = options, margin = list(b = 20, l = 20, r = 20, t = 50), 
                          paper_bgcolor = '#0f2537', font = list(color = '#ffffff'), height = 300)
      }
    }
      p
  })
  
  #Dynamically renders data table based on data set, plot type and filter options
  output$datatable <- renderDataTable({
    if(input$dataset == 'Global'){
      if(input$plot == 'Time Series'){
        table <- subset(global_total, Year >= input$year_range[1] & Year <= input$year_range[2])[c('Year', 'LandAverageTemperature')]
      }
      else if(input$plot == 'Time Series with Forecasting'){
        table <- subset(forecast, Year >= max(input$year_range[1], 1850))[c('Year', 'LandAverageTemperature')]
      }
    }
    else if(input$dataset == 'By Country'){
      if(input$plot == 'Time Series'){
        table <- subset(countries, Year >= input$year_range[1] & Year <= input$year_range[2] & Country %in% input$country_selector)[c('Year', 'Country', 'AverageTemperature')]
      }
      else if(input$plot == 'Time Series with Forecasting'){
        table <- subset(countries_forecast, Year >= input$year_range[1] & Year <= input$year_range[2] & Country %in% input$country_selector)[c('Year', 'Country', 'AverageTemperature')]
      }
      else if(input$plot == 'Choropleth'){
        table <- subset(countries, Year == input$year_range & Country %in% input$country_selector)[c('Year', 'Country', 'AverageTemperature')]
      }
    }
    else if(input$dataset == 'By State'){
      if(input$plot == 'Time Series'){
        table <- subset(states, Year >= input$year_range[1] & Year <= input$year_range[2] & State %in% input$state_selector)[c('Year', 'State', 'AverageTemperature')]
      }
      else if(input$plot == 'Time Series with Forecasting'){
        table <- subset(states_forecast, Year >= input$year_range[1] & Year <= input$year_range[2] & State %in% input$state_selector)[c('Year', 'State', 'AverageTemperature')]
      }
      else if(input$plot == 'Choropleth'){
        table <- subset(states, Year == input$year_range & State %in% input$state_selector)[c('Year', 'State', 'AverageTemperature')]
      }
      }
      table
    }, rownames = FALSE)
  
  #Dynamically render main panel plots based on which plot type is selected.
  output$panel <- renderUI(
    if(input$plot == 'Time Series'){
        tabsetPanel(
          tabPanel('Plot', plotOutput('plot')),
          tabPanel('Table', dataTableOutput('datatable'))
        )
    }
    else if(input$plot == 'Time Series with Forecasting'){
      tabsetPanel(
        tabPanel('Plot', plotOutput('plot')),
        tabPanel('Table', dataTableOutput('datatable'))
      )
    }
    else if(input$plot == 'Choropleth'){
        tabsetPanel(
          tabPanel('Plot', plotlyOutput('plotly_plot')),
          tabPanel('Table', dataTableOutput('datatable'))
        )
    }
    )
})

#Runs app
shinyApp(ui, server)