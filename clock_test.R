###24hour digital clock

library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { text-align: center; font-family: Arial, sans-serif; margin-top: 100px; }
    #clock {
      font-size: 50px;
      font-weight: bold;
      color: red; /* Clock text color */
      background-color: cyan; /* Clock background */
      padding: 100px;
      border-radius: 10px; /* round corners */
      display: inline-block;
    }
  "))),
  h1("24-Hour Digital Clock"),
  tags$span(textOutput("clock"), id = "clock")  # Corrected placement of ID
)

server <- function(input, output, session) {
  output$clock <- renderText({
    invalidateLater(1000, session)  # Updates every second
    format(Sys.time(), "%H:%M:%S")  # 24-hour format
  })
}

shinyApp(ui, server)

###12 hour clock

library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { text-align: center; font-family: Arial, sans-serif; margin-top: 100px; }
    #clock {
      font-size: 50px;
      font-weight: bold;
      color: white; /* Clock text color */
      background-color: magenta; /* Clock background */
      padding: 50px;
      border-radius: 50px; /* round corners */
      display: inline-block;
    }
  "))),
  h1("12-Hour Digital Clock"),
  tags$span(textOutput("clock"), id = "clock")  # Corrected placement of ID
)

server <- function(input, output, session) {
  output$clock <- renderText({
    invalidateLater(1000, session)  # Updates every second
    format(Sys.time(), "%I:%M:%S %p")  # 12-hour format with AM/PM
  })
}

shinyApp(ui, server)

###Analog Clock
library(shiny)
library(ggplot2)
library(dplyr)

# Function to generate clock hands
get_clock_data <- function() {
  time <- Sys.time()
  
  # Extract time components
  hours <- as.numeric(format(time, "%I"))  # 12-hour format
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  
  # Convert time to angles (rotated by 90 degrees)
  second_angle <- (seconds / 60) * 360 - 90
  minute_angle <- (minutes / 60) * 360 + (seconds / 60) * 6 - 90
  hour_angle <- (hours / 12) * 360 + (minutes / 60) * 30 - 90
  
  # Convert angles to coordinates
  hands <- data.frame(
    hand = c("hour", "minute", "second"),
    x = c(cospi(hour_angle / 180) * 0.5, cospi(minute_angle / 180) * 0.7, cospi(second_angle / 180) * 0.9),
    y = c(sinpi(hour_angle / 180) * 0.5, sinpi(minute_angle / 180) * 0.7, sinpi(second_angle / 180) * 0.9),
    color = c("black", "black", "red"),
    size = c(2, 1.5, 1)
  )
  
  return(hands)
}

# Function to generate clock numbers at correct positions
get_clock_numbers <- function() {
  angles <- seq(0, 330, by = 30) - 90  # Rotate numbers by -90 degrees
  numbers <- data.frame(
    number = 1:12,
    x = cospi(angles / 180) * 0.85,  # Scale positions slightly inside the edge
    y = sinpi(angles / 180) * 0.85
  )
  return(numbers)
}

ui <- fluidPage(
  titlePanel("Analog Clock with Correct Number Positions"),
  plotOutput("clockPlot", width = "400px", height = "400px")
)

server <- function(input, output, session) {
  output$clockPlot <- renderPlot({
    invalidateLater(1000, session)  # Update every second
    hands <- get_clock_data()
    numbers <- get_clock_numbers()
    
    # Create clock face
    clock_face <- ggplot() +
      # Draw clock circle
      geom_point(aes(x = 0, y = 0), size = 5, color = "black") +  # Center dot
      annotate("point", x = cospi(seq(0, 330, by = 30) / 180) * 1,
               y = sinpi(seq(0, 330, by = 30) / 180) * 1, size = 4) +  # Hour markers
      # Add clock numbers at correct positions
      geom_text(data = numbers, aes(x = x, y = y, label = number), size = 6, fontface = "bold") +
      # Add clock hands
      geom_segment(data = hands, aes(x = 0, y = 0, xend = x, yend = y, color = color, size = size),
                   lineend = "round") +
      scale_color_identity() +
      scale_size_identity() +
      theme_void() +
      coord_fixed()
    
    return(clock_face)
  })
}

shinyApp(ui, server)
