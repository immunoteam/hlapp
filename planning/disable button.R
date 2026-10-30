library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5), # Bootstrap 5 theme
  tags$h3("Task Button Demo"),

  # Task button with spinner
  bslib::input_task_button(
    id = "run_task",
    label = "Run Long Task",
    status = "primary",
    icon = icon("cogs"),
    spinner = TRUE
  ),

  # Output text to show task status
  verbatimTextOutput("task_status")
)

server <- function(input, output, session) {
  # Reactive value to track task completion
  task_done <- reactiveVal(FALSE)

  observeEvent(input$run_task, {
    task_done(FALSE) # Reset status

    # Simulate a long task
    Sys.sleep(3) # 3-second delay

    task_done(TRUE) # Task completed
  })

  # Display task status
  output$task_status <- renderText({
    if (input$run_task == 0) {
      "Click the button to start the task."
    } else if (!task_done()) {
      "Task is running..."
    } else {
      "Task completed successfully!"
    }
  })
}

shinyApp(ui, server)
