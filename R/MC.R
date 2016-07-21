#' @export
MCinput <-
  function(id, choice_list) {
    ns <- NS(id)

    tagList(
      div(style="display:inline-block",
          selectInput(ns("choices"), label = NULL, choice_list,
                  selected = NULL, width = "100pt")),
      div(style="display:inline-block",
          textOutput(ns("result"))),
      div(style="display:inline-block",
          a(href="#", title="The item id.",
            span(title=id, " #"))),
      div(style="display:none",
          numericInput(ns("attempts"), label = NULL, value = 0)
      )
    )
  }
#' @export
MC <- function(input, output, session, my_name, hints=TRUE, for_scores = NULL) {
  output$result <- renderText({
    if (grepl("You haven't answered yet.", input$choices)) {
      return("You haven't answered yet.")
    } else {
      score <- as.numeric(grepl("\\*\\*RIGHT\\*\\*", input$choices))
      # grab information from the input value
      to_show <- value <- input$choices

      to_show <- gsub(".*\\*\\*EMPTY\\*\\*", "", to_show)
      to_show <- gsub(".*\\*\\*WRONG[0-9]*\\*\\*", "Sorry.", to_show)
      to_show <- gsub("^.*:::", "", to_show)

      answer_choice <- gsub(":::.*$", "", value)

      # next line not needed since attempts in tabulated in the scorekeeper
      # updateNumericInput(session, "attempts", value = isolate(input$attempts) + 1)
      if (is.null(for_scores)) warning("No scorekeeper created.")
      else for_scores$update(my_name, score, answer_choice)

      # cat(my_name, "\n")

      if (hints) to_show
    }
  })
}


#' @export
MC_question <- function(id, ..., hints = TRUE) {
  if ( ! exists(".the_scorekeeper", envir = .GlobalEnv) )
    stop("Must create '.the_scorekeeper' in the global environment")
  for_scores <- eval(.the_scorekeeper, env = .GlobalEnv)
  choices <- list(...)

  choices <- c("Choose one." = "**EMPTY**You haven't answered yet.",
               choices)
  empty_response <- which(names(choices) == "")
  names(choices)[empty_response] <-
    unlist(choices[empty_response])
  choices[empty_response] <-
    paste0("**WRONG", empty_response, "**")
  shown_text <- names(choices)
  choices[] <- as.list(paste0(shown_text, ":::", choices[]))
  callModule(MC, id, id, hints = hints, for_scores)

  MCinput(id, choices)
}

