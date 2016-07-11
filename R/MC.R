#' @export
MCinput <-
  function(id, choice_list) {
    ns <- NS(id)

    tagList(
      selectInput(ns("choices"), label = id, choice_list, selected = NULL),
      textOutput(ns("result")),
      div(style="display:none",
          numericInput(ns("attempts"), label = NULL, value = 0)
      )
    )
  }
#' @export
MC <- function(input, output, session, my_name, hints=TRUE, attempts = 0) {
  output$result <- renderText({
    if (input$choices == "Choose one.") return("You haven't answered yet.")
    else {
      score <- as.numeric(grepl("\\*\\*RIGHT\\*\\*", input$choices))
      # grab information from the input value
      to_show <- value <- input$choices

      to_show <- gsub(".*\\*\\*EMPTY\\*\\*", "", to_show)
      to_show <- gsub(".*\\*\\*WRONG[0-9]*\\*\\*", "Sorry.", to_show)
      to_show <- gsub("^.*:::", "", to_show)

      answer_choice <- gsub(":::.*$", "", value)

      updateNumericInput(session, "attempts", value = isolate(input$attempts) + 1)
      update_scorekeeper(my_name, score, answer_choice, isolate(input$attempts))

      cat(my_name, "\n")

      if (hints) to_show
    }
  })
}

#' @export
update_scorekeeper <- function(problem, score, answer, attempts) {
  ind <- which(problem == Scorekeeper$problem)
  if (length(ind) == 0) {
    # add a new one
    Scorekeeper$score <<- c(Scorekeeper$score, NA)
    Scorekeeper$problem <<- c(Scorekeeper$problem, problem)
    Scorekeeper$answer <<- c(Scorekeeper$answer, answer)
    Scorekeeper$attempts <<- c(Scorekeeper$attempts, attempts)
  } else {
    # update it
    Scorekeeper$attempts[ind] <<- 1 + Scorekeeper$attempts[ind]
    Scorekeeper$score[ind] <<- score
    Scorekeeper$answer[ind] <<- answer
  }
}

#' @export
MC_question <- function(id, ...) {
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
  MCinput(id, choices)
}

