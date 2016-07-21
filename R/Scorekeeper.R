#' Manage score-keeping
#'
#' Creates a storage object which can then be accessed by $update() and $report()
#' functions to hold permanently info on attempts and the score of the most
#' recent answer.
#'
#' @export
createScorekeeper <- function() {
  # Generate a random session ID
  session_id <- paste(sample(c(0:9, LETTERS[1:6]),size=10,replace=TRUE),collapse="")
  cache <- data.frame(problem = "start session",
                      answer = NA, score = NA, attempts=NA,
                      when=Sys.time(),
                      stringsAsFactors = FALSE)
  # a function to add information into the cache
  update <- function(problem, score, answer, session = session_id) {
    ind <- which(problem == cache$problem)
    if (length(ind) == 0) {
      # add a new one
      new_data <- data.frame(problem = problem,
                             answer = answer, score = score,
                             attempts = 1, when = Sys.time(),
                             stringsAsFactors = FALSE)
      cache <<- rbind(cache, new_data)
    } else {
      # update it
      cache$attempts[ind] <<- 1 + cache$attempts[ind]
      cache$score[ind] <<- score
      cache$problem[ind] <<- problem
      cache$answer[ind] <<- answer
      cache$when[ind] <<- Sys.time()
    }
  }
  # what are the scores, so far
  report <- function(){ cache }
  # submit the scores
  save_scores <- function(userid = "bogus", assignment = "assignment X",
                          ip = "ip address Y",
                          filename = "Scores.rda") {
    cat("Saving scores!\n")
    # mark the end of the session
    update(problem = "end session", score = NA, answer = NA)
    # the scores to save
    warning("Got here 1!")
    These_scores <- eval(.the_scorekeeper, envir = .GlobalEnv)$report()
    # add in the user ID
    These_scores$user <- userid
    These_scores$assignment <- assignment
    These_scores$ip <- ip
    # Append to the data file storage
    # Is the file there?
    if (0 == length(list.files(path = ".", pattern = filename))) {
      Scores <- These_scores
    } else {
      load(filename)
      Scores <- rbind(Scores, These_scores)
    }

    save(Scores, file = filename)
  }


  list(update = update, report = report, save_scores = save_scores)
}


