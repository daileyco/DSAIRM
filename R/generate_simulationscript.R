#' @title A function that prepares simulator code to write file for code download
#'
#' @description This function is called when the user clicks the "Download Code" button in
#' the simulator app. It creates an R script containing the code to run the
#' simulation specified in the app from which the button is clicked and to generate output
#' similar to what is shown in the app.
#'
#' @param modelsettings A list with model settings.
#' #' Needs to contain list elements with names and values for all inputs expected
#' by simulation function. Assumed to be the return of generate_modelsettings()
#'
#' @return Creates a character vector for an R script that runs the simulation specified in the app
#' and returns the text and plots created by the simulation.
#' @export

generate_simulationscript <- function(modelsettings) {


  # Utility function for processing deparsed function arguments
  ## deparse1() will return character vector like 'arg = value, arg = value'
  deparse_with_linebreak_and_tabs <- function(the_call, include_tabs=TRUE, n_characters_offset=0)
  {
    temp <- deparse1(the_call)

    if(include_tabs){

      #try to align each with first argument
      n_leading_characters <- grep("\\(", unlist(strsplit(temp, split="")))[1]+n_characters_offset
      n_tabs <- floor(n_leading_characters/2)
      gsub_string <- paste0(",\n", paste0(rep("\t", n_tabs), collapse = ""))

    }else{
      gsub_string <- ",\n"
    }

    temp <- gsub(", ", gsub_string, temp)

    return(temp)
   }

  # Generate the set of simulation function calls specified in the model settings
  sim_fctcalls <- generate_fctcalls(modelsettings)

  # Set up character vectors to write script
  sim_fctcalls_code <- lapply(sim_fctcalls,
                              deparse_with_linebreak_and_tabs,
                              n_characters_offset = nchar("res1 <- ")
                              )
  sim_fctcalls_code <- unlist(sim_fctcalls_code)


  # Opening lines
  opening_lines <- paste0("# R code to run current DSAIRM scenario\n",
                          "## model type = ", modelsettings$modeltype,
                          "\n\n",
                          "library(DSAIRM)",
                          "\n")

  modelsettings_lines <- paste0("modelsettings <- ",
                                deparse_with_linebreak_and_tabs(modelsettings,
                                                                n_characters_offset = nchar("modelsettings <- ")),
                                '\n')

  function_lines <- paste0(paste0("res",
                                  1:length(sim_fctcalls_code),
                                  " <- ",
                                  sim_fctcalls_code),
                           collapse = '\n')

  # function_lines <- paste('#this is a list of the simulation function calls',
  #                         paste0('fctcalls <- ', deparse1(sim_fctcalls)),
  #                         '#this executes the call(s) to the function(s) to be run',
  #                         '#results are returned as nested list, the exact structure of the list depends',
  #                         '#on the models/app that is executed',
  #                         'simlist <- lapply(fctcalls, function(this_fctcall){try(eval(this_fctcall))})',
  #                         sep = '\n')

  simlist_lines <- paste0('#compile simulation results in a list\n',
                          'simlist <- list(',
                          paste0('res', 1:length(sim_fctcalls_code), collapse = ","),
                          ')')

  checks_lines <- paste('#error handling',
                        '#check if sim function(s) ran ok',
                        '#we expect a list if things worked ok, otherwise an error string is returned',
                        '#which will be passed to caller',
                        'for (n in 1:length(simlist))\n{\nchecksim <- check_simresults(simlist[[n]])\n# an error occured\nif (is.character(checksim)) {return(checksim)}\n}',
                        sep = "\n")



  #need to figure this out to not hide behind generate_results()
  results_lines <- paste0('# to have results similar to shiny GUI\n',
                          'all_results <- generate_results(simlist, as.call(', deparse1(sim_fctcalls),'))',
                          '\n')




  plotting_lines <- paste0("generate_",
                           modelsettings$plotengine,
                           "(all_results)",
                           "\n",
                           "cat(gsub('<br/>', '\\n', generate_text(all_results)))")

  closing_lines <- paste0("\n\n# Happy simulating! :)")

  # Writing to file
  output_text <- paste(opening_lines,
                       modelsettings_lines,
                       function_lines,
                       simlist_lines,
                       results_lines,
                       plotting_lines,
                       closing_lines, sep = "\n")
  return(output_text)
}
