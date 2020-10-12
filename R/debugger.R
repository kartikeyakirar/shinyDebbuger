#' add_debugger_dir
#'
#' This function takes application input and output directory as input parameter
#' and update the shiny application code by adding messages for function and reactive expression
#' if you don't provide the output directory it ask you to overwrite existing directory or
#' provide output directory location.
#'
#' @param input_dir Path to the shiny application
#' @param output_dir Path to output directory where you want to save shiny application . default value is NULL to overwrite.
#'
#' @export
add_debugger_dir <- function(input_dir, output_dir = NULL) {
    if(!dir.exists(input_dir)) {
        stop("Directory does not exist")
    }

    if (is.null(output_dir)) {
        message("You did not provide the output director. Do you want to overwrite on input path?")
        message("(y/n)")
        response <- readline(prompt="")
        if (tolower(response) == "n") {
            message("Enter output direcorty. (without qoutes)")
            output_dir <- readline(prompt="")
        } else if (tolower(response) == "y"){
            output_dir <- input_dir
        } else {
            stop("Wrong input id provided")
        }
    }

    filePaths <- list.files(input_dir,pattern = "(?i).R$",all.files = TRUE,full.names = TRUE,recursive = TRUE)

    for(filePath in filePaths) {
        add_debugger_file(filePath, input_dir, output_dir)
    }

}

#' add_debugger_file
#'
#' This function takes R file and update it with message
#'
#' @param path_from_file R file which you want to update
#' @param input_dir Path to the shiny application
#' @param output_dir Path to output directory where you want to save shiny application . default value is NULL to overwrite.
#'
#' @export
add_debugger_file<- function(path_from_file, input_dir, output_dir) {
    file_name <- basename(path_from_file)
    dir_name <- gsub(input_dir,"", dirname(path_from_file))
    code_file <- readLines(path_from_file)
    updated_code <- find_function_index(code_file)
    if (dir_name == input_dir) {
        dir_loc<-glue::glue(output_dir,  file_name)
    } else {
        dir_loc<-glue::glue(output_dir, dir_name, "/",  file_name)
    }
    message("File updated at locaton: ", dir_loc)
    writeLines(updated_code, dir_loc)
}

