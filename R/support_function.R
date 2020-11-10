
find_function_index <- function(code) {
    # function message
    function_index <- grep("function",x = code,ignore.case = FALSE,fixed = TRUE)
    function_messages <- getFunctionName(function_index, code, "function")
    code <- insert_message(code, function_messages)
    # observeEvent message
    function_index <- grep("observeEvent",x = code,ignore.case = FALSE,fixed = TRUE)
    function_messages <- getFunctionName(function_index, code, "observeEvent")
    code <- insert_message(code, function_messages)
    # renderUI message
    function_index <- grep("renderUI",x = code,ignore.case = FALSE,fixed = TRUE)
    function_messages <- getFunctionName(function_index, code, "renderUI")
    code <- insert_message(code, function_messages)

    code
}

getFunctionName <- function(function_index, code, funcType) {
    if(funcType == "function") {
        sapply(function_index, function(x) {
            funName <- trimws(stringi::stri_extract(code[x],regex = ".*(?=<-)|.*(?==)"))
            if (!is.na(funName)) {
                paramNames <-  strsplit(gsub(".*\\((.*)\\).*", "\\1", code[x]), ",")[[1]]

                if(length(paramNames) > 0) {
                    megTxt <- glue::glue("if(global_debugger) message('function:{funName}:    [{paramNames}]')", .transformer = parameter_transformer(debug = TRUE))
                } else {
                    megTxt <- glue::glue("if(global_debugger) message('function:{funName}')", .transformer = parameter_transformer(debug = TRUE))

                }
                names(megTxt) <- as.character(x)
                megTxt
            }

        })
    } else if(funcType == "observeEvent") {
        sapply(function_index, function(x) {
            funName <- gsub("observeEvent(", "", trimws(stringi::stri_extract(code[x],regex = ".*(?=,)")), fixed = TRUE)
            if (!is.na(funName)) {
                megTxt <- glue::glue("if(global_debugger) message('observeEvent:{funName}')", .transformer = parameter_transformer(debug = TRUE))
                names(megTxt) <- as.character(x)
                megTxt
            }
        })
    } else if(funcType == "reactive") {
        sapply(function_index, function(x) {
            funName <- trimws(stringi::stri_extract(code[x],regex = ".*(?=<-)|.*(?==)"))
            if (!is.na(funName)) {
                megTxt <- glue::glue("if(global_debugger) message('reactive:{funName}')", .transformer = parameter_transformer(debug = TRUE))


                names(megTxt) <- as.character(x)
                megTxt
            }
        })
    } else if(funcType == "renderUI") {
        sapply(function_index, function(x) {
            if (!is.na(x)) {
                funName <- trimws(stringi::stri_extract(code[x],regex = ".*(?=<-)|.*(?==)"))

                megTxt <- glue::glue("if(global_debugger) message('renderUI:{funName}')", .transformer = parameter_transformer(debug = TRUE))


                names(megTxt) <- as.character(x)
                megTxt
            }

        })
    }
}

insert_message <- function(code, function_messages) {
    incr <- 0
    for (i in names(function_messages)) {
        code <- c(code[1: (as.numeric(i) + incr)], function_messages[i], code[(as.numeric(i)+1 + incr):length(code)])
        incr<- incr+1
    }
    code
}

add_global_parameter <- function(input_dir) {
    if(!file.exists(paste0(input_dir,"/global.R"))) {
        file.create(paste0(input_dir,"/global.R"))
        updated_code <- "global_debugger <- TRUE"
        writeLines(updated_code, paste0(input_dir,"/global.R"))
    } else {
        file.create(paste0(input_dir,"global_debugger.R"))
        updated_code <- "global_debugger <- TRUE"
        writeLines(updated_code, paste0(input_dir,"/global_debugger.R"))

        code_file <- readLines(paste0(input_dir,"/global.R"))
        updated_code <- paste0("source('global_debugger.R', local = TRUE)")
        code_file <- c(updated_code, code_file)
        writeLines(code_file, paste0(input_dir,"/global.R"))
    }
    message("Added global parameter for debugger package.")

}

parameter_transformer <- function(debug = FALSE) {
    function(text, envir) {
        out <- glue::identity_transformer(text, envir)
        if (debug) {
            if (is.null(out)) {
                out <- "NULL"
            }

            if (identical(out, character(0))) {
                out <- "character(0)"
            }

            if (identical(out, numeric(0))) {
                out <- "numeric(0)"
            }

            if (identical(out, logical(0))) {
                out <- "logical(0)"
            }

            if (identical(out, list())) {
                out <- "list()"
            }

            out <- glue::glue_collapse(out, sep = ',')
        } else {
            if (is.null(out) || identical(out, character(0)) || identical(out, numeric(0)) || identical(out, logical(0)) || identical(out, list())) {
                out <- ''
            }
        }
        return(out)
    }
}
