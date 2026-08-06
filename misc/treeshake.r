
args <- commandArgs(trailingOnly = TRUE)

treeshake <- function(qmd){
    tmp <- tempfile(fileext = ".R")

    knitr::purl(qmd, output = tmp, documentation = 0, quiet = TRUE)

    parseData <- getParseData(parse(tmp), includeText = TRUE)

    functionCalls <- unique(parseData[parseData$token == "SYMBOL_FUNCTION_CALL", "text"])

    libraryCalls <- parseData[parseData$token == "SYMBOL_FUNCTION_CALL" & parseData$text %in% c("library", "require"),]
    libraryCalls <- parseData[parseData$id %in% libraryCalls$parent,]
    libraryCalls <- parseData[parseData$id %in% libraryCalls$parent,]
    libraryCalls <- libraryCalls$text

    msgout <- tempfile()
    zz <- file(msgout, open = "wt")
    sink(zz, type = "message")
    eval(parse(text = libraryCalls))
    sink(type = "message")
    file.remove(msgout)

    names(functionCalls) <- functionCalls
    matchPkg <- vapply(functionCalls, 
                    FUN = (\(f) grep("^package:", getAnywhere(f)$where, value = TRUE)[1]), 
                    FUN.VALUE = character(1))

    packages <- search()
    packages <- grep("^package:", packages, value = TRUE)
    packages <- setdiff(packages, c("package:base", "package:methods", "package:datasets", "package:utils", "package:grDevices", "package:graphics", "package:stats"))
    packages <- setdiff(packages, unique(matchPkg))

    if(length(packages) > 0) {
        warning(paste("Unused packages",paste(packages, collapse = ", ")))
    } else {
        message(paste("No unused packages found"))
    }
}

treeshake(args[1])