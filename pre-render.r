
if(!dir.exists("purl")){dir.create("purl")}

qmd_files <- list.files(path = c("statistik"), pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

sapply(qmd_files, \(x){
  output_r <- file.path("purl", paste0(tools::file_path_sans_ext(basename(x)), ".R"))
  output_qmd <- file.path("purl",basename(x))
  knitr::purl(x, output = output_r, documentation = 2L, quiet = TRUE)
  file.copy(x, output_qmd, overwrite = TRUE)
    }) |> 
  invisible()
