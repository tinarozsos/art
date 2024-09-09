library(tidyverse)

pages <- list.dirs("fig") |> str_extract("(?<=/).*") |> na.omit()

for (page in pages) {
  files <- list.files(paste0("fig/", page), full.names = TRUE)
  img <- map_chr(files, ~ paste0('<img style="float: left;" src="', ., '" height="350"/>')) |> 
    paste(collapse = "\n")
  
  paste0(
    '<!DOCTYPE html>
    <html>
    <head>
    <style>
    img {
      margin: 10px;
    }
    </style>
    </head>
    <body>',
    img,
    '</body>
    </html>'
  ) |> writeLines(paste0(page, ".html"))
}
