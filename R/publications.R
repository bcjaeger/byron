

library(rbibutils)
library(bib2df)
library(tidyverse)
library(glue)
library(lubridate)

me <- bib2df(file = 'dev/publication/byron.bib') %>%
 set_names(tolower(names(.))) %>%
 mutate(title = str_remove_all(title, "\\{|\\}"),
        date = mdy(glue("{month}-{day}-{year}")),
        bibtexkey = str_remove(bibtexkey, "_\\d.*$"))

pub_write <- function(ref, overwrite = FALSE){

 if(!is.Date(ref$date)) stop("fix date for ", ref$bibtexkey)
 if(!str_detect(ref$url, "^https")) stop("fix url for ", ref$bibtexkey)

 index_string <- "
---
title: '{ref$title}'
author:
- {paste(ref$author[[1]], collapse = '\n- ')}
date: '{ref$date}'
categories:
  - Research
links:
- icon: file-richtext-fill
  name: Publication
  url: {ref$url}
---

#### Abstract

{ref$abstract}

"

 yaml <- glue::glue(index_string)

 dir_name <- glue("dev/publication/{ref$date}-{ref$bibtexkey}")

 if(!dir.exists(dir_name)){
  dir.create(dir_name)
 }

 fpath <- glue("{dir_name}/index.qmd")

 if(overwrite && file.exists(fpath)) file.remove(fpath)

 if(!file.exists(fpath) || overwrite){

  cat(yaml, file = fpath)

 }

}

pub_move <- function(fpath, delete = FALSE, overwrite = FALSE){

 destination <- glue("publication/{basename(fpath)}")

 if(!dir.exists(destination)) dir.create(destination)

 if(file.exists(file.path(destination, "index.qmd")) && !overwrite){
  stop("file exists in destination but overwrite is FALSE",
       call. = FALSE)
 }

 file.copy(from = file.path(fpath, "index.qmd"),
           to = file.path(destination, "index.qmd"))

 if(delete){
  file.remove(file.path(fpath, "index.qmd"))
  unlink(fpath, recursive = TRUE)
 }

}

for(i in seq(nrow(me))){

 ref <- as.list(slice(me, i))

 pub_write(ref, overwrite = FALSE)

}



pub_move("dev/publication/2019-09-01-jaeger_oblique/")


