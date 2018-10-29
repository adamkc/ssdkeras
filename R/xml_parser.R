library(xml2)
library(tidyverse)
library(stringr)

parseXML <- function(xml) {
  #print(xml)
  frame <- xml %>%
    xml_find_first("//filename") %>%
    xml_text()
  
  classes <- xml %>%
    xml_find_all("//object") %>%
    xml_find_all("//name") %>%
    xml_text() %>%
    factor(levels = c("greenhouse", "outdoor")) %>%
    as.integer() %>%
    as_tibble() %>%
    magrittr::set_colnames("class_id")
  
  bndbx <- xml %>%
    xml_find_all("//bndbox") %>%
    xml_children() %>%
    xml_integer() %>%
    split(rep(1:dim(classes)[1], each = 4)) %>%
    as_tibble() %>%
    t() %>%
    magrittr::set_colnames(c("xmin", "ymin", "xmax", "ymax")) %>%
    as_tibble() %>%
    select(xmin, xmax, ymin, ymax)
  
  cbind(frame, bndbx, classes) %>%
    as_tibble %>%
    mutate(frame = as.character(frame))
}


data <- list.files("data/Greenhouse", full.names = TRUE) %>%
  discard(!str_detect(., "xml")) %>%
  map(., read_xml) %>%
  map_dfr(parseXML)

splitN <- 0.9
imageNames <-list.files("data/Greenhouse", full.names = FALSE) %>%
  discard(str_detect(., "xml"))
trainset <- sample(imageNames,size = ceiling(length(imageNames)*splitN),replace=FALSE)
valset <- imageNames[!(imageNames %in% trainset)]

write_csv(data[data$frame %in% trainset,], "trainGH.csv")
write_csv(data[data$frame %in% valset,], "valGH.csv")


