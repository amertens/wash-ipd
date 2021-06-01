# uncomment run the line below if googledrive package is not installed
# install.packages("googledrive")

library(googledrive)
setwd("C:/Users/andre/Documents/wash-ipd/manuscripts")

#---- render your chapter as a Word document
rmarkdown::render(
  "Aim1_manuscript.Rmd",
  output_format = "word_document")

#---- run this next part only once
file1 <- googledrive::drive_upload(
  "Aim1_manuscript.docx", 
  type = "document")

# get the file id (copy this to your clipboard)
#file1$id

#----

# paste file id, run this next part every time you want to push a new version to Google Drive
id_aim1 <- "1P83sSXb1I7jIUzeYw68jke58TP5GAKv0qN54XY6Enhg"
googledrive::drive_update(
  googledrive::as_id(id_aim1), 
  "Aim1_manuscript.docx")



#aim 2
rmarkdown::render(
  "Aim2_manuscript.Rmd",
  output_format = "word_document")
file2 <- googledrive::drive_upload(
  "Aim2_manuscript.docx", 
  type = "document")
# get the file id (copy this to your clipboard)
#file2$id

id_aim2 <- "1NRHYz9ZEqsQmI7hviHCdwN4N3aHySgHIXxpR_JK5nT8"
googledrive::drive_update(
  googledrive::as_id(id_aim2), 
  "Aim2_manuscript.docx")