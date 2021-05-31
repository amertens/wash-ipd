# uncomment run the line below if googledrive package is not installed
# install.packages("googledrive")

setwd("C:/Users/andre/Documents/wash-ipd/manuscripts")

#---- render your chapter as a Word document
rmarkdown::render(
  "Aim1_manuscript.Rmd",
  output_format = "word_document")

#---- run this next part only once
file <- googledrive::drive_upload(
  "Aim1_manuscript.docx", 
  type = "document")

# get the file id (copy this to your clipboard)
#file$id

#----

# paste file id, run this next part every time you want to push a new version to Google Drive
id <- "1P83sSXb1I7jIUzeYw68jke58TP5GAKv0qN54XY6Enhg"
googledrive::drive_update(
  googledrive::as_id(id), 
  "Aim1_manuscript.docx")