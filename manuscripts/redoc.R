library(redoc)
library(here)
# print(basename(redoc_example_docx()))
# dedoc(redoc_example_docx())
# redoc_diff(redoc_example_edited_docx())

dedoc(here("manuscripts/Aim1_manuscript.docx"), overwrite = T)
redoc_diff(here("manuscripts/Aim1_manuscript_update_test.docx"))

#https://bookdown.org/yihui/rmarkdown-cookbook/word-redoc.html