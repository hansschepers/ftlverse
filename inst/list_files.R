lsal <- list.files(all.files = T, full.names = T, recursive = T, no.. = T)
lsal[!grepl("(shinyExamples)|(testDirectory)|(Copy)|(rds$)", lsal)]