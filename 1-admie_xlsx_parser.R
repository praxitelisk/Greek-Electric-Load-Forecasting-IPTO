#############################################################################
######MAIN######
#############################################################################

library("xlsx")

# rm(list = ls())

startTime <- proc.time()[3]

xlsFiles <-
  list.files(
    path = "ADMIE",
    pattern = "xlsx",
    all.files = FALSE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE,
    no.. = FALSE
  )


size <- length(xlsFiles)
c <- vector(mode = "numeric", length = 0)
for (i in 1:size) {
  df <- read.xlsx(xlsFiles[i], sheetIndex = 1, colIndex = (1:5))
  
  c <- rbind(c, df)
  
}

test <- c[-c(1,2)]
myLoads <- test[!test$HOUR == 25, ]
rm("test", "df")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime) / 60)


#############################################################################
######FUNCTION SECTION######
#############################################################################