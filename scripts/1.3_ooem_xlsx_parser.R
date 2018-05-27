#####################################################################################
#Parsing the xlsx ooem prediction files to get the predictied loads from ooem--------
#####################################################################################

#load or even install required libraries
if (!("xlsx" %in% rownames(installed.packages()))) {
  install.packages("xlsx")
  library("xlsx")
} else {
  library("xlsx")
}


startTime <- proc.time()[3]

ooemXLSFiles <-
  list.files(
    path = "OoEM",
    pattern = "xls",
    all.files = FALSE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE,
    no.. = FALSE
  )

first.file = 1
last.file = length(ooemXLSFiles)
ooem_predictions = data.frame()

for(i in 1:length(ooemXLSFiles)) {
  
  dfa <- read.xlsx(ooemXLSFiles[i], sheetIndex = 1, startRow = 1, endRow = 6)
  cat("parsing: ", i,"# ", ooemXLSFiles[i], "\n", sep="")
  
  # if(first.file == i) {
  #   
  #   dfa = dfa[, -c(26:28)]
  #   dfa = t(dfa)
  #   rownames(dfa) = 1:dim(dfa)[1]
  #   dfa = dfa[, -1]
  #   rownames(dfa) = 1:dim(dfa)[1]
  #   temp = dfa[1, ]
  #   dfa = dfa[-1, ]
  #   rownames(dfa) = 1:dim(dfa)[1]
  #   dfa1 = as.numeric(dfa[, 1])
  #   dfa2 = as.numeric(dfa[, 4])
  #   dfa = dfa1 - dfa2
  #   dfa = data.frame("ooem_predictions" = dfa)
  #   
  #   #first day of the testSet
  #   dfa = dfa[dim(dfa)[1], ]
  #   dfa = data.frame("ooem_predictions" = dfa)
  #   
  # 
  #   
  #   
  # } else if (last.file == i) {
  #     
  #     dfa = dfa[, -c(26:28)]
  #     dfa = t(dfa)
  #     rownames(dfa) = 1:dim(dfa)[1]
  #     dfa = dfa[, -1]
  #     rownames(dfa) = 1:dim(dfa)[1]
  #     temp = dfa[1, ]
  #     dfa = dfa[-1, ]
  #     rownames(dfa) = 1:dim(dfa)[1]
  #     dfa1 = as.numeric(dfa[, 1])
  #     dfa2 = as.numeric(dfa[, 4])
  #     dfa = dfa1 - dfa2
  #     dfa = data.frame("ooem_predictions" = dfa)
  #     
  #     #last day of the data Set
  #     dfa = dfa[-24, ]
  #     dfa = data.frame("ooem_predictions" = dfa)
  # 
  #   } else {
  #       
  #       dfa = dfa[, -c(27:28)]
  #       dfa = t(dfa)
  #       rownames(dfa) = 1:dim(dfa)[1]
  #       dfa = dfa[, -c(1, 3, 4)]
  #       rownames(dfa) = 1:dim(dfa)[1]
  #       #temp = dfa[1, ]
  #       dfa = dfa[-1, ]
  #       rownames(dfa) = 1:dim(dfa)[1]
  #       df1 = as.numeric(dfa[, 1])
  #       df2 = as.numeric(dfa[, 2])
  #       dfa = df1 - df2
  #       dfa = data.frame("ooem_predictions" = dfa)
  #       
  #       #march last Sunday 23-hour day issue
  #       if (is.na(dfa$ooem_predictions[24])) {
  #         
  #         dfa$ooem_predictions[24] = mean(c(dfa$ooem_predictions[23], dfa$ooem_predictions[1]))
  #         
  # }
  # 
  # #October last Sunday 25-hour day issue
  # dfa = dfa[-c(25), ]
  # dfa = data.frame("ooem_predictions" = dfa)
  # }
  
  temp = t(dfa)
  temp = temp[, -c(1, 3, 4)]
  temp = temp[-1, ]
  row.names(temp) = 1:dim(temp)[1]
  temp = temp[-((dim(temp)[1] - 1):dim(temp)[1]),]
  flag = temp[, 1] > 0
  temp = temp[flag, ]
  temp = as.data.frame(temp)
  temp[, 1] = as.numeric(as.character(temp[, 1]))
  temp[, 2] = as.numeric(as.character(temp[, 2]))
  
  if(first.file == i) {
    temp = temp[dim(temp)[1], ]
  }
  
  else if (last.file == i) {
    temp = temp[1:(dim(temp)[1]-1), ]
  }
  
  df = data.frame("ooem_predictions"= temp[, 1] - temp[, 2])
  
  ooem_predictions = rbind(ooem_predictions, df)
}


rm(df1, df2, dfa, dfa1, dfa2, first.file, last.file, i, temp, df, flag)


cat("elapsed time in minutes: ", (proc.time()[3] - startTime) / 60)
rm(startTime)