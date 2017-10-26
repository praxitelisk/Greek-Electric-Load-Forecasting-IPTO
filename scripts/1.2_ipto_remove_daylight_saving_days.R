###################################################
####Resolving the daylight saving days#############
###################################################


temp = myLoads


# OctoberToBeRemoved = which((temp$HOUR == 25 & temp$Loads!=0))
# datesToBeRemoved = temp$DATE[OctoberToBeRemoved]
# 
# for(i in 1:length(datesToBeRemoved)) {
#   #print(datesToBeRemoved[i])
#   temp = temp[!temp$DATE == datesToBeRemoved[i], ]
# }


OctoberDayLightSavingIndex = which(temp$HOUR == 25 & temp$Loads > 0)
MarchDayLightSavingIndex = which(temp$HOUR == 24 & temp$Loads == 0)


#October's last Sunday-------------
for(i in 1:length(OctoberDayLightSavingIndex)) {
  
  for(j in 4:25) {
    temp[OctoberDayLightSavingIndex[i] - 25 + j, 2] = j - 1
  }
  
  #temp = temp[-(OctoberDayLightSavingIndex[i] - 25 + 2), ]
  
}


#March's last Sunday-------------
for(i in 1:length(MarchDayLightSavingIndex)) {
  
  for(j in 23:2) {
    temp[MarchDayLightSavingIndex[i] - 23 + j, 3] = temp[MarchDayLightSavingIndex[i] - 23 + j - 1, 3]
  }
  
  
}


temp = temp[-(MarchDayLightSavingIndex - 23 + 2), ]





# MarchToBeRemoved = which((temp$HOUR == 24 & temp$Loads==0))
# datesToBeRemoved = temp$DATE[MarchToBeRemoved]
# 
# for(i in 1:length(datesToBeRemoved)) {
#   #print(datesToBeRemoved[i])
#   temp = temp[!temp$DATE == datesToBeRemoved[i], ]
# }


#remove the other 25 hour values-------------
temp <- temp[!temp$HOUR == 25, ]


#renewing the myLoads data frame-------------
myLoads = temp


#re-indexing
row.names(myLoads) <- 1:nrow(myLoads)


#removing auxiliary variabels
rm(temp, i, OctoberDayLightSavingIndex, MarchDayLightSavingIndex)