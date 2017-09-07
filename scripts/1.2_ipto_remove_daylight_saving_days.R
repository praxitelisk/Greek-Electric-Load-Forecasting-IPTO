#Removing the daylight saving days-------------

#remove the October's last Sunday-------------

temp = myLoads

# OctoberToBeRemoved = which((temp$HOUR == 25 & temp$Loads!=0))
# datesToBeRemoved = temp$DATE[OctoberToBeRemoved]
# 
# for(i in 1:length(datesToBeRemoved)) {
#   #print(datesToBeRemoved[i])
#   temp = temp[!temp$DATE == datesToBeRemoved[i], ]
# }


#remove the March's last Sunday-------------

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
#rm(temp, dateToBeRemoved, i, MarchToBeRemoved, OctoberToBeRemoved)


#re-indexing
row.names(myLoads) <- 1:nrow(myLoads)