#Removing the daylight saving days-------------

#remove the October's last Sunday-------------

temp = myLoads

OctoberToBeRemoved = which((myLoads$HOUR == 25 & myLoads$Loads!=0))

for(i in 1:length(OctoberToBeRemoved)) {
  dateToBeRemoved = temp$DATE[OctoberToBeRemoved[i]]
  temp = temp[!temp$DATE == dateToBeRemoved, ]
}


#remove the March's last Sunday-------------

MarchToBeRemoved = which((myLoads$HOUR == 24 & myLoads$Loads==0))
for(i in 1:length(MarchToBeRemoved)) {
  dateToBeRemoved = temp$DATE[MarchToBeRemoved[i]]
  temp = temp[!temp$DATE == dateToBeRemoved, ]
}


#remove the other 25 hour values-------------
temp <- temp[!temp$HOUR == 25, ]


#renewing the myLoads data frame-------------
myLoads = temp
rm(temp, dateToBeRemoved, i, j, MarchToBeRemoved, OctoberToBeRemoved)