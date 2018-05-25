library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(My.stepwise)
set.seed(4345435)

##################################### Preliminary uploading and Such #################################

# First Read in the Data and Convert to Date-Time Objects, also removing empty tracks:
spot = read.csv("data.csv")
spot$Date = as.Date(as.character(spot$Date))
spot = filter(spot, Track.Name != "")

# Export a unique csv of tracks:
#attach(spot)
uniquetracks = unique(data.frame(Track.Name,Artist,URL))

#write.csv(uniquetracks, file = 'uniquetracks.csv', row.names = F)

# Next Invert the Ranks to make Position a point-system:
A = function(x) 201 - x
invrank = data.frame(lapply(spot[1], A), spot[2:7] )

# Add regions by country
countryplus = read.csv("countup.csv")
invrank = merge(invrank, countryplus, by="Region")

################# Add the startdates, days from start and exclude songs that were on the charts #####
#   on the first day of observation:
attach(invrank)
startdate = aggregate(Date, list(Track.Name), FUN = min)
startdate = select(startdate,Track.Name = Group.1 , Start.Date = x)

enddate = aggregate(Date, list(Track.Name), FUN = max)
enddate = select(enddate, Track.Name = Group.1 , End.Date = x)

startdate$FinalTimeonCharts = as.Date(as.character(enddate$End.Date))-as.Date(as.character(startdate$Start.Date)) + 1
startdate$End.Date = enddate$End.Date
##################### Contained section for Merging Regions Start Dates and Such ########
startdateinregion = aggregate(Date, list(Track.Name, Region.1), FUN = min)
startdateinregion = select(startdateinregion,Track.Name = Group.1 , Region.1 = Group.2, Start.Date.Region = x)

enddateinregion = aggregate(Date, list(Track.Name, Region.1), FUN = max)
enddateinregion = select(enddateinregion,Track.Name = Group.1 , Region.1 = Group.2, End.Date.Region = x)

startdateinregion$RegionTimeonCharts = as.Date(as.character(enddateinregion$End.Date))-as.Date(as.character(startdateinregion$Start.Date)) + 1
startdateinregion$End.Date.Region = enddateinregion$End.Date.Region

astarts = startdateinregion
astarts = filter(astarts, Region.1 == "A")
astarts = select(astarts, Track.Name, Start.Date.A = Start.Date.Region, End.Date.A = End.Date.Region, timein.A = RegionTimeonCharts)
eustarts = startdateinregion
eustarts = filter(eustarts, Region.1 == "EU")
eustarts = select(eustarts, Track.Name, Start.Date.EU = Start.Date.Region, End.Date.EU = End.Date.Region, timein.EU = RegionTimeonCharts)
nstarts = startdateinregion
nstarts = filter(nstarts, Region.1 == "N")
nstarts = select(nstarts, Track.Name, Start.Date.N = Start.Date.Region, End.Date.N = End.Date.Region, timein.N = RegionTimeonCharts)
sastarts = startdateinregion
sastarts = filter(sastarts, Region.1 == "SA")
sastarts = select(sastarts, Track.Name, Start.Date.SA = Start.Date.Region, End.Date.SA = End.Date.Region, timein.SA = RegionTimeonCharts)
ostarts = startdateinregion
ostarts = filter(ostarts, Region.1 == "O")
ostarts = select(ostarts, Track.Name, Start.Date.O = Start.Date.Region, End.Date.O = End.Date.Region, timein.O = RegionTimeonCharts)
lastarts = startdateinregion
lastarts = filter(lastarts, Region.1 == "LA")
lastarts = select(lastarts, Track.Name, Start.Date.LA = Start.Date.Region, End.Date.LA = End.Date.Region, timein.LA = RegionTimeonCharts)
mestarts = startdateinregion
mestarts = filter(mestarts, Region.1 == "ME")
mestarts = select(mestarts, Track.Name, Start.Date.ME = Start.Date.Region, End.Date.ME = End.Date.Region, timein.ME = RegionTimeonCharts)

## Merging the Region Data
regionstarts = Reduce(function(x, y) merge(x, y, all=TRUE, by="Track.Name"), list(astarts, nstarts, eustarts, sastarts, lastarts, ostarts, mestarts, startdate))
regionstarts$timein.A[is.na(regionstarts$timein.A)] = 0
regionstarts$timein.EU[is.na(regionstarts$timein.EU)] = 0
regionstarts$timein.N[is.na(regionstarts$timein.N)] = 0
regionstarts$timein.SA[is.na(regionstarts$timein.SA)] = 0
regionstarts$timein.O[is.na(regionstarts$timein.O)] = 0
regionstarts$timein.LA[is.na(regionstarts$timein.LA)] = 0
regionstarts$timein.ME[is.na(regionstarts$timein.ME)] = 0

regionstarts$timein.A = as.numeric(regionstarts$timein.A)
regionstarts$timein.EU = as.numeric(regionstarts$timein.EU)
regionstarts$timein.N = as.numeric(regionstarts$timein.N)
regionstarts$timein.SA = as.numeric(regionstarts$timein.SA)
regionstarts$timein.O = as.numeric(regionstarts$timein.O)
regionstarts$timein.LA = as.numeric(regionstarts$timein.LA)
regionstarts$timein.ME = as.numeric(regionstarts$timein.ME)
regionstarts$FinalTimeonCharts = as.numeric(regionstarts$FinalTimeonCharts)



# I here merge the startdates to the original data
invstart = merge(invrank, regionstarts, by="Track.Name", all = TRUE)

########## I filtered the Regions to not include first Day **** CHANGED **** #######
regionstartsfil = regionstarts



############## This is a filtering that used to be important and was reflected in the data selection ###### 
invstartfil = filter(invstart, invstart$Start.Date != "2017-01-01")

############## Also abandoned Survival Info ###########

allregiondata = na.omit(invstartfil)

survivaldata = allregiondata

survivaldata$lastinA = numeric(length(survivaldata$Track.Name))
survivaldata$lastinA[which(survivaldata$Date == survivaldata$End.Date.A)] = 1
survivaldata$lastinLA = numeric(length(survivaldata$Track.Name))
survivaldata$lastinLA[which(survivaldata$Date == survivaldata$End.Date.LA)] = 1
survivaldata$lastinSA = numeric(length(survivaldata$Track.Name))
survivaldata$lastinSA[which(survivaldata$Date == survivaldata$End.Date.SA)] = 1
survivaldata$lastinN = numeric(length(survivaldata$Track.Name))
survivaldata$lastinN[which(survivaldata$Date == survivaldata$End.Date.N)] = 1
survivaldata$lastinEU = numeric(length(survivaldata$Track.Name))
survivaldata$lastinEU[which(survivaldata$Date == survivaldata$End.Date.EU)] = 1
survivaldata$lastinME = numeric(length(survivaldata$Track.Name))
survivaldata$lastinME[which(survivaldata$Date == survivaldata$End.Date.ME)] = 1
survivaldata$lastinO = numeric(length(survivaldata$Track.Name))
survivaldata$lastinO[which(survivaldata$Date == survivaldata$End.Date.O)] = 1

survivaldata$LastOnCharts = numeric(length(survivaldata$Track.Name))
survivaldata$LastOnCharts[which(survivaldata$Date == survivaldata$End.Date)] = 1
survivaldata$RCensor = numeric(length(survivaldata$Track.Name))
survivaldata$RCensor[which(survivaldata$End.Date == "2018-01-09")] = 1
survivaldata$RCensor[which(survivaldata$End.Date == Date)] = 2

############## Effective merging Of Origins to Region Data####################################

readrandorigins = read.csv('rndoms.csv')
readorigins = read.csv("originread.csv")
readorigins = select(readorigins, Origin = Region, Artist, Track.Name)
countmerge = select(countryplus, Region.1, Origin = Region)
nonrand = merge(readorigins, countmerge, by="Origin")
readrandmerge = merge(readrandorigins, countmerge, by = "Origin")

andmerge = rbind(readrandmerge, nonrand)  
andmerge = select(andmerge, Track.Name, Origin, Region.1)

###### Survival Section Abandoned ##############
trying = merge(survivaldata, andmerge, by="Track.Name")
tryingcens = filter(trying, Date == End.Date)


###############  Survival Cleaning #########################################
globaltops = filter(invrank, Region == "global")
attach(globaltops)
globalstart = aggregate(globaltops$Date, list(Track.Name), FUN = min)

globalend = aggregate(globaltops$Date, list(Track.Name), FUN = max)

globals = merge(globalstart, globalend, by="Group.1")
globals = select(globals, Track.Name = Group.1, Global.Start = x.x, Global.End = x.y)

globals$GlobalChartDays = as.numeric(as.Date(as.character(globals$Global.End)) - as.Date(as.character(globals$Global.Start)) + 1)


omore = merge(regionstartsfil, andmerge, by="Track.Name")

#omore$GlobalChartDays[is.na(omore$GlobalChartDays)] = 0

################### Set up Maxes #########
grandata = invstart
# Find the dates with the higest amount of international Ranking 
#  and similarly the day with the most number of Streams:
attach(grandata)
firstmax = aggregate(Position, list(Date, Track.Name), FUN = sum)
firstmax = select(firstmax, Date = Group.1, Track.Name = Group.2, RnkSum = x)
firststrmax = aggregate(Streams, list(Date, Track.Name), FUN = sum)
firststrmax = select(firststrmax, Date = Group.1, Track.Name = Group.2, StrSum = x)
attach(firstmax)
maxranks = aggregate(RnkSum, list(Track.Name), FUN = max)
maxranks = select(maxranks, Track.Name = Group.1, RnkSum.max= x)
attach(firststrmax)
maxstrs = aggregate(StrSum, list(Track.Name), FUN = max)
maxstrs = select(maxstrs, Track.Name = Group.1, StrSum.max = x)
maxes = merge(maxranks, maxstrs, by="Track.Name")

first1 = subset(grandata, Date == Start.Date)
attach(first1)
totrnks1 = aggregate(Position, list(Track.Name), FUN = sum) 
totrnks1 = select(totrnks1, Track.Name = Group.1, RnkSum.1 = x)
totstrms1 = aggregate(Streams, list(Track.Name), FUN = sum)
totstrms1 = select(totstrms1,Track.Name = Group.1, StrmSum.1 = x)
set1 = merge(totrnks1, totstrms1, by="Track.Name")

omore = merge(omore, maxes, by="Track.Name")
omore = merge(omore, set1, by="Track.Name")
################### Set up Statuses #########

onemore = filter(omore, Region.1 != "O", Region.1 != "ME")
onemore$Region.1[which(onemore$Region.1 == "LA")] = "SA" 

# Create Statuses for Every Chart Analysis 

datalen = length(onemore$Track.Name)
onemore$status = numeric(datalen)

for (i in 1:datalen){
  if (onemore$End.Date[i] == "2018-01-09"){
    if (onemore$Start.Date[i] == "2017-01-01"){
      onemore$status[i] = 3
    }else{
      onemore$status[i] = 0
    }
  }else if(onemore$Start.Date[i] == "2017-01-01"){
    onemore$status[i] = 2
  }else{
    onemore$status[i] = 1
  }
}

# Create Statuses for Global Chart Analysis 
globmore = merge(onemore, globals, by="Track.Name")
datalen = length(globmore$Track.Name)
globmore$globalstatus = numeric(datalen)
for (i in 1:datalen){
  if (globmore$Global.End[i] == "2018-01-09"){
    if (globmore$Global.Start[i] == "2017-01-01"){
      globmore$globalstatus[i] = 3
    }else{
      globmore$globalstatus[i] = 0
    }
  }else if(globmore$Global.Start[i] == "2017-01-01"){
    globmore$globalstatus[i] = 2
  }else{
    globmore$globalstatus[i] = 1
  }
}



################ Survival Analysis ################################

# Do Analysis on Global Chart Data
globitem = Surv( globmore$GlobalChartDays, globmore$globalstatus)
globkap <- survfit(globitem~Region.1, data = globmore)
ggsurvplot(globkap, data = globmore, 
            legend.title="Region",
           legend.labs=c("Asia", "Europe", "North America", "Latin America"),
           title="Kaplan-Meier Curve for Staying on the Global Chart",
           xlab = "Days from Start")
coxglob1 = coxph(globitem~Region.1, data = globmore)
summary(coxglob1)

coxglob2 = coxph(globitem~Region.1 + log(StrmSum.1), data = globmore)
summary(coxglob2)

# Do Analysis on Every Chart Data
item  = Surv( onemore$FinalTimeonCharts, onemore$status)
sfit <- survfit(item~Region.1, data = onemore)
ggsurvplot(sfit, data = onemore)
ggsurvplot(sfit, conf.int=TRUE, 
           legend.labs=c("Asia", "Europe", "North America", "Latin America"), legend.title="Region",  
           title="Kaplan-Meier Curve for Staying on the Spotify Top Charts",
           xlab = "Days from Start")

coxfit1 = coxph(item~Region.1 , data = onemore)
summary(coxfit1)

coxfit2 = coxph(item~Region.1 + log(StrmSum.1), data = onemore)
summary(coxfit2)



