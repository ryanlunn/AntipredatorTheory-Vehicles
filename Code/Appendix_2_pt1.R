#Libraries Used 
library(ggplot2)
library(car)
library(NLRoot)
#install.packages("NLRoot")
library(rootSolve)
library(psych)

###########################Generating Quantitative Predictions with each Model
#Reading in DeVault et al. 2015 empirical data used to make quantitative predictions
devault_data<-read.csv("DeVaultData.csv", header=TRUE)

#Renaming columns 
colnames(devault_data)<-c("Fate", "AD","FID","Size","Speed", "HQA","TTCA","TTCF")

#Perceptual Limits Hypothesis
#r is the radius of a vehicle width
r<-1.725/2
#a is the inverse of a species spatial resolving power
a<-1/4.82
#dd is the equation to estimate detection distance based on a species visual acuity 
dd<-(r)/(tan(a/2)*(pi/180))
dd

#######################################FEAR Hypothesis 

#Function that calculates phi index values and p-value for the phi-index for the FEAR hypothesis.
#code provided by (Samia et al. 2014)

phi.index<-function(data, rounds){
  
  data = subset(data, data$FID & data$AD !='Na')
  N = nrow(data)
  phi = numeric(0)                               #creates temporary vector to store simulated phi-indices
  progress.bar <- txtProgressBar(min = 0,
                                 max = rounds,
                                 style = 3)      #insert progress bar
  
  #null expectation
  #---------------------------------------------------
  for(i in 1:rounds){
    
    setTxtProgressBar(progress.bar, i)           #start progress bar
    
    sFID = runif(nrow(data), 
                 0, 
                 data$AD)                        #simulates random FIDs
    
    phi[i] = 1 - (sum((data$AD-sFID)/data$AD)/ N)  
  } #close i
  
  close(progress.bar)                            #close progress bar
  
  
  #computes P-value
  #---------------------------------------------------
  obs.phi = 1 - (sum((data$AD-data$FID)/data$AD)/ N)   #extract observed phi
  P  = sum(phi >= obs.phi)/rounds                #calculates the P-value of observed phi
  
  
  #plot
  #---------------------------------------------------
  plot(data$AD,
       data$FID,
       xlab = "AD",
       ylab = "FID",
       las = 1,
       bty = "l",
       ylim=c(0,max(data$AD)),
       xlim=c(0,max(data$AD)),
       t="n")
  abline(0, 1, col=8, lwd=3, lty = "dotted")
  points(data$AD, data$FID, cex = 1.3, pch = 21, bg=16)
  
  
  #output to workspace
  #---------------------------------------------------
  output = list('phi index '= obs.phi,
                'P-value'=P,
                'sample size' = N)
  
  return(output)
  
  
} # close function


#####Estimating phi and its p-value per each AD~FID per a given vehicle approach speed treatment with 

#Number of simulated p-values for the phi index to compare the empirically observed phi against 
rounds<-1000

#Data per each speed treatment 

#60 km/h speed treatment
data_60<-devault_data[1:20,]

#90 km/h speed treatment
data_90<-devault_data[21:40,]

#120 km/h speed treatment
data_120<-devault_data[41:60,]

#150 km/h speed treatment
data_150<-devault_data[61:80,]

#180 km/h speed treatment
data_180<-devault_data[81:100,]

#210 km/h speed treatment
data_210<-devault_data[101:120,]

#240 km/h speed treatment
data_240<-devault_data[121:130,]

#360 km/h speed treatment
data_360<-devault_data[131:140,]


#Linear Regression between AD & FID for each speed treatment with the intercept forced to be 0

#60 km/h speed treatment
model.60<-lm(FID~AD+0, data=data_60)  
summary(model.60)     

#90 km/h speed treatment
model.90<-lm(FID~AD+0, data=data_90)  
summary(model.90)  

#120 km/h speed treatment
model.120<-lm(FID~AD+0, data=data_120)  
summary(model.120)  

#150 km/h speed treatment
model.150<-lm(FID~AD+0, data=data_150)  
summary(model.150)  

#180 km/h speed treatment
model.180<-lm(FID~AD+0, data=data_180)  
summary(model.180)  

#210 km/h speed treatment
model.210<-lm(FID~AD+0, data=data_210)  
summary(model.210)  

#240 km/h speed treatment
model.240<-lm(FID~AD+0, data=data_240)  
summary(model.240)  

#360 km/h speed treatment
model.360<-lm(FID~AD+0, data=data_360)  
summary(model.360)  


#Calculating the phi index value and significance for each speed treatment of DeVault et al. 2015

#60 km/h speed treatment
output.60<-phi.index(data_60,rounds)

#90 km/h speed treatment
output.90<-phi.index(data_90,rounds)

#120 km/h speed treatment
output.120<-phi.index(data_120,rounds)

#150 km/h speed treatment
output.150<-phi.index(data_150,rounds)

#180 km/h speed treatment
output.180<-phi.index(data_180,rounds)

#210 km/h speed treatment
output.210<-phi.index(data_210,rounds)

#240 km/h speed treatment
output.240<-phi.index(data_240,rounds)

#360 km/h speed treatment
output.360<-phi.index(data_360,rounds)


#Assempling the output for the phi index based on DeVault et al. 2015 data in a single data frame

output<-data.frame(rbind(output.60,output.90,output.120,output.150,output.180,output.210,output.240,output.360))
output$Speed<-c(60,90,120,150,180,210,240,360)
output$phi.index.<-as.numeric(output$phi.index.)
output$P.value<-as.numeric(output$P.value)
output$sample.size<-as.numeric(output$sample.size)
output$coef<-as.numeric(c(model.60[1], model.90[1],model.120[1], model.150[1],model.180[1], model.210[1], model.240[1],model.360[1]))

output$phi.index./output$coef


#Plot Phi-Index values for each speed treatment 

phi.graph<-ggplot(data=output, aes(x = Speed, y=phi.index., label= round(phi.index.,2)))+
  geom_text(hjust = -0.25, vjust = 0.75, nudge_x = -0.05, size=4.5)+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=2)+
  labs(y = "Φ Index",x = "Vehicel Speed (km/h)")+ 
  theme_classic(base_size = 16)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+
  ylim(0,1)+geom_hline(yintercept = 0.5,linetype="dashed",size=1)
phi.graph 

#AD effect on FID according to a linear model for each speed treatment
coef.graph<-ggplot(data=output, aes(x = Speed, y=coef, label= round(coef,2)))+
  geom_text(hjust = -0.25, vjust = 0.75, nudge_x = -0.05, size=4)+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=2)+
  labs(y = "coef",x = "Vehicel Speed (km/h)")+ 
  theme_classic(base_size = 16)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+
  ylim(0,1)+geom_hline(yintercept = 0.5,linetype="dashed",size=1)
coef.graph 



###Estimated FID based on the AD where 95% of Prey(Cowbirds) became alert


#Identifying the distance at which 95% of prey had become alert 

#60 km/h speed treatment
AD.60<-quantile(devault_data$AD[1:20],0.05)

#90 km/h speed treatment
AD.90<-quantile(devault_data$AD[21:40],0.05)

#120 km/h speed treatment
AD.120<-quantile(devault_data$AD[41:60],0.05)

#150 km/h speed treatment
AD.150<-quantile(devault_data$AD[61:80],0.05)

#180 km/h speed treatment
AD.180<-quantile(devault_data$AD[81:100],0.05)

#210 km/h speed treatment
AD.210<-quantile(devault_data$AD[101:120],0.05)

#240 km/h speed treatment
AD.240<-quantile(devault_data$AD[121:130],0.05)

#360 km/h speed treatment
AD.360<-quantile(devault_data$AD[131:140],0.05)

#Generating Predicted FIDs for each speed treatment 

#60 km/h speed treatment
FIDpredict.60<-output$coef[1]*AD.60
FIDpredict.60

#90 km/h speed treatment
FIDpredict.90<-output$coef[2]*AD.90
FIDpredict.90

#120 km/h speed treatment
FIDpredict.120<-output$coef[3]*AD.120
FIDpredict.120

#150 km/h speed treatment
FIDpredict.150<-output$coef[4]*AD.150
FIDpredict.150

#180 km/h speed treatment
FIDpredict.180<-output$coef[5]*AD.180
FIDpredict.180

#210 km/h speed treatment
FIDpredict.210<-output$coef[6]*AD.210
FIDpredict.210

#240 km/h speed treatment
FIDpredict.240<-output$coef[7]*AD.240
FIDpredict.240

#360 km/h speed treatment
FIDpredict.360<-output$coef[8]*AD.360
FIDpredict.360


#Assemblying a data frame of the predicted FID at each speed treatment
a<-rbind(60,90,120,150,180,210,240,360)
b<-matrix(rbind(FIDpredict.60,FIDpredict.90,FIDpredict.120,FIDpredict.150,FIDpredict.180,FIDpredict.210,FIDpredict.240,FIDpredict.360))
FearFID<-data.frame(cbind(a,b))
colnames(FearFID)=c("Speed","FID")

#Graphing the predict FID for the FEAR hypothesis for each speed treatment
FearFID.graph<-ggplot(data=NULL, aes(x = FearFID[,1], y=FearFID[,2], label= round(FearFID[,2], 2)))+
  geom_text(hjust =-.1, vjust = .5, nudge_x = 0, size=5)+
  geom_smooth(method="lm")+
  geom_point(size=2)+
  labs(y = "Predicted FID (m)",x = " Vehicle Speed (km/h)")+ 
  theme_classic(base_size = 16)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+
  ylim(0,30)

FearFID.graph

ADFIDslope<-lm(FID~Speed, data = FearFID)
summary(ADFIDslope)

#######################################Looming Stimulus Hypothesis

#Half width of an approaching vehicle 
r<-1.725/2

#Creating a new data frame for the Looming Stimulus Hypothesis
devault_data_l<-devault_data

#Converting km/h into meters per second.
devault_data_l$Speed<-devault_data_l$Speed*0.27778

#Estimating the physiological delay between the onset of neurons firing and when they neurons reach peak firing rate 
#triggering an escape response following procedures in Fotowat, H., & Gabbiani, F. (2011). 
#The average TTC Flight per each speed treatment in DeVault et al.2015
TTCF.60<-mean(devault_data_l$TTCF[1:20])
TTCF.90<-mean(devault_data_l$TTCF[21:30])
TTCF.120<-mean(devault_data_l$TTCF[41:50])
TTCF.150<-mean(devault_data_l$TTCF[61:70])
TTCF.180<-mean(devault_data_l$TTCF[81:90])
TTCF.210<-mean(devault_data_l$TTCF[101:110])
TTCF.240<-mean(devault_data_l$TTCF[121:130])
TTCF.360<-mean(devault_data_l$TTCF[131:140])

### calculating the Ratio of Size to Speed
ratio.60<-r/devault_data_l$Speed[1]
ratio.90<-r/devault_data_l$Speed[21]
ratio.120<-r/devault_data_l$Speed[41]
ratio.150<-r/devault_data_l$Speed[61]
ratio.180<-r/devault_data_l$Speed[81]
ratio.210<-r/devault_data_l$Speed[101]
ratio.240<-r/devault_data_l$Speed[121]
ratio.360<-r/devault_data_l$Speed[131]

#Creating a data frame with the mean TTCFlight (i.e., the amount of seconds remaining prior to collision at which the animal escaped)
#and the ratio of approach speed and size, a proxy for visual angle and the rate of change in the visual angle. 
a<-matrix(rbind(TTCF.60,TTCF.90,TTCF.120,TTCF.150,TTCF.180,TTCF.210,TTCF.240,TTCF.360))
b<-matrix(rbind(ratio.60,ratio.90,ratio.120,ratio.150,ratio.180,ratio.210,ratio.240,ratio.360))

df<-data.frame(cbind(a,b))
colnames(df)<-c("TTCFlight","Ratio")
df

#Graphing the relationship between TTCFlight and the ratio between size and approach speed
looming.graph<-ggplot(data=df, aes(x = Ratio, y= TTCFlight))+
  #geom_text(hjust = 0.05, vjust = -0.5, nudge_x = -0.05, size=4)+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=3)+
  labs(y = "Time to Collision Flight (s)",x = " Ratio of Size to Speed ")+ 
  theme_classic(base_size = 16)+
  ylim(0,2)+
  xlim(0,0.15)
looming.graph

###The intercept estimates the parameter in the looming model for the physiological delay between the onset of neuron firing 
#and when neurons reach their peak firing rate 
loom_model<-lm(TTCFlight~Ratio, data = df)
summary(loom_model)


##########################Critical Angle for 60
#vehicle size(object size)
delay<-loom_model$coefficients[1]

#Putting vehicle speed in m/s into an object

#ie vehicle speed 60km/h
v.60<- devault_data_l$Speed[1]

#ie vehicle speed 90km/h
v.90<- devault_data_l$Speed[21]

#ie vehicle speed 120km/h
v.120<- devault_data_l$Speed[41]

#ie vehicle speed 150km/h
v.150<- devault_data_l$Speed[61]

#ie vehicle speed 180km/h
v.180<- devault_data_l$Speed[81]

#ie vehicle speed 210km/h
v.210<- devault_data_l$Speed[101]

#ie vehicle speed 240km/h
v.240<- devault_data_l$Speed[121]

#ie vehicle speed 360km/h
v.360<- devault_data_l$Speed[131]

###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###60 km/h speed treatment 
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.60<-2*(atan((r/(v.60*TTCF.60)))*(180/pi))
angle.FID.60

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.60<- (1/(((v.60/(r*2))*(TTCF.60^2))+((r*2)/(4*v.60)))) 
angle.expand.60

##### Calculates Tau Ratio
tau.deg.60<-angle.FID.60/angle.expand.60
tau.deg.60

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.60<-(r/(tan((angle.FID.60/2)*(pi/180)))) - (v.60*delay)
FID.60


###90 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.90<-2*(atan((r/(v.90*TTCF.90)))*(180/pi))
angle.FID.90

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.90<- (1/(((v.90/(r*2))*(TTCF.90^2))+((r*2)/(4*v.90)))) 
angle.expand.90

##### Calculates Tau Ratio
tau.deg.90<-angle.FID.90/angle.expand.90
tau.deg.90

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.90<-(r/(tan((angle.FID.90/2)*(pi/180)))) - (v.90*delay)
FID.90



###120 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.120<-2*(atan((r/(v.120*TTCF.120)))*(180/pi))
angle.FID.120

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.120<- (1/(((v.120/(r*2))*(TTCF.120^2))+((r*2)/(4*v.120)))) 
angle.expand.120

##### Calculates Tau Ratio
tau.deg.120<-angle.FID.120/angle.expand.120
tau.deg.120

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.120<-(r/(tan((angle.FID.120/2)*(pi/180)))) - (v.120*delay)
FID.120


###150 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.150<-2*(atan((r/(v.150*TTCF.150)))*(180/pi))
angle.FID.150

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.150<- (1/(((v.150/(r*2))*(TTCF.150^2))+((r*2)/(4*v.150)))) 
angle.expand.150

##### Calculates Tau Ratio
tau.deg.150<-angle.FID.150/angle.expand.150
tau.deg.150

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.150<-(r/(tan((angle.FID.150/2)*(pi/180)))) - (v.150*delay)
FID.150


###180 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.180<-2*(atan((r/(v.180*TTCF.180)))*(180/pi))
angle.FID.180

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.180<- (1/(((v.180/(r*2))*(TTCF.180^2))+((r*2)/(4*v.180)))) 
angle.expand.180

##### Calculates Tau Ratio
tau.deg.180<-angle.FID.180/angle.expand.180
tau.deg.180

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.180<-(r/(tan((angle.FID.180/2)*(pi/180)))) - (v.180*delay)
FID.180


###210 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.210<-2*(atan((r/(v.210*TTCF.210)))*(180/pi))
angle.FID.210

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.210<- (1/(((v.210/(r*2))*(TTCF.210^2))+((r*2)/(4*v.210)))) 
angle.expand.210

##### Calculates Tau Ratio
tau.deg.210<-angle.FID.210/angle.expand.210
tau.deg.210

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.210<-(r/(tan((angle.FID.210/2)*(pi/180)))) - (v.210*delay)
FID.210


###240 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.240<-2*(atan((r/(v.240*TTCF.240)))*(180/pi))
angle.FID.240

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.240<- (1/(((v.240/(r*2))*(TTCF.240^2))+((r*2)/(4*v.240)))) 
angle.expand.240

##### Calculates Tau Ratio
tau.deg.240<-angle.FID.240/angle.expand.240
tau.deg.240

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.240<-(r/(tan((angle.FID.240/2)*(pi/180)))) - (v.240*delay)
FID.240

###360 km/h speed treatment 
###Trigonometry in R is in radians, (180/pi) converts radians -> degrees
###Calculates the visual angle of the approaching vehicle at TTC Flight
angle.FID.360<-2*(atan((r/(v.360*TTCF.360)))*(180/pi))
angle.FID.360

###Calculates the rate at which the visual angle expands for the approaching vehicle at TTC Flight
angle.expand.360<- (1/(((v.360/(r*2))*(TTCF.360^2))+((r*2)/(4*v.360)))) 
angle.expand.360

##### Calculates Tau Ratio
tau.deg.360<-angle.FID.360/angle.expand.360
tau.deg.360

#Predicted FID according to the hypothesis
#The visual angle essentially predicts the FID based on TTC and then accounts for the neuronal latency
FID.360<-(r/(tan((angle.FID.360/2)*(pi/180)))) - (v.360*delay)
FID.360

####Putting The outputs into a single data frame
a<-matrix(rbind(TTCF.60,TTCF.90,TTCF.120,TTCF.150,TTCF.180,TTCF.210,TTCF.240,TTCF.390))
b<-matrix(rbind(FID.60,FID.90,FID.120,FID.150,FID.180,FID.210,FID.240,FID.360))
c<-matrix(rbind(tau.deg.60,tau.deg.90,tau.deg.120,tau.deg.150,tau.deg.180,tau.deg.210,tau.deg.240,tau.deg.360))
d<-matrix(rbind(angle.FID.60,angle.FID.90,angle.FID.120,angle.FID.150,angle.FID.180,angle.FID.210,angle.FID.240,angle.FID.360))
e<-matrix(rbind(angle.expand.60,angle.expand.90,angle.expand.120,angle.expand.150,angle.expand.180,angle.expand.210,angle.expand.240,angle.expand.360))
f<-matrix(rbind(60,90,120,150,180,210,240,360))

df.1<-data.frame(cbind(a,b,c,d,e,f))
colnames(df.1)<-c("TTCF","FID","Tau","FID.Angle","Expansion.Angle","Speed")

mean(df.1$FID)

####How the predicted FID Changes with approach Speed
FID.Speed<-ggplot(data=df.1, aes(x = Speed, y=FID, label=round(FID,2)))+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=3)+
  labs(y = "Predicted FID (m)",x = "Vehicle Speed (km/h)")+ 
  geom_text(hjust = 0.4, vjust = -1.25, nudge_x = -0.05, size=4)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+theme_classic(base_size = 16) +ylim(0,45)

FID.Speed
mean(df.1$FID)

#Model Predicted FID with Approach Speed, Approach Speed was run as a quantitative variable in the model
loom_model<-lm(FID~Speed, data = df.1)
summary(loom_model)



############################Visual Cue Model

#The mean alert distance per each speed treatment
AD.60<-mean(devault_data$AD[1:20])
AD.90<-mean(devault_data$AD[21:40])
AD.120<-mean(devault_data$AD[41:60])
AD.150<-mean(devault_data$AD[61:80])
AD.180<-mean(devault_data$AD[81:100])
AD.210<-mean(devault_data$AD[101:120])
AD.240<-mean(devault_data$AD[121:130])
AD.360<-mean(devault_data$AD[131:140])

#The mean flight initiation distance per each speed treatment
FID.60<-round(mean(devault_data$FID[1:20]),0)
FID.90<-round(mean(devault_data$FID[21:40]),0)
FID.120<-round(mean(devault_data$FID[41:60]),0)
FID.150<-round(mean(devault_data$FID[61:80]),0)
FID.180<-round(mean(devault_data$FID[81:100]),0)
FID.210<-round(mean(devault_data$FID[101:120]),0)
FID.240<-round(mean(devault_data$FID[121:130]),0)
FID.360<-round(mean(devault_data$FID[131:140]),0)

#e<-data.frame(AD.60,AD.90,AD.120,AD.150,AD.180,AD.210,AD.240,AD.360)
#e<-data.frame(stack(e))

#Parameters 
#Profile Size, Diameter of the Vehicle
A<-(1.725)*pi
#Approach Angle
a<-0.00
#Vegtation Parameter
c<-0
#Maximum Possible Detection distance
detection<-dd
#Change in that distance as the vehicle moves closer
delta<-seq(474,1,by=-1)

####Estimated change in perceived vehicle size when viewed at two distances

#The first perceived visual cue based on profile size
view_1<-A/(detection^2)

#The second perceived visual cue based on profile size
view_2<-A/((detection-delta)^2)

#The change in profile size at every distance from the approaching vehicle 
delta_A<-(view_1-view_2)
#plot(deltaA)

#Estimation of the threshold change in perceived profile size which triggers an escape response
#based on the mean FID for each speed treatment 
threshold_delta_A.60<-delta_A[FID.60]

threshold_delta_A.90<-delta_A[FID.90]
threshold_delta_A.120<-delta_A[FID.120]
threshold_delta_A.150<-delta_A[FID.150]
threshold_delta_A.180<-delta_A[FID.180]
threshold_delta_A.210<-delta_A[FID.210]
threshold_delta_A.240<-delta_A[FID.240]
threshold_delta_A.360<-delta_A[FID.360]


#The function to solve for the predicted FID
#AD, the second distance at which prey receive a visual cue, 
bisection<-function(f, a, b, num = 10, eps = 1e-05) 
{
  h = abs(b - a)/num
  i = 0
  j = 0
  a1 = b1 = 0
  while (i <= num) {
    a1 = a + i * h
    b1 = a1 + h
    if (f(a1) == 0) {
      #print(a1)
      #print(f(a1))
    }
    else if (f(b1) == 0) {
      print(b1)
      print(f(b1))
    }
    else if (f(a1) * f(b1) < 0) {
      repeat {
        if (abs(b1 - a1) < eps) 
          break
        x <- (a1 + b1)/2
        if (f(a1) * f(x) < 0) 
          b1 <- x
        else a1 <- x
      }
      #print(j + 1)
      j = j + 1
      #print((a1 + b1)/2)
      #print(f((a1 + b1)/2))
      return(a1)
    }
    i = i + 1
  }
}

###########Predicted FID Visual Cue Model

#60 km/h speed treatment
vc.60<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.60, delta_A=threshold_delta_A.60, A=A)

FID_60_vc<-bisection(vc.60,0,AD.60)


#90 km/h speed treatment
vc.90<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.90, delta_A=threshold_delta_A.90, A=A)

FID_90_vc<-bisection(vc.90,0,AD.90)

#120 km/h speed treatment
vc.120<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.120, delta_A=threshold_delta_A.120, A=A)

FID_120_vc<-bisection(vc.120,0,AD.120)

#150 km/h speed treatment
vc.150<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.150, delta_A=threshold_delta_A.150, A=A)

FID_150_vc<-bisection(vc.150,0,AD.150)

#180 km/h speed treatment
vc.180<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.180, delta_A=threshold_delta_A.180, A=A)

FID_180_vc<-bisection(vc.180,0,AD.180)

#210 km/h speed treatment
vc.210<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.210, delta_A=threshold_delta_A.210, A=A)

FID_210_vc<-bisection(vc.210,0,AD.210)

#240 km/h speed treatment
vc.240<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.240, delta_A=threshold_delta_A.240, A=A)

FID_240_vc<-bisection(vc.240,0,AD.240)

#360 km/h speed treatment
vc.360<-function(FID_vc) {
  with (as.list(params),{
    df.FID<-(1-c)*((1/((FID_vc-AD*cos(a))^2)+(AD^2*sin(a)^2))-(1/(FID_vc^2)))-delta_A/A
    
    return(c(df.FID=df.FID))
  })}
params<-c(c=0, a=0, AD=AD.360, delta_A=threshold_delta_A.360, A=A)

FID_360_vc<-bisection(vc.360,0,AD.360)

#Creating a data frame of the predicted FID for the visual cue model
a<-matrix(rbind(FID_60_vc,FID_90_vc,FID_120_vc,FID_150_vc,FID_180_vc,FID_210_vc,FID_240_vc,FID_360_vc))
b<-matrix(rbind(60,90,120,150,180,210,240,360))

df_vc<-data.frame(cbind(a,b))
colnames(df_vc)<-c("FID","Speed")

mean(df_vc$FID)

#Ploting the FID predicted and approach speed for the visualcue model
FID.plot<-ggplot(data=df_vc, aes(x = Speed, y= FID, label=round(FID,2)))+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=3)+
  labs(y = "Predicted FID (m)",x = "Vehicle Speed (km/h)")+ 
  geom_text(hjust = -0.4, vjust = 0.15, nudge_x = -0.05, size=4)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+theme_classic(base_size = 16) +ylim(0,45)
FID.plot


############################Bayesian optimal escape model


rm(list = ls ())
#
devault_data<-read.csv("DeVaultData.csv", header=TRUE)

#Creating a new data frame for the Bayesian optimal escape model
devault_data_b<-devault_data

#Converting km/h into meters per second.
devault_data_b$Speed<-devault_data_b$Speed*0.27778

#Mean alert distance for each speed speed treatment form DeVault et al.2015
AD.60<-mean(devault_data$AD[1:20])
AD.90<-mean(devault_data$AD[21:40])
AD.120<-mean(devault_data$AD[41:60])
AD.150<-mean(devault_data$AD[61:80])
AD.180<-mean(devault_data$AD[81:100])
AD.210<-mean(devault_data$AD[101:120])
AD.240<-mean(devault_data$AD[121:130])
AD.360<-mean(devault_data$AD[131:140])

#Function for predicting FID for the Bayesian optimal escape model from Sutton & O'Dwyer 2018


bayesian<-function(AD,dr,g){
  M<-exp(g)
  d<-seq(0,AD, by=1)
  E<- -.797+(0.659*(log(M)))
  watts<-61.718*((M/1000)^0.7902)
  B<-watts*1.5/1000
  m<-15.9*((M/1000)^0.13)
  h=0.5
  
  rfs<-function(AD,dr,m){
    rf1<-ifelse(1-d/AD<=1,(1-(d/AD)),0)
    rf2<-ifelse(dr/m<=1,(dr/m),1)
    rf1*rf2
  }
  
  Energy<-function(E,h,AD,dr,m){((E*((rfs(AD,dr,m)*h)/((rfs(AD,dr,m)*h)+
                                                         ((1-(rfs(AD,dr,m)))*(1-h))))))}
  
  risk<-data.frame(Energy(E,h,AD,dr,m))
  risk$dist<-d
  colnames(risk)<-c("DEE","distance") 
  risk
  
  a<-risk$distance[which.max(risk$DEE<B)]
  
  output<-ifelse(a==0,
                 ifelse(max(risk$DEE)<B,1,
                        ifelse( min(risk$DEE)>=B,AD,
                        )),a)
  
  output
}

#60 km/h speed treatment
output.60<-bayesian(AD.60,devault_data_b$Speed[1],log(43.9))

#90 km/h speed treatment
output.90<-bayesian(AD.90,devault_data_b$Speed[21],log(43.9))

#120 km/h speed treatment
output.120<-bayesian(AD.120,devault_data_b$Speed[41],log(43.9))

#150 km/h speed treatment
output.150<-bayesian(AD.150,devault_data_b$Speed[61],log(43.9))

#180 km/h peed treatment
output.180<-bayesian(AD.180,devault_data_b$Speed[81],log(43.9))

#210 km/h peed treatment
output.210<-bayesian(AD.210,devault_data_b$Speed[101],log(43.9))

#240 km/h peed treatment
output.240<-bayesian(AD.240,devault_data_b$Speed[121],log(43.9))

#360 km/h peed treatment
output.360<-bayesian(AD.240,devault_data_b$Speed[131],log(43.9))


#Creating a data frame of the predicted FID for the Bayesian optimal escape model 
output<-data.frame(rbind(output.60,output.90,output.120,output.150,output.180,output.210,output.240,output.360))
output$Speed<-c(60,90,120,150,180,210,240,360)
colnames(output)<-c("FID","Speed")
output

#Plotting the relationship between predicted FID and approach speed
graph<-ggplot(data=output, aes(x = Speed, y=FID, label= round(FID,2)))+
  geom_text(hjust = -0.25, vjust = 0.1, nudge_x = -0.05, size=5)+
  geom_smooth(method="lm", formula= y~x)+ geom_point(size=2)+
  labs(y = "Predicted FID (m)",x = "Vehicel Speed (km/h)")+ 
  theme_classic(base_size = 16)+
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350))+
  ylim(0,55)
graph 


