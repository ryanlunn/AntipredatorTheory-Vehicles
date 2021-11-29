############################Simulation 
rm(list = ls ())


#Packages
library(dplyr)
library(broom)
library(lme4)
library(ggplot2)
library(plotly)
library(plyr)
library(scatterplot3d)

getwd()
setwd("/Users/Ryan/Desktop/Review_MS")
###############Review 
#Evaluating the FID and speed relationship for different species based on empirical data from the literature

data<- read.csv("FID_Speed_Review.csv",na.strings = c("","NA"), header=T)

df<-data.frame(data$Species,data$n,data$Speeds,data$FID,data$Stimulus.Type)
colnames(df)<-c("Species","n","Speed","FID","stimulus")

#Estimating the slope and intercept for the FID and approach speed relationship for each species 
models <- dlply(df, "Species", function(df) 
  lm(FID ~ Speed, data = df))

#Creating a data frame with the slope and intercept for the FID and approach speed relationship for each species
df_slope<-data.frame(ldply(models, coef))
df_slope

#calculating the average FID for each species
a<-data.frame(aggregate(df$FID,list(Species=df$Species), mean))

#adding the mean FID to data frame of slope and intercept 
df_slope$FID<-a$x

#adding mass values for each species to the data frame
mass_df<- read.csv("species_mass.csv",na.strings = c("","NA"), header=T)
df_slope$mass<-mass_df$mass

#Estimating Alert distances based on body mass
AD_func<-function(b){
  (10^((0.347*log10(b))))+mean(df_slope$X.Intercept.)
}

min(AD_sim_out[[17]])


#Adding alert distances to the data frame 
df_slope$AD<-as.numeric(AD_func(df_slope$mass))

#Inserting empirically observed alert distances 
df_slope$AD<-ifelse(df_slope$Species=="Brown-headed Cowbird",44.65,
                    ifelse(df_slope$Species=="House Sparrow",12.5,
                           ifelse(df_slope$Species=="European Goldfinch",13.7,
                                  ifelse(df_slope$Species=="Hadeda ibis",9.9,df_slope$AD))))


#Plotting the histogram for slope of FID and approach speed for each species
ggplot(data=df_slope, aes(x=Speed))+
  geom_histogram(binwidth=10,fill="white",color="black")+
  xlab("Slope")+ylab("Count")+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,30))+
  ggtitle("   Observed Slope of FID & approach speed for 50 Species")+
  theme_classic(base_size = 14)

#Plotting the histogram for Intercept of FID and approach speed for each species
ggplot(data=df_slope, aes(x=X.Intercept.))+
  geom_histogram(binwidth=25,fill="white",color="black")+
  xlab("Intercept")+ylab("Count")+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,20))+
  ggtitle("Observed Intercept of FID & approach speed for 50 Species")+
  theme_classic(base_size = 14)

#Plotting the histogram for mean FID for each species
ggplot(data=df_slope, aes(x=FID))+
  geom_histogram(binwidth=10,fill="white",color="black")+
  xlab("Flight Initiation Distance (m)")+ylab("Count")+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,15))+
  ggtitle("        Observed Flight Initiation Distance for 50 Species")+
  theme_classic(base_size = 14)

#Plotting the histogram for mean AD for each species
ggplot(data=df_slope, aes(x=AD))+
  geom_histogram(binwidth=5,fill="white",color="black")+
  xlab("Alert Distance (m)")+ylab("Count")+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,25))+
  ggtitle("         Estimated & Observed Alert Distance for 50 Species")+
  theme_classic(base_size = 14)


#Renaming columns 
colnames(df_slope)<-c("Species","Intercept","Slope","FID","mass","AD")

#Removing species without mass values
df_slope<-na.omit(df_slope)


###############Simulation

#Parameters

#range of slopes used
slope<-seq(-37,32,by=1)

#range of approach speeds in m/s
x<-seq(1,100,by=1)

#The range of different neuronal latency values used 
delay<-seq(.050,.100,length.out = 25)

#Establishing range of body mass values in even intervals along a log scale 
mass_df<-df_slope[order(df_slope$mass),]
mass_df_log<-log(mass_df$mass)
mass_df_log<-data.frame(mass_df_log)

df<-matrix(data=0, nrow=50,ncol=1)
df

for(i in 2:50){
  df[i]<- mass_df_log[i,] - mass_df_log[i-1,]
}
df
#Interval of log transformed body mass
diff<-mean(df[2:50,])

mass<-seq(min(mass_df_log),max(mass_df_log),by=diff)


#Functions

#AD Function
AD_function<-function(a,sa) {
  #a is the FID fed to generate the AD, a is essentially the minimum value AD can be
  b<-round(rnorm(1,mean=a,sd=sa))
  AD<-ifelse(b<=0,0,b)
  AD
} 

AD_function(mean(df_slope$AD),sd(df_slope$AD))

#ad<-AD_function(mean(df_slope$AD),sd(df_slope$AD))
#ad
###FID function
FID_function<-function(AD,x,m,b,s) {
  #x is speed m/s
  #m is slope
  #b is the intercept
  #s is the variation in FID
  #The equation for a line + or - some random integer
  #The random integer is being pulled from a uniform distribution 
  a<-((rnorm(1,mean=m*x,sd=s)))+b
  ####Limiting extremely large FID values 542, is the maximum distance a cowbird could detect
  #### an approaching vehicle.0 because you can not have a negative FID
  FID<-ifelse(a>=AD,AD,ifelse(a<=0,0,a))
  FID<-round(FID)
  FID
} 


#Function used to simulate pairs of AD and FID 

sim_function_FID<-function(b,s){
  #x looping through all approach speeds values  
  x<-seq(1,100,by=1)
  #m looping through all the different range of slopes 
  m<-seq(-37,32,by=1)
  #Making an empty matrix to store AD values
  AD_results <- matrix(nrow= length(x) , ncol= length(m))
  
  
  #Simulating our AD's feeding. I am feeding our AD function our simulated FID values
  for(i in 1:length(x)){
    for (j in 1:length(m)){
      AD_results[i,j] <- AD_function(mean(df_slope$AD),sd(df_slope$AD))
    }
  }  
  
  #Making an empty matrix to store FID values
  FID_results <- matrix(nrow= length(x) , ncol= length(m))
  #Simulating our AD's feeding. I am feeding our AD function our simulated FID values
  for(i in 1:length(x)){
    for (j in 1:length(m)){
      FID_results[i,j] <- FID_function(AD_results[i,j],x[i],m[j],mean(df_slope$Intercept),sd(df_slope$FID))
      #FID_function(results[i,j],x[i],t)
    }
  }  
  
  
  
  
  output = list('AD'=AD_results,'FID'=FID_results)
}

#Repeat the Simulate 7,000 pairs of AD and FID values for 100 iterations 
FID_sim_1<-replicate(100,sim_function_FID(mean(df_slope$Intercept),sd(df_slope$FID)),simplify = F)

#Parsing out the FID values
FID_sim_out<-rep(list(matrix(NA, nrow=100,ncol=70)), 100)

for(i in 1:100) { 
  FID_sim_out[[i]]<-FID_sim_1[[i]]$FID   # Printing some output
}

#Parsing out the AD values 
AD_sim_out<-rep(list(matrix(NA, nrow=100,ncol=70)), 100)
for(i in 1:100) { 
  AD_sim_out[[i]]<-FID_sim_1[[i]]$AD   # Printing some output
}



#Function used to estimate f^2 values
f_func<-function(x){
  f<-(x/(1-x))
}

###FEAR Hypothesis
#Evaluating simulated AD and FID with the FEAR Hypothesis

#Function used to estimate the phi-index value and significance according to the FEAR hypothesis from Samia & Blumstein 2014

FEAR<-function(AD,FID,N,S){
  
  phi = numeric(0) 
  
  for(i in 1:S){
    sFID = runif(N,0,AD)                        #simulates random FIDs
    phi[i] = 1 - (sum((AD-sFID)/AD)/N)}
  
  obs.phi= 1 - (sum((AD-FID)/AD)/ N)   #extract observed phi
  P  = sum(phi >= obs.phi)/(S)               #calculates the P-value of observed phi
  
  FID_p<-as.numeric(obs.phi*quantile(AD, c(0.05))) #generates predicted FID
  
  output = list('phi index '= obs.phi,'P-value'=P,'Predicted_FID_95' = FID_p)
  #FID_p
  output
}

#Creating the matrix to store the results of the FEAR hypothesis for each iteration
results_f <- rep(list(matrix(NA, nrow=length(slope),ncol=4)), 100)



AD_df<-data.frame(do.call(rbind,AD_sim_out))
write.csv(AD_df,"Alert_Distance_Simulated_Data.csv")

FID_df<-data.frame(do.call(rbind,FID_sim_out))
write.csv(FID_df,"Flight_Initiation_Distance_Simulated_Data.csv")

#Evaluating each iteration of the simulation according ot the FEAR hypothesis and parsing the results into a matrix
for (k in 1:100){
  for (i in 1:length(slope)){
    #Predicted FID
    results_f[[k]][i,1] <- round(as.numeric(FEAR(AD_sim_out[[k]][,i],FID_sim_out[[k]][,i],length(x),100)[3]))
    #P-value according to the Phi index
    results_f[[k]][i,2] <- as.numeric(FEAR(AD_sim_out[[k]][,i],FID_sim_out[[k]][,i],length(x),100)[2])
    #Phi index  value
    results_f[[k]][i,3] <- as.numeric(FEAR(AD_sim_out[[k]][,i],FID_sim_out[[k]][,i],length(x),100)[1])
    results_f[[k]][i,4]<-slope[i]
    colnames(results_f[[k]])<-c("Predicted FID","P-value","phi","Slope")
    
  }
}
results_f[1]

#converting the results store in a matrix into a data frame
fear_df<-data.frame(do.call(rbind,results_f))

head(fear_df)
nrow(fear_df)
#writing out the results for the FEAR hypothesis for every simulation
write.csv(fear_df, file="Fear_Results.csv")

#reading in the results of every simulation back in
#fear_df<-read.csv("fear_model.csv")

#creating the matrix to store the average results of the FEAR hypothesis for each slope
eval_f<-matrix(NA, nrow=70,ncol=3)
eval_f

fear_df

#Function used to estimate the mean phi value and variation in phi value for each slope
eval_function_f<-function(df,y){
  
  a<-ifelse(df[,4]==y,df[,3],NA)
  #a<-ifelse(df[,4]==y,df[,1],NA)
  
  eval_df<-data.frame(a)
  eval_df<-na.omit(eval_df)
  
  colnames(eval_df)<-c("phi")
  
  output = list("mean_phi"=mean(eval_df$phi), "phi_SD"=sd(eval_df$phi), y)
  
  output
}

#eval_function_f(fear_df,slope[1])

#Calculating the mean phi value and standard deviation for each slope and parsing the results into a matrix
for (i in 1:length(slope)){ 
  #mean phifor each slope
  eval_f[i,1]<-as.numeric(eval_function_f(fear_df,slope[i])[[1]])
  #variation in phi for each slope
  eval_f[i,2]<-as.numeric(eval_function_f(fear_df,slope[i])[[2]])
  #slope
  eval_f[i,3]<-as.numeric(eval_function_f(fear_df,slope[i])[[3]])
}

#Renaming the column names of the matrix
colnames(eval_f)<-c("mean_phi","sd_phi","slope")

#Reading out the results for each slope 
write.csv(eval_f, file="FEAR_Results_Summary.csv")


eval_f<-data.frame(eval_f)

#Plotting the mean phi value for each slope 
ggplot(data=eval_f,aes(x=slope, y=mean_phi)) +
  geom_point(size=2)+
  geom_line(lwd=2)+
  xlab("Slope") +
  ggtitle("                                FEAR Hypothesis")+
  ylab("Phi Index")+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,1.1))+
  theme_classic(base_size = 16)

#Mean Mean Phi and SD value for negative slopes
mean(eval_f[1:37,1])
sd(eval_f[1:37,1])

#Mean Mean Phi and SD value for positive slopes
mean(eval_f[39:70,1])
sd(eval_f[39:70,1])

###Looming stimulus hypothesis

#Function used to generate predicted FID according to the looming stimulus hypothesis

Looming<-function(FID,AD,delay,Speed){
  #width of vehicle
  r=1.725
  #Estimating the TTC Flight
  TTCF<-(FID/Speed)
  #Generating predicted FID according to the Looming stimulus hypothesis
  FID_p<-ifelse((FID-(Speed*delay))<=0,0,
                ifelse(FID-(Speed*delay)>=AD,AD,
                       round(FID-(Speed*delay),2)))
  
  #Calculating the visual angle of the approaching vehicle when the animal escapes
  angle.FID<-2*(atan((r/(FID_p)))*(180/pi))
  
  #Calculates the rate at which the visual angle expands for the approaching vehicle when the animal escapes
  angle.expand<- (1/(((Speed/(r*2))*(TTCF^2))+((r*2)/(4*Speed)))) 
  #angle.expand
  
  #Estimates the Tau Ratio when the animal escapes
  tau.deg<-angle.FID/angle.expand
  
  #Output is the predicted FID 
  FID_p
  
}



#Creating the matrix to store the predicted FID values for the looming stimulus hypothesis
results_l<-rep(list(rep(list(matrix(NA, nrow=100,ncol=70)), 25)),100)

#Using the simulated FID and AD pairs to generate predicted FID for the looming stimulus hypothesis and parsing the results 
#for each simulation into a matrix
for(k in 1:100){
  for(i in 1:length(x)){
    for (j in 1:length(slope)){
      for (l in 1:length(delay)){
        results_l[[k]][[l]][i,j] <- Looming(FID_sim_out[[k]][i,j],AD_sim_out[[k]][i,j],delay[l],x[i])
      }
    }
  }  
}




#Turning the matrix into a data frame 
list <- unlist(results_l, recursive = FALSE)
loom_df <- do.call("rbind",list)
nrow(loom_df)
2+2
results_l_df<-data.frame(do.call(rbind,results_l))
results_l[[1]]

head(results_l_df)

#Reading out the model predicted FID for the looming stimulus hypothesis
write.csv(loom_df, file="Looming_FID_Data.csv")


#Creating a matrix to store the the evaluation results for the looming stimulus hypothesis
effect_l_size<-rep(list(rep(list(matrix(NA, nrow=70,ncol=5)),25)),100)

##Evaluating the model predicted FID for the looming stimulus hypothesis and parsing the results in a matrix

for(k in 1:100){
  for (l in 1:length(delay)){
    for (j in 1:length(slope)){
      effect_l_size[[k]][[l]][j,1]<-summary(lm(results_l[[k]][[l]][,j]~x))$adj.r.squared
      effect_l_size[[k]][[l]][j,2]<-summary(lm(results_l[[k]][[l]][,j]~x))$r.squared
      effect_l_size[[k]][[l]][,3]<-delay[l]
      effect_l_size[[k]][[l]][,4]<-slope
      effect_l_size[[k]][[l]][,5]<-k
      colnames(effect_l_size[[k]][[l]])<-c("Adj_r_squared","r_squared","delay","slope","simulation")
    }
  }
}

#Converting matrix into data frame
list <- unlist(effect_l_size, recursive = FALSE)
loom_df <- do.call("rbind",list)
head(loom_df)
tail(loom_df)
write.csv(loom_df, file="Looming_Results.csv")

#Function used to estimate the mean R_squared between FID and approach for each slope in the Looming Stimulus hypothesis
eval_function_l<-function(df,y,x){
  
  a<-ifelse(df[,4]==y,
            ifelse(df[,3]==x,df[,1],NA),NA)
  b<-ifelse(df[,4]==y,
            ifelse(df[,3]==x,df[,2],NA),NA)
  
  eval_df<-data.frame(cbind(a,b))
  eval_df<-na.omit(eval_df)
  
  colnames(eval_df)<-c("Adj_R_Squared","R_squared")
  
  output = list('Adj_R_squared'= mean(eval_df$Adj_R_Squared),'Adj_R_Squared_SD'= sd(eval_df$Adj_R_Squared),
                'R_Squared'=mean(eval_df$R_squared),'R_squared_SD'= sd(eval_df$R_squared),
                y,x)
  
  #output = list(mean(eval_df[,2]),sd(eval_df[,2]),mean(eval_df[,3]),
  #    sd(eval_df[,3]),f,x)
  
  output
}


#Creating the matrix to store the R_squared results for each slope for the Looming Stimulus Hypothesis
eval_l<-rep(list(matrix(NA, nrow=70,ncol=6)),25)


#Calculating the mean R_squared value and standard deviation in R_squared for each slope and parsing the results into a matrix
for(j in 1:length(delay)){
  for (i in 1:length(slope)){ 
    #Adj.R_squared
    eval_l[[j]][i,1]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[1]])
    eval_l[[j]][i,2]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[2]])
    #R_Squared
    eval_l[[j]][i,3]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[3]])
    eval_l[[j]][i,4]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[4]])
    #Slope
    eval_l[[j]][i,5]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[5]])
    #Delay
    eval_l[[j]][i,6]<-as.numeric(eval_function_l(loom_df,slope[i],delay[j])[[6]])
    
  }
}

#Converting the matrix into a data.frame
eval_l_df<-data.frame(do.call(rbind,eval_l))

#Renaming the columns of the matrix
colnames(eval_l_df)<-c('Adj_R_squared','Adj_R_squared_SD',
                       'R_squared','R_squared_SD',
                       'Slope','Delay')
write.csv(eval_l_df, file="Looming_Results_Summary.csv")
#######Plotting the results of the looming stimulus hypothesis

eval_l_df_2D<-eval_l_df
#2D plotting of looming stimulus hypothesis
#Only including a single neuronal latency value, 0.075 sec

eval_l_df_2D$Delay<-round(eval_l_df$Delay,3)

eval_l_df_2D$Slope<-(ifelse(eval_l_df_2D$Delay==0.075,eval_l_df_2D$Slope,NA))
eval_l_df_2D<-na.omit(eval_l_df_2D)

#Estimating f_squared from Cohen 1988 from R_squared
f<-lapply(eval_l_df_2D$R_squared, f_func)
f_squared<-do.call(rbind,f)
#appending f_squared to data frame
eval_l_df_2D$f_squared<-f_squared

#2D graph
ggplot(data=eval_l_df_2D,aes(x=Slope, y=f_squared)) +
  geom_point(size=2)+
  geom_line(lwd=2)+
  xlab("Slope") +
  ggtitle("                                   Looming Stimulus Hypothesis")+
  ylab(expression(italic(f^{2})))+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,.65))+
  theme_classic(base_size = 16)

#Mean Mean f^2 and SD value for negative slopes
mean(eval_l_df_2D[1:37,7])
sd(eval_l_df_2D[1:37,7])

#Mean Mean Phi and SD value for positive slopes
mean(eval_l_df_2D[39:70,7])
sd(eval_l_df_2D[39:70,7])



#Generating a 3D graph between delay (s), slope, and f^2 for the looming stimulus hypothesis
#putting f^2 into a matrix 
f_Squared<-matrix(eval_l_df$R_squared,nrow = 70, ncol=25)
f_Squared

#Crating axis titles
axx <- list(
  title = "Neuronal Latency (s)"
)

axy <- list(
  title = "Slope"
)

axz <- list(
  title = "f squared"
)
delay

#Plotting the three different variables
surf_l<-plot_ly(x=~delay,y=~slope, z = ~f_Squared, type = "surface", showlegend=T)
surf_l <- surf_l %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
surf_l <- surf_l %>%layout(radialaxis = list(ticksuffix = "%"), orientation = 270)
surf_l

###Bayesian optimal escape model

#Function used to generate predicted FID according to the Bayesian optimal escape model
bayesian<-function(AD,g,dr){
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
  
  output<-ifelse(AD>0,
                 (ifelse(a==0,
                         ifelse(max(risk$DEE)<B,1,
                                ifelse(min(risk$DEE)>=B,AD,
                                )),a)),0)
  output
}
#bayesian(0,5,50)


#x
#mass
#slope
#Creating the matrix to store the predicted FID values for the Bayesian optimal escape model
results_b<-rep(list(rep(list(matrix(NA, nrow=100,ncol=70)), 50)),100)

#results_b[17]

#AD_sim_out[[1]][[]]
#Using the simulated AD to generate predicted FID for the Bayesian optimal escape model and parsing the results 
#for each simulation into a matrix
for(k in 1:100){
  for(i in 1:length(x)){
    for (j in 1:length(slope)){
      for (l in 1:length(mass)){
        results_b[[k]][[l]][i,j] <- bayesian(AD_sim_out[[k]][i,j],mass[l],x[i])
      }
    }
  }  
}


list <- unlist(results_b, recursive = FALSE)
bayes_df <- do.call("rbind",list)
head(bayes_df)

write.csv(bayes_df, file="Bayesian_FID_Data.csv")

#Creating a matrix to store the the evaluation results for the Bayesian optimal escape model
effect_b_size<-rep(list(rep(list(matrix(NA, nrow=70,ncol=5)),50)),100)

#effect_b_size[[17]]
#Evaluating the model predicted FID for the Bayesian optimal escape model and parsing the results in a matrix

for(k in 1:100){
  for (l in 1:length(mass)){
    for (j in 1:length(slope)){
      effect_b_size[[k]][[l]][j,1]<-summary(lm(results_b[[k]][[l]][,j]~x))$adj.r.squared
      effect_b_size[[k]][[l]][j,2]<-summary(lm(results_b[[k]][[l]][,j]~x))$r.squared
      effect_b_size[[k]][[l]][,3]<-mass[l]
      effect_b_size[[k]][[l]][j,4]<-slope[j]
      effect_b_size[[k]][[l]][,5]<-k
      colnames(effect_b_size[[k]][[l]])<-c("Adj_r_squared","r_squared","mass","slope","simulation")
    }
  }
}


#Converting matrix into data frame
list <- unlist(effect_b_size, recursive = FALSE)
bayesian_df <- do.call("rbind",list)
head(bayesian_df)


write.csv(bayesian_df, file="Bayesian_Results.csv")

#Function used to estimate the mean R_squared between FID and approach for each slope in the Bayesian optimal escape model
eval_function_b<-function(df,y,x){
  
  a<-ifelse(df[,4]==y,
            ifelse(df[,3]==x,df[,1],NA),NA)
  b<-ifelse(df[,4]==y,
            ifelse(df[,3]==x,df[,2],NA),NA)
  
  eval_df<-data.frame(cbind(a,b))
  eval_df<-na.omit(eval_df)
  
  colnames(eval_df)<-c("Adj_R_Squared","R_squared")
  
  output = list('Adj_R_squared'= mean(eval_df$Adj_R_Squared),'Adj_R_Squared_SD'= sd(eval_df$Adj_R_Squared),
                'R_Squared'=mean(eval_df$R_squared),'R_squared_SD'= sd(eval_df$R_squared),
                x,y)
  
  #output = list(mean(eval_df[,2]),sd(eval_df[,2]),mean(eval_df[,3]),
  #    sd(eval_df[,3]),f,x)
  
  output
}


#Creating the matrix to store the R_squared results for each slope for the Bayesian optimal escape model
eval_b<-rep(list(matrix(NA, nrow=70,ncol=6)),50)



#Calculating the mean R_squared value and standard deviation in R_squared for each slope and parsing the results into a matrix

for(j in 1:length(mass)){
  for (i in 1:length(slope)){ 
    #Adj.R_squared
    eval_b[[j]][i,1]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[1]])
    eval_b[[j]][i,2]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[2]])
    #R_Squared
    eval_b[[j]][i,3]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[3]])
    eval_b[[j]][i,4]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[4]])
    #Mass
    eval_b[[j]][i,5]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[5]])
    #Slope
    eval_b[[j]][i,6]<-as.numeric(eval_function_b(bayesian_df,slope[i],mass[j])[[6]])
    
  }
}

list <- unlist(effect_b_size, recursive = FALSE)
bayesian_df <- do.call("rbind",list)
head(bayesian_df)


write.csv(bayesian_df, file="Bayesian_Results.csv")

#Converting the matrix into a data.frame
eval_b_df<-data.frame(do.call(rbind,eval_b))
head(eval_b_df)
nrow(eval_b_df)

#Renaming the columns of the matrix
colnames(eval_b_df)<-c('Adj_R_squared','Adj_R_squared_SD','R_squared','R_squared_SD','Mass','Slope')
eval_b_df$Mass<-round(eval_b_df$Mass,5)

write.csv(eval_b_df, file="Bayesian_Results_Summary.csv")

#######Plotting the results of the Bayesian optimal escape model


#2D plotting of looming stimulus hypothesis
#Only including a single log body mass value value, 0.075 sec
eval_b_df$Mass<-(ifelse(eval_b_df$Mass==5.52511,eval_b_df$Mass,NA))

eval_b_df<-na.omit(eval_b_df)

#Estimating f_squared from Cohen 1988 from R_squared
f<-lapply(eval_b_df$R_squared, f_func)
f_squared<-do.call(rbind,f)
#appending f_squared to data frame
eval_b_df$f_squared<-f_squared

#2D graph
ggplot(data=eval_b_df,aes(x=Slope, y=f_squared)) +
  geom_point(size=2)+
  geom_line(lwd=2)+
  xlab("Slope") +
  ggtitle("                Bayesian optimal escape model")+
  #geom_errorbar(aes(ymin=Adj_R_squared-Adj_R_squared_SD, ymax=Adj_R_squared+Adj_R_squared_SD), colour="black", width=.1)+
  ylab(expression(italic(f^{2})))+
  scale_y_continuous(expand =c(0,0))+coord_cartesian(ylim = c(0,.15))+
  #ylab("Total Cost of Collision per species (millions)")+
  theme_classic(base_size = 16)


#Mean Mean f^2 and SD value for negative slopes
mean(eval_b_df[1:37,7])
sd(eval_b_df[1:37,7])

#Mean Mean Phi and SD value for positive slopes
mean(eval_b_df[39:70,7])
sd(eval_b_df[39:70,7])



#Converting the matrix into a data.frame
eval_b_df<-data.frame(do.call(rbind,eval_b))
head(eval_b_df)
nrow(eval_b_df)

#Renaming the columns of the matrix
colnames(eval_b_df)<-c('Adj_R_squared','Adj_R_squared_SD','R_squared','R_squared_SD','Mass','Slope')

f<-lapply(eval_b_df$R_squared, f_func)
f_squared<-do.call(rbind,f)
#appending f_squared to data frame
eval_b_df$f_squared<-f_squared


#Generating a 3D graph between log bod mass, slope, and f^2 for the Bayesian optimal escape model
#Putting f^2 into a matrix
f_Squared<-matrix(eval_b_df$f_squared,nrow = 70, ncol=50)

#Creating titles for the 3D graph
axx <- list(
  title = "Log Body Mass (g)"
)

axy <- list(
  title = "Slope"
)

axz <- list(
  title = "f squared"
)


#Plotting the three different variables
surf_b<-plot_ly(x=~mass,y=~slope, z = ~f_Squared, type = "surface", showlegend=T)
#surf_b<- surf_b %>% layout(legend=list(title=list(text='<b>Probability<b>')))
surf_b <- surf_b %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
#surf_b <- surf_b %>% layout(title= "Bayesian Escape Model")
surf_b <- surf_b %>%layout(radialaxis = list(ticksuffix = "%"), orientation = 270)

surf_b

