
# Main Function of simulating bottom-up cracking

source("BottomUpCracking_lib.R")
n.hours=365*3      # of hours to be simulated
set.seed(1)
load.vec=runif(n.hours,0,200)   # simulated traffic load profiles

Nf.vec=Nf_Model_2(load.vec)
Damage.vec=cumsum(load.vec/Nf.vec)
crack=Crack_Model(Damage.vec)

plot(crack)


source("BottomUpCracking_lib.R")
n.load=24      # of hours to be simulated
set.seed(1)
load.vec=runif(n.load,50,100)   # simulated traffic load profiles
times.vec<-sample(50:150,size=length(load.vec),replace = T)   # of times per weight 

DayDamage.vec<-c()
for(i in 1:365){
  load.vec=runif(n.load,50,100)  # simulated traffic load profiles per day
  times.vec=sample(50:150,size=length(load.vec),replace = T)  # of times per weight
  DayDamage.vec[i]=sum(times.vec/Nf_Model_2(load.vec)) # Damage per day i
}

Damage.vec<-cumsum(DayDamage.vec) # cumulative damage vector for a year
crack<-Crack_Model(Damage.vec)
plot(crack,xlim=c(0,10),ylim=c(0,2),type="l")
