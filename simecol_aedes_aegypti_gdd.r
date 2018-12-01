##===================================================================
## 1. Define the Model as a simecol object
##===================================================================
library(simecol)
##-------------------------------------------------------------------
## 1.1 Derive a new class from the simecol base class simObj
##-------------------------------------------------------------------


setClass("indbasedModel2",
         representation(
           parms  = "list",
           init   = "data.frame"
         ),
         contains = "simObj"
)

# Funcoes para calcular o GDD

setwd("dados_temperatura_28_11_2018")
DADOS_CLIMA <- read.csv(file="dados", header = TRUE, sep=",")

tb <- c(2.9, 8.6, 10.8, 3.43) # temperatura base (em cada fase)
DIAS <- length(DADOS_CLIMA$DATA)/24  # Quantidade de dias 
MINIMAS <- as.vector(DIAS)
MAXIMAS <- as.vector(DIAS)
FASE = 1
ITERACAO = 1

#Calculo da Temperatura MINIMA
MINIMAS[1] <- min(DADOS_CLIMA$temp_min[1:24])
for (index in 1:365) {
  MINIMAS[index] <- min(DADOS_CLIMA$temp_min[(24*(index-1)+1):(24*index)])
}

#Calculo da Temperatura MAXIMA
MAXIMAS[1] <- max(DADOS_CLIMA$temp_max[1:24])
for (index in 1:365) {
  MAXIMAS[index] <- max(DADOS_CLIMA$temp_max[(24*(index-1)+1):(24*index)])
}

temperatura_media = function(ITERACAO,fase){
  MEDIAS <- ((MAXIMAS[ITERACAO] + MINIMAS[ITERACAO])/2) - tb[fase]
  MEDIAS
}


##-------------------------------------------------------------------
## 1.2 Define the model object
##-------------------------------------------------------------------

ibm_aedes = new("indbasedModel2",
                main = function(time, init, parms) { 
                  
                  init = adult(init, parms)
                  init = development(init, parms)
                  init = pupa(init, parms)
                  init = larva(init, parms)
                  init = egg(init, parms)
                  init = survive(init, parms)
                  #we are using the opposite cycle direction (adult - pupa - larva - egg) 
                  #because it will prevent an individual to enter two stages in a single day
                },
                equations = list(
                  newaedes = function(n) { 
                    if (n>0) { 
                      data.frame(age = rep(0, n), phase = 1, eggs = 0, gdd = 0, oviposition = 0) 
                      #create data.frame with 5 columns: age, phase, eggs, gdd, oviposition
                    } else { 
                      NULL
                    }
                  },
                  
                  egg = function(inds, parms){   
                    with(parms,{
                      inds[inds$phase==1 & parms$water >= 10,'gdd'] <- inds[inds$phase==1 & parms$water >= 10,'gdd'] + temperatura_media(DELTAT, 1)
                      #checks if water level is at least 10mL
                      
                      inds[inds$phase==1 & parms$water >=10, 'age'] <- inds[inds$phase==1 & parms$water >=10, 'age'] + DELTAT
                      #if water < 10mL egg age won't change
                      
                      EtL  <- which(inds$phase == 1 & inds$gdd>=parms$k[1]) #egg to larva
                      if(length(EtL)>0) {
                        inds$phase[EtL] = 2 #change phase
                        inds$gdd[EtL] = inds$gdd[EtL] - parms$k[1] #inds who evolved will most likely surpass the minimum gdd necessary to next phase in this case, we need to add the excess to the next stage 
                      }
                      inds  #return inds
                    })},
                  larva = function(inds, parms){
                    with(parms,{
                      inds[inds$phase==2, 'age'] <- inds[inds$phase==2, 'age'] + DELTAT 
                      
                      inds[inds$phase==2,'gdd'] <- inds[inds$phase==2,'gdd']  + temperatura_media(DELTAT, 2)
                      
                      LtP  <- which(inds$phase == 2 & inds$gdd>=parms$k[2]) #Larva to Pupa
                      if(length(LtP)>0) {
                        inds$phase[LtP] = 3 #change phase
                        inds$gdd[LtP] = inds$gdd[LtP] - parms$k[2]  #inds who evolved will most likely surpass the minimum gdd necessary to next phase in this case, we need to add the excess to the next stage 
                        
                      }
                      inds
                    })},
                  pupa = function(inds, parms){
                    with(parms,{
                      # print(inds)
                      inds[inds$phase==3, 'age'] <- inds[inds$phase==3, 'age'] + DELTAT 
                      
                      inds[inds$phase==3,'gdd'] <- inds[inds$phase==3,'gdd'] + temperatura_media(DELTAT, 3)
                      PtA  <- which(inds$phase == 3 & inds$gdd>=parms$k[3]) #Pupa to adult
                      if(length(PtA)>0) { #checks if there's any individual with phase = 3 and GDD > 465.6
                        inds$eggs[PtA] = round(runif(inds$eggs,200,300))[PtA] 
                        #insert a random Number between 200 and 300 when the pupa is ready to become an adult
                        # ...[PtA] returns TRUE or FALSE and indicate which individuals are ready
                        inds$phase[PtA] = 4 #change phase
                        inds$gdd[PtA] = inds$gdd[PtA] - parms$k[3]  #inds who evolved will most likely surpass the minimum gdd necessary to next phase in this case, we need to add the excess to the next stage 
                      }
                      inds
                    })},
                  survive  = function(inds, parms){ 
                    
                    eggsLive <-  subset(inds,inds$phase==1 & runif(age) > (gdd*parms$deathrate[1]))  #...
                    larvaLive <-  subset(inds,inds$phase==2 & runif(age) > (gdd*parms$deathrate[2]))
                    pupaLive <-  subset(inds,inds$phase==3 & runif(age) > (gdd*parms$deathrate[3]))
                    young <- subset(inds, inds$gdd < parms$k[4])
                    adultLive <-  subset(young,young$phase==4 & runif(age) > (gdd*parms$deathrate[4]))
                    
                    inds <- rbind(eggsLive,larvaLive,pupaLive,adultLive)
                    inds
                  },
                  adult = function(inds, parms){
                    with(parms,{
                      inds[inds$phase==4, 'age'] <- inds[inds$phase==4, 'age'] + DELTAT 
                      inds[inds$phase==4, 'gdd'] <- inds[inds$phase==4, 'gdd'] + temperatura_media(DELTAT, 4)
                      inds
                    })},
                  development = function(inds, parms){
                    newinds = NULL
                    with(parms,{
                      have.neo  = which(inds$eggs > 0 & (inds$age %% 3 == 0))
                      oviposition = round(runif(inds$eggs[have.neo],30,50))
                      inds$oviposition[have.neo] = inds$oviposition[have.neo] + 1
                      eggs = inds$eggs[have.neo]
                      ImmatureEggs = ifelse(length(eggs) > 0, mapply(max, (eggs - oviposition), 0), 0) 
                      new.neo = sum(eggs - ImmatureEggs)
                      
                      inds$eggs[have.neo] <- ImmatureEggs
                      
                      newinds = newaedes(new.neo) #create new inds according to the sum(eggs)
                      rbind(inds, newinds) # bind these new inds to the data.frame
                    })}
                  
                  
                ),parms = list(
                  maxage      = 44,       #days
                  temp        = 25, #runif(1,25,40),       
                  water = 10, #runif(1,8,10),
                  bt = c(2.9, 8.6, 10.8, 3.43),
                  k = c(305.8, 433.5, 465.6, 800), #added a new parameter (660) for the adult mosquitos 
                  deathrate = c(0.383, 0.075, 0.062, 0.02)/ c(305.8, 433.5, 465.6, 800)
                ),                      
                init = data.frame(age= c(12, 17, 1, 14, 22, 19, 8, 20, 11, 15), phase= c(3, 4, 1, 4, 4, 4, 2, 4, 3, 4), eggs=c(0,110,0,150,0,115,0,105,0,152), 
                                  gdd= c(444, 530, 172, 473, 749, 585, 381, 697, 439, 504), oviposition = 0),
                times = c(from=0, to=80, by=1),
                solver = "myiteration"
)


##-------------------------------------------------------------------
## 1.3 Optionally define a user provided solver function that stores
##     the data more model specific than the default iteration
##-------------------------------------------------------------------


myiteration = function(y, times = NULL, func = NULL, parms = NULL,
                       animate = FALSE, ...) {
  observer = function(res) { 
    
    Number   = nrow(res)
    Meaneggs = sum(res$eggs)
    EggsN = length(which(res$phase==1))
    LarvaeN = length(which(res$phase==2))
    PupaeN = length(which(res$phase==3))
    AdultsN = length(which(res$phase==4))
    
    c(Number=Number, Meaneggs=Meaneggs, EggsN=EggsN,LarvaeN=LarvaeN,PupaeN=PupaeN,AdultsN=AdultsN)
  }
  init              = y@init
  times             = fromtoby(y@times)
  func              = y@main
  parms             = y@parms
  inputs            = y@inputs
  equations         = y@equations
  equations         = addtoenv(equations)
  environment(func) = environment()
  parms$DELTAT = 0
  res = observer(init)
  out = res
  for (i in 2:length(times)) { 
    time = times[i]
    parms$DELTAT = times[i] - times[i-1]
    init = func(time, init, parms)
    res  = observer(init)
    out  = rbind(out, res)
  }
  row.names(out) = NULL
  out = cbind(times, out)
  as.data.frame(out)
}

##-------------------------------------------------------------------
## 1.4 Define a user specified plot function according to the
##     data structure stored by "myiteration"
##-------------------------------------------------------------------
setMethod("plot", c("indbasedModel2", "missing"), function(x, y, ...) {
  opar = par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow=c(3, 2))
  o = out(x)
  
  plot(o$times, o$Meaneggs, type="l", xlab="Time", ylab="Eggs per individual")
  plot(o$times, o$Number,   type="l", xlab="Time", ylab="Abundance")
  plot(o$times, o$EggsN,   type="l", xlab="Time", ylab="Number of Eggs")
  plot(o$times, o$LarvaeN,   type="l", xlab="Time", ylab="Number of Larvae")
  plot(o$times, o$PupaeN,   type="l", xlab="Time", ylab="Number of Pupae")
  plot(o$times, o$AdultsN,   type="l", xlab="Time", ylab="Number of Adults")
  data.frame(o)
})

##===================================================================
## 2. Simulate the Model
##===================================================================

solver(ibm_aedes) = "myiteration"
ibm_aedes = sim(ibm_aedes)
plot(ibm_aedes)

