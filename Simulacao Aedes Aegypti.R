
##===================================================================
## 1. Define the Model as a simecol object
##===================================================================
library(simecol)
##-------------------------------------------------------------------
## 1.1 Derive a new class from the simecol base class simObj
##-------------------------------------------------------------------



setClass("indbasedModel",
         representation(
           parms  = "list",
           init   = "data.frame"
         ),
         contains = "simObj"
)

##-------------------------------------------------------------------
## 1.2 Define the model object
##-------------------------------------------------------------------

ibm_aedes = new("indbasedModel",
                main = function(time, init, parms) { 
                  
                  init = adult(init, parms)
                  init = development(init, parms)
                  init = pupa(init, parms)
                  init = larva(init, parms)
                  init = egg(init, parms)
                  init = survive(init, parms)
                  
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
                  #we are using the opposite cycle direction (adult - pupa - larva - egg) 
                  #because it will prevent an individual to enter two stages in a single day
                  egg = function(inds, parms){
                    with(parms,{
                      inds[inds$phase==1 & parms$water >= 10,'gdd'] <- inds[inds$phase==1 & parms$water >= 10,'gdd'] + (parms$temp-parms$bt[1])
                      #checks if water level is at least 10mL
                      
                      inds[inds$phase==1 & parms$water >=10, 'age'] <- inds[inds$phase==1 & parms$water >=10, 'age'] + DELTAT
                      #if water < 10mL egg age won't change
                      
                      EtL  <- which(inds$phase == 1 & inds$gdd>=parms$k[1]) #egg to larva
                      inds$phase[EtL] = 2 #troca de phase
                      inds$gdd[EtL] = inds$gdd[EtL] - parms$k[1] 
                      #inds who evolved will most likely surpass the minimum gdd necessary to next phase 
                      #in this case we need to add the excess to the next stage 
                      
                      inds
                    })},
                  larva = function(inds, parms){
                    with(parms,{
                      inds[inds$phase==2, 'age'] <- inds[inds$phase==2, 'age'] + DELTAT 
                      
                      inds[inds$phase==2,'gdd'] <- inds[inds$phase==2,'gdd'] + (parms$temp-parms$bt[2])
                      
                      LtP  <- which(inds$phase == 2 & inds$gdd>=parms$k[2]) #Larva to Pupa
                      inds$phase[LtP] = 3 #change phase
                      inds$gdd[LtP] = inds$gdd[LtP] - parms$k[2] 
                      #inds who evolved will most likely surpass the minimum gdd necessary to next phase 
                      #in this case, we need to add the excess to the next stage 
                      
                      inds
                      #subset
                    })},
                  pupa = function(inds, parms){
                    with(parms,{
                      inds[inds$phase==3, 'age'] <- inds[inds$phase==3, 'age'] + DELTAT 
                      
                      inds[inds$phase==3,'gdd'] <- inds[inds$phase==3,'gdd'] + (parms$temp-parms$bt[3])
                      PtA  <- which(inds$phase == 3 & inds$gdd>=parms$k[3]) #Pupa to adult
                      inds$eggs[PtA] = round(runif(inds$eggs,200,300))[PtA] 
                      inds$phase[PtA] = 4 #change phase
                      inds$gdd[PtA] = inds$gdd[PtA] - parms$k[3]  
                      #inds who evolved will most likely surpass the minimum gdd necessary to next phase 
                      #in this case we need to add the excess to the next stage 
                       
                      #insert a random number between 200 and 300 when the pupa is ready to become an adult
                      # ...[PtA] returns TRUE or FALSE and indicate which individuals are ready
                    
                      inds
                    })},

                  
                  
                     survive  = function(inds, parms){ 
                  #     diasphase = c(4.1, 6.6, 2.4, 12.4) # médias de dias 
                      probmorte = c(0.383, 0.075, 0.062, 0.02) # probabilidade de morte em cada phase
                      probmorte <- probmorte / parms$k

                      
                      #eggsLive <-  subset(inds[inds$phase==1,],  runif(age) > (gdd*probmorte[1]))
                      #larvaLive <- subset(inds[inds$phase==2,],  runif(age) > (gdd*probmorte[2]))
                      #pupaLive <-  subset(inds[inds$phase==3,],  runif(age) > (gdd*probmorte[3]))
                      #adultLive <- subset(inds[inds$phase==4,],  runif(age) > (gdd*probmorte[4]) & gdd<parms$k[4])
                      
                     #inds <- rbind(eggsLive,larvaLive,pupaLive,adultLive)
                     
                     inds
                  
                },
                     adult = function(inds, parms){
                       with(parms,{
                         inds[inds$phase==4, 'age'] <- inds[inds$phase==4, 'age'] + DELTAT 
                         inds[inds$phase==4, 'gdd'] <- inds[inds$phase==4, 'gdd'] + (parms$temp-parms$bt[4])
                         inds
                       })},
                     development = function(inds, parms){
                       newinds = NULL
                       with(parms,{
                         have.neo  = which(inds$eggs > 0 & (inds$age %% 3 == 0))
                         oviposition = round(runif(inds$eggs[have.neo],30,50))
                         inds$oviposition[have.neo] = inds$oviposition[have.neo] + 1
                         eggs = inds$eggs[have.neo]
                         #print("oviposition ")
                         #print(oviposition)
                       #  print(inds)
                         #print("**********")
                         #print("eggs ")
                         #print(eggs)
                         ImmatureEggs = ifelse(length(eggs) > 0, mapply(max, (eggs - oviposition), 0), 0) #problema
                         new.neo = sum(eggs - ImmatureEggs)
                         
                         inds$eggs[have.neo] <- ImmatureEggs
                         
                         newinds = newaedes(new.neo) #create new inds according to the sum(eggs)
                         rbind(inds, newinds) # bind these new inds to the data.frame
                       })}
                    
                     
                ),parms = list(
                  maxage      = 44,       #days
                  temp        = 50, #runif(1,25,40),       
                  water = 10, #runif(1,8,10),
                  bt = c(2.9, 8.6, 10.8, 3.43),
                  k = c(305.8, 433.5, 465.6, 660) #added a new parameter (660) for the adult mosquitos 
                  #probmorte = c(0.383, 0.075, 0.062, 0.02)
                ),                      
                init = data.frame(age=0, phase=4, eggs=150, gdd=0, oviposition = 0),
                times = c(from=0, to=60, by=1),
                solver = "myiteration"
)
#alterar o metodo de morte - ver como resolver o problema
#adicionar o grupo de individuos conforme o campina grande - ver porque entra no estado de pupa mesmo sendo egg
#alterar graficos, adicionar numero de individuos em cada fase e o numero de ovos que cada adulto tem

##-------------------------------------------------------------------
## 1.3 Optionally define a user provided solver function that stores
##     the data more model specific than the default iteration
##-------------------------------------------------------------------


myiteration = function(y, times = NULL, func = NULL, parms = NULL,
                       animate = FALSE, ...) {
  observer = function(res) { 
    # eggs, size, age, eggage
   # t = length(which(res$phase==4))
    number   = nrow(res)
    meanphase = mean(res$phase)
    meanage  = mean(res$age)
    meaneggs = mean(res$eggs)
    #print("res$eggs:")
    #print(res$eggs)
    #print("mean eggs:")
    #print(meaneggs)
    c(number=number, meanphase=meanphase, meanage=meanage, meaneggs=meaneggs)
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
    #print(time)
  }
  row.names(out) = NULL
  out = cbind(times, out)
  as.data.frame(out)
}

##-------------------------------------------------------------------
## 1.4 Define a user specified plot function according to the
##     data structure stored by "myiteration"
##-------------------------------------------------------------------
setMethod("plot", c("indbasedModel", "missing"), function(x, y, ...) {
  opar = par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow=c(2, 2))
  o = out(x)
  
  plot(o$times, o$meanage,  type="l", xlab="Time", ylab="Mean age (d)")
  plot(o$times, o$meaneggs, type="l", xlab="Time", ylab="Eggs per individual")
  plot(o$times, o$number,   type="l", xlab="Time", ylab="Abundance")
  plot(o$times, o$number,   type="l", xlab="Time", ylab="Abundance", log="y")
})

##===================================================================
## 2. Simulate the Model
##===================================================================

##solver(ibm_aedes) = "myiteration"
ibm_aedes = sim(ibm_aedes)
plot(ibm_aedes)
