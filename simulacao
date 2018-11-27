
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
                  main = function(time, init, parms) { #para cada vez q ela ser chamada
                    init = live(init, parms)
                    init = survive(init, parms)
                    init = hatch(init, parms)
                    #init = death(init)
                    init  #retorna todos os inits acima
                    #qual seria substituido
                  },
                  equations = list(
                    newaedes = function(n) { 
                      if (n>0) { #se nasceu no dia
                        data.frame(age = rep(0, n), fase = 1, eggs = 0, gdd = 0, ovoposicao = 0) 
                        
                      } else { #se nao nasceu no dia
                        NULL
                      }
                    },

                    live = function(inds, parms){
                      with(parms,{
                        ninds       = nrow(inds) #fala o numero de individuos atraves da  contagem do numero de linhas na tabela

                        if (ninds == 0){  #caso todos individuos tenham morrido, printa no console a seguinte mensagem
                        
                          warning("All individuals died - Game Over")  
                          
                        }
                       
                        #verifica se tem nivel minimo de 10mL de agua
                        inds[inds$fase==1 & parms$agua >= 10,'gdd'] <- inds[inds$fase==1 & parms$agua >= 10,'gdd'] + (parms$temp-parms$tb[1])
                        
                        #para o age caso nao tem nivel minimo de 10mL de agua
                        inds[inds$fase==1 & parms$agua >=10, 'age'] <- inds[inds$fase==1 & parms$agua >=10, 'age'] + DELTAT
                        
                        inds[inds$fase!=1, 'age'] <- inds[inds$fase!=1, 'age'] + DELTAT # os demais aumentam
                        
                        inds[inds$fase==2,'gdd'] <- inds[inds$fase==2,'gdd'] + (parms$temp-parms$tb[2])
                        
                        inds[inds$fase==3,'gdd'] <- inds[inds$fase==3,'gdd'] + (parms$temp-parms$tb[3])
                        
                        inds[inds$fase==4,'gdd'] <- inds[inds$fase==4,'gdd'] + (parms$temp-parms$tb[4])
                        
                        OpL  <- which(inds$fase == 1 & inds$gdd>=305.8) #ovo para larva
                        inds$fase[OpL] = 2 #troca de fase
                        inds$gdd[OpL] = 0 #zera graus dia de quem mudou de fase
                        LpP  <- which(inds$fase == 2 & inds$gdd>=433.5) #Larva para Pupa
                        inds$fase[LpP] = 3 #troca de fase
                        inds$gdd[LpP] = 0 #zera graus dia de quem mudou de fase
                        PpA  <- which(inds$fase == 3 & inds$gdd>=465.6) #Pupa para Adulto
                        inds$fase[PpA] = 4 #troca de fase
                        inds$gdd[PpA] = 0 #zera graus dia de quem mudou de fase
                        #1204.9
                        
                        #****fazer subset que elimine os individuos em fases aquaticas caso nao ha 10mL****
                        #subset(inds, inds$fase==2 | inds$fase==3 & parms$agua <=10)
                        
                        inds #retorna os resultados
                      })},
                    survive  = function(inds, parms){ 
                      diasfase = c(4.1, 6.6, 2.4, 12.4) # médias de dias 
                      probmorte = c(0.383, 0.075, 0.062, 0.02) # probabilidade de morte em cada fase
                      probmorte = probmorte / diasfase # ajustado por que os indivíduos são testados para morte a cada dia, e não a cada fase
                      subset(inds, runif(inds$age) > probmorte[inds$fase] & inds$age <= parms$maxage & parms$agua != 0) #|
                               #runif(inds$age) > probmorte[inds$fase] & inds$age <= parms$maxage & parms$agua == 0 & inds$fase == 1)
                      
                    },
                    hatch = function(inds, parms) {
                      newinds = NULL
                      with(parms, {
                        
                        #***ver maneira de fazer o max de ovosicao randomico para cada individuo***
                        # maxovoposicao = runif(1,1,5)
                        #ovoposicao < 5 ou ovoposicao <= maxovoposicao
                        
                       # inds[inds$fase==4,'eggs'] <- inds[inds$fase==4,'eggs'] + round(runif(1,40,200))
                        inds[inds$fase==4 & inds$ovoposicao < 5,'eggs'] <- inds[inds$fase ==4 & inds$ovoposicao < 5,'eggs'] + round(runif(1,40,200))
                        #verifica se o individuo esta na fase 4 e gera um numero random de eggs de 40-200
                        
                        inds[inds$fase==4  & inds$ovoposicao <= 5,'ovoposicao'] <- inds[inds$fase==4 & inds$ovoposicao <= 5,'ovoposicao'] + 1
                        #verifica se chegou no limite maximo de ovoposicao, caso sim, o mosquito nao gera mais ovos
                        
                       #print(inds)
                        #print("**********")
                        
                        have.neo  = 0
                        new.neo   = 0
                        have.neo  = which(inds$eggs > 0)
                        
                        eggs      = inds$eggs[have.neo] #seleciona somente os ind que tem eggs
                        #print(eggs)
                        new.neo   = sum(eggs) # soma o total de ovos 
                        #print("Antes ")
                        #print(new.neo)
                        inds$eggs[have.neo]   = 0
                       #print("Depois ")
                        #print(new.neo)
                         # zera os q tinham ovos
                        newinds = newaedes(new.neo) # cria novos mosquitos 
                        rbind(inds, newinds) # adiciona na lista (data.frame) de individuos
                      })
                    }
                  ),parms = list(
                    maxage      = 44,       #dias
                    temp        = 40, #runif(1,25,40),       
                    agua = 10, #runif(1,8,10),
                    tb = c(2.9, 8.6, 10.8, 3.43)
                  ),                      
                  init = data.frame(age=0, fase=4, eggs=0, gdd=0, ovoposicao = 0),
                  times = c(from=0, to=60, by=1),
                  solver = "myiteration"
)

##-------------------------------------------------------------------
## 1.3 Optionally define a user provided solver function that stores
##     the data more model specific than the default iteration
##-------------------------------------------------------------------

#aqui possivelmente esta o problema
myiteration = function(y, times = NULL, func = NULL, parms = NULL,
                       animate = FALSE, ...) {
  observer = function(res) { 
    # eggs, size, age, eggage
    number   = nrow(res)
    meanfase = mean(res$fase)
    meanage  = mean(res$age)
    meaneggs = mean(res$eggs)
    #print("res$eggs:")
    #print(res$eggs)
   #print("mean eggs:")
    #print(meaneggs)
    c(number=number, meanfase=meanfase, meanage=meanage, meaneggs=meaneggs)
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
  for (i in 2:length(times)) { #comeca em 2 porque vetores comecam em 1 inves de 0
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

