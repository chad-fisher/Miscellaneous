# Lactation Model
# Chad Fisher
# 10.23.23

#Lactation model based on Heather et al. (1983) https://doi.org/10.1017/S0021859600037710

#This function executes the lactation model for the set of parameters given in table A1.32
# State Variables:
  {# H = hormone levels [kg/m^3]?
  # CS = level of milk in milk secreting cells []?
  # M = quantity of milk in animal [kg]
  # Mmean = time average of milk [kg] 
  # RM = the amount of milk removed [kg/day]
  # Y = Yield, cumulative [kg]
  }

#Inputs:
  {# cu: Number of undifferentiated cells
  # kdiv: Cell division rate aka Michaelis-Menten constant [kg hormone/m^3]
  # kdl: constant degradation of milk [kg]
  # kdh: rate of hormone decompsotion [1/day]
  # km: Constant secretion of milk [kg/cell/day]
  # ksl: Milk secretion rate aka Michaelis-Menten constant [kg]
  # Kr: Average milk constant [1/day]
  # ks: basal cell degradation rate [1/day]
  # ksm: Constant rate of degradation of milk secreting cells [1/day]
  # mh: half-response point parameter [kg]
  # mm: storage capacity of milk per animal [kg/animal]
  # P: steepness parameter for response of milk destruction inside animal when not needed
  # mum: maximum rate of cell division [divisions/cell/day]
  # rc: regular consumption of milk by calf [kg/day]
    
    
  # rma: ????/ []?
  # dur: duration of simulation [days]
  # dt: time step of simulation [days]
  }
  
#Default parameter values determined from curve fitting to experimental data
lactationmodel=function(cu=1000,
                        kdiv=0.2,
                        kdl=5.0,
                        kdh=0.01,
                        km=0.00506,
                        ksl=3.0,
                        Kr=0.048,
                        ks=0.1,
                        ksm=0.2,
                        mh=27.0,
                        mm=30.0,
                        P=10,
                        mum=1.0,
                        rc=40,
                        rma=4000,
                        dur,
                        dt=0.01)
  {
  
  #Initializing output dataframe
  time=seq(0,dur,dt)
  output=data.frame(time,H=NA,CS=NA,M=NA,Mmean=NA,RM=NA,Y=NA)
  
  #Initial values based on hormone pulse due to birthing
  output$H[1]=1
  output$CS[1]=0
  output$M[1]=0
  output$Mmean[1]=0
  output$Y[1]=0
  
  #Looping through time
  for(i in 1:(length(time)-1)) {
    #calculating time step
    dt_curr=time[i+1]-time[i]
    
    #Steady consumption by calf, model can also be adjusted for an automated milking
    m_t=rc
    
    #Difference equation forms of flows/kinetic equations
    dH=-kdh*output$H[i]*dt_curr
    dCS=(mum*(output$H[i]/(kdiv+output$H[i]))*cu-(ks+ksm*((output$Mmean[i]/mh)^P)/((1+output$Mmean[i]/mh)^P))*output$CS[i])*dt_curr
    dM=(km*output$CS[i]*((mm-output$M[i])/(mm-output$M[i]+ksl))-(output$M[i]/(kdl+output$M[i]))*m_t)*dt    
    dMmean=Kr*(output$M[i]-output$Mmean[i])*dt
    
    #Calculating state variables
    output$H[i+1]=output$H[i]+dH
    output$CS[i+1]=output$CS[i]+dCS
    output$M[i+1]=output$M[i]+dM
    output$Mmean[i+1]=output$Mmean[i]+dMmean
    output$RM[i]=output$M[i]/(kdl+output$M[i])*m_t
    output$Y[i+1]=output$Y[i]+output$RM[i]*dt
    }
  
  #For plotting results
  {
    par(mfrow=c(2,2))
    plot(output$time,output$M,xlab="Time (days)",ylab="Quantity of milk in animal (kg)",
         type="l",col="black")
    plot(output$time,output$Mmean,xlab="Time (days)",ylab="Average amount of milk in animal (kg)",
        type="l",col="black")
    plot(output$time,output$CS,xlab="Time (days)",ylab="# of secretory cells",
        type="l",col="black")
    plot(output$time,output$RM,xlab="Time (days)",ylab="Milk removed (kg)",
        type="l",col="black")
  }
  
  return(output)
  }