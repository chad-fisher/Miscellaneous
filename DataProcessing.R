# Final Project DNDC Data Processing
# Chad Fisher
# 11.27.23

#This script processes and graphs data from DNDC outputs

#Clear and initialize workspace and working directory
rm(list = ls())

#Paper scenarios
{
  setwd("C:/Users/cfishe07/Box/Final Project/Results")
  
  #Import datasets
  library(readxl)
  OriginalData <- read_excel("Summary.xlsx", 
                              sheet = "Original")
  BDadjData <- read_excel("Summary.xlsx", 
                          sheet = "BDadj")
  SOCadjData <- read_excel("Summary.xlsx", 
                          sheet = "SOCadj")
  
  #Initializing output data frames
  year=1:14
  OutputD1=data.frame(year)
  OutputD2=data.frame(year)
  
  #Looping through original data and generating D1 (0 - 15 cm) and D2 (15 - 30 cm) average SOC
  for(x in 1:12)
  {
    for(y in 1:14)
    {
    CurrentColumn1 <- 3*x-1
    CurrentColumn2 <- 3*x
    CurrentColumn3 <- 3*x+1
    val1=OriginalData[y,CurrentColumn1]
    val2=OriginalData[y,CurrentColumn2]
    val3=OriginalData[y,CurrentColumn3]
    OutputD1[y,x+1] <- mean(c(val1[[1]],val2[[1]]))
    OutputD2[y,x+1] <- mean(c(val2[[1]],val3[[1]]))
    }
  }
  
  #Looping through bulk density adjusted data and generating D1 (0 - 15 cm) and D2 (15 - 30 cm) average SOC
  for(x in 13:24)
  {
    for(y in 1:14)
    {
      CurrentColumn1 <- 3*(x-12)-1
      CurrentColumn2 <- 3*(x-12)
      CurrentColumn3 <- 3*(x-12)+1
      val1=BDadjData[y,CurrentColumn1]
      val2=BDadjData[y,CurrentColumn2]
      val3=BDadjData[y,CurrentColumn3]
      OutputD1[y,x+1] <- mean(c(val1[[1]],val2[[1]]))
      OutputD2[y,x+1] <- mean(c(val2[[1]],val3[[1]]))
    }
  }
  
  #Looping through SOC data and generating D1 (0 - 15 cm) and D2 (15 - 30 cm) average SOC
  for(x in 25:36)
  {
    for(y in 1:14)
    {
      CurrentColumn1 <- 3*(x-24)-1
      CurrentColumn2 <- 3*(x-24)
      CurrentColumn3 <- 3*(x-24)+1
      val1=SOCadjData[y,CurrentColumn1]
      val2=SOCadjData[y,CurrentColumn2]
      val3=SOCadjData[y,CurrentColumn3]
      OutputD1[y,x+1] <- mean(c(val1[[1]],val2[[1]]))
      OutputD2[y,x+1] <- mean(c(val2[[1]],val3[[1]]))
    }
  }
  
  #0 - 15 cm plots original data
  {
    par(mfrow = c(3, 2))
  
    plot(OutputD1$year,
       OutputD1$V2,
       type = "p",
       col = "black",
       ylim = c(24000, 30000),
       main="No-till, 0 N",
       ylab="SOC (kg/ha)",
       xlab="year")
    lines(OutputD1$year,
          OutputD1$V3,
          type = "l",
          col = "black"
    )
    
    plot(OutputD1$year,
         OutputD1$V4,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V5,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V6,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V7,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V8,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V9,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V10,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V11,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V12,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V13,
          type = "l",
          col = "black",
    )
  }
  
  #15 - 30 cm plots original data
  {
    par(mfrow = c(3, 2))
    
    plot(OutputD2$year,
         OutputD2$V2,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V3,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V4,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V5,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V6,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V7,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V8,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V9,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V10,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V11,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V12,
         type = "p",
         col = "black",
         ylim = c(24000, 30000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V13,
          type = "l",
          col = "black",
    )
  }
  
  #0 - 15 cm plots BD adj data
  {
    par(mfrow = c(3, 2))
    
    plot(OutputD1$year,
         OutputD1$V14,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V15,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V16,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V17,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V18,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V19,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V20,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V21,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V22,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V23,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V24,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V25,
          type = "l",
          col = "black",
    )
  }
  
  #15 - 30 cm plots BD adj data
  {
    par(mfrow = c(3, 2))
    
    plot(OutputD2$year,
         OutputD2$V14,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V15,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V16,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V17,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V18,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V19,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V20,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V21,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V22,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V23,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V24,
         type = "p",
         col = "black",
         ylim = c(15000, 21000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V25,
          type = "l",
          col = "black",
    )
  }
  
  #0 - 15 cm plots SOC adj data
  {
    par(mfrow = c(3, 2))
    
    plot(OutputD1$year,
         OutputD1$V26,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V27,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V28,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V29,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V34,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V35,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V36,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V37,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V30,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V31,
          type = "l",
          col = "black",
    )
    
    plot(OutputD1$year,
         OutputD1$V32,
         type = "p",
         col = "black",
         ylim = c(45000, 65000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD1$year,
          OutputD1$V33,
          type = "l",
          col = "black",
    )
  
  }
  
  #15 - 30 cm plots BD adj data
  {
    par(mfrow = c(3, 2))
    
    plot(OutputD2$year,
         OutputD2$V26,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V27,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V28,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V29,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V34,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="Moldboard till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V35,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V36,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="Moldboard till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V37,
          type = "l",
          col = "black", 
    )
    
    plot(OutputD2$year,
         OutputD2$V30,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="Chisel till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V31,
          type = "l",
          col = "black",
    )
    
    plot(OutputD2$year,
         OutputD2$V32,
         type = "p",
         col = "black",
         ylim = c(30000, 55000),
         main="Chisel till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputD2$year,
          OutputD2$V33,
          type = "l",
          col = "black",
    )
  
  }
}

#Air Temp scenarios
{
  #Set WD
  setwd("C:/Users/cfishe07/Box/Final Project/AirTempResults")
  
  #Import dataset
  library(readxl)
  AirTempData <- read_excel("SummaryAirTemps.xlsx", 
                             sheet = "Sheet1")
  
  #Initializing output data frames
  year=1:14
  OutputAirD1=data.frame(year)
  OutputAirD2=data.frame(year)
  
  #Looping through Air Temp data and generating D1 (0 - 15 cm) and D2 (15 - 30 cm) average SOC
  for(x in 1:12)
  {
    for(y in 1:14)
    {
      CurrentColumn1 <- 3*x-1
      CurrentColumn2 <- 3*x
      CurrentColumn3 <- 3*x+1
      val1=AirTempData[y,CurrentColumn1]
      val2=AirTempData[y,CurrentColumn2]
      val3=AirTempData[y,CurrentColumn3]
      OutputAirD1[y,x+1] <- mean(c(val1[[1]],val2[[1]]))
      OutputAirD2[y,x+1] <- mean(c(val2[[1]],val3[[1]]))
    }
  }
  
  
  #0 - 15 cm plots SOC adj data
  {
    par(mfrow = c(2, 2))
    
    plot(OutputAirD1$year,
         OutputAirD1$V2,
         type = "p",
         col = "black",
         ylim = c(52000, 56000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD1$year,
          OutputAirD1$V6,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD1$year,
          OutputAirD1$V10,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD1$year,
         OutputAirD1$V3,
         type = "p",
         col = "black",
         ylim = c(52000, 56000),
         main="No-till, 0 N, w/ Residue",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD1$year,
          OutputAirD1$V7,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD1$year,
          OutputAirD1$V11,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD1$year,
         OutputAirD1$V4,
         type = "p",
         col = "black",
         ylim = c(52000, 56000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD1$year,
          OutputAirD1$V8,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD1$year,
          OutputAirD1$V12,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD1$year,
         OutputAirD1$V5,
         type = "p",
         col = "black",
         ylim = c(52000, 56000),
         main="No-till, 200 kg/ha N, w/ Residue",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD1$year,
          OutputAirD1$V9,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD1$year,
          OutputAirD1$V13,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
  }
  
  #15 - 30 cm plots SOC adj data
  {
    par(mfrow = c(2, 2))
    plot(OutputAirD2$year,
         OutputAirD2$V2,
         type = "p",
         col = "black",
         ylim = c(48000, 52000),
         main="No-till, 0 N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD2$year,
          OutputAirD2$V6,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD2$year,
          OutputAirD2$V10,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD2$year,
         OutputAirD2$V3,
         type = "p",
         col = "black",
         ylim = c(48000, 52000),
         main="No-till, 0 N, w/ Residue",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD2$year,
          OutputAirD2$V7,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD2$year,
          OutputAirD2$V11,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD2$year,
         OutputAirD2$V4,
         type = "p",
         col = "black",
         ylim = c(48000, 52000),
         main="No-till, 200 kg/ha N",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD2$year,
          OutputAirD2$V8,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD2$year,
          OutputAirD2$V12,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
    
    plot(OutputAirD2$year,
         OutputAirD2$V5,
         type = "p",
         col = "black",
         ylim = c(48000, 52000),
         main="No-till, 200 kg/ha N, w/ Residue",
         ylab="SOC (kg/ha)",
         xlab="year")
    lines(OutputAirD2$year,
          OutputAirD2$V9,
          type = "l",
          col = "blue",
    )
    lines(OutputAirD2$year,
          OutputAirD2$V13,
          type = "l",
          col = "red",
    )
    legend(x="bottomleft",legend=c("baseline","-2°C","+2°C"),fill=c("black","blue","red"))
  }
}