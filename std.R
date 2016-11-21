

abaloneVar<-function(){
  library(plyr) 
  getwd()
  data <- read.table("datasets/abalone.txt",
                     header = FALSE)
  data<-prepareData(data)
  data<-addColNames(data)
  data<-updateFactor(data)
  data[,9]<-as.numeric(data[,9])
  x <- subset(data, Rings >= 5 & Rings <= 16)
  data<-data.frame()
  data<-updateFactor(x)
  data<-normaliseAndFactor(data)
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
   # print("Iteration: ")
   # print(i)
    trainData = data.frame()
    testData = data.frame()
    if(i==nrsIts){
      testData<-data[start:nrow(data),]      
    }else{
      testData<-data[start:(start+390),]      
    }
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+390):nrow(data),] )
    }else{
      trainData<-data[(start+391):nrow(data),]
    }
    t<-data.frame()
  #  print(nrow(trainData))
  #  print(start)
    t<-ddply(trainData, .(Rings), summarize, Sex=(var(Sex)),Length=(var(Length)), Diameter=(var(Diameter)),Height=(var(Height)),Wholeweight=(var(Wholeweight)), Shuckedweight=(var(Shuckedweight)), Visceraweight=(var(Visceraweight)), Shellweight=(var(Shellweight)))
    vpk<-getVariancePerKolumn(trainData)
       if(i==1){
      allVar=t
      colVar=rbind(vpk)
      print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    oldVar=oldVar+getOldVariance(trainData)
  #  print(oldVar)
    i=i+1
    start=start+390
  }
 # print(allVar)
 print(allVar/10)
 #print(colVar)

# print(oldVar)
 # print(oldVar/10)
}
abaloneVar()


adultVar<-function(){
  library(plyr) 
  getwd()
  data <- read.table("datasets/adult.data.txt",
                     header = FALSE)
  data<-preparedAdult(data)
  data<-addColNames(data)
  data<-normaliseAndFactor(data)
  

  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
  #   print("Iteration: ")
   #  print(i)
    trainData = data.frame()
    testData = data.frame()

    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+3256):nrow(data),] )
    }else{
      trainData<-data[(start+3256):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
      #print(start)
    t<-detailedVariance<-ddply(trainData, .(income), summarize,  age=(var(age)), fnlwgt=(var(fnlwgt)),educationnum=(var(educationnum)),capitalgain=(var(capitalgain)), capitalloss=(var(capitalloss)), hoursperweek=(var(hoursperweek)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
   #   print(t)
      colVar=rbind(vpk)
     # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+3256
  }
   # print(allVar)
   #print(allVar/10)
  print(colVar)
  
#  print(oldVar)
  print(oldVar/10)
}
adultVar()



adultWilt<-function(){
  getwd()
  data1 <- read.table("datasets/wilt/training.csv",
                      header = TRUE)
  head(data)
  
  data<-prepare(data1)
  data<-updateFactor(data)
  
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
    #   print("Iteration: ")
    #  print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+433):nrow(data),] )
    }else{
      trainData<-data[(start+434):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
    #print(start)
    t<-detailedVariance<-ddply(trainData, .(class), summarize, GLCM_pan=(var(GLCM_pan)), Mean_Green=(var(Mean_Green)), Mean_Red=(var(Mean_Red)), Mean_NIR=(var(Mean_NIR)), SD_Pan=(var(SD_Pan)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
      #   print(t)
      colVar=rbind(vpk)
      # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+434
  }
  print(allVar)
  print(allVar/10)
  # print(colVar)
  
  #  print(oldVar)
  # print(oldVar/10)
}
adultWilt()


wineVar<-function(){
  getwd()
  data <- read.table("datasets/wine.txt",
                     header = TRUE)
  #head(wine)
  data<-preparedWine(data)
  data<-addColNames(data)
  data<-updateFactor(data)
  data<-normaliseAndFactor(data)
  head(data)
  
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
    #   print("Iteration: ")
    #  print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+489):nrow(data),] )
    }else{
      trainData<-data[(start+490):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
    #print(start)
    t<-detailedVariance<-ddply(trainData, .(quality), summarize,  citricAcid =(var(citricAcid)), residualSugar =(var(residualSugar)),chlorides =(var(chlorides)),freeSulfurDioxide =(var(freeSulfurDioxide)),  totalSulfurDioxide =(var(totalSulfurDioxide)), density=(var(density)), pH=(var(pH)), sulphates =(var(sulphates)), alcohol=(var(alcohol)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
      #   print(t)
      colVar=rbind(vpk)
      # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+490
  }
  #   print(allVar)
  # print(allVar/10)
  print(colVar)
  
  #  print(oldVar)
  print(oldVar/10)
}
wineVar()


varRooms<-function(){
  getwd()
  roomOccupancy <- read.table("datasets/occupancy_data/datatraining.txt",
                              header = TRUE)
  roomOccupancy<-prepare(roomOccupancy)
  roomOccupancy<-addColNames(roomOccupancy)
  roomOccupancy<-updateFactor(roomOccupancy)
  data<-normaliseAndFactor(roomOccupancy)
  head(data)
  
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
    #   print("Iteration: ")
    #  print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+814):nrow(data),] )
    }else{
      trainData<-data[(start+815):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
    #print(start)
    t<-detailedVariance<-ddply(trainData, .(Occupancy), summarize,  Temperature=(var(Temperature)), Humidity=(var(Humidity)),Light=(var(Light)),CO2=(var(CO2)),  HumidityRatio=(var(HumidityRatio)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
      #   print(t)
      colVar=rbind(vpk)
      # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+815
  }
  #   print(allVar)
   print(allVar/10)
 # print(colVar)
  
  #  print(oldVar)
  print(oldVar/10)
}
varRooms()


varIris<-function(){
  preparedIris<-prepareIris()
  data<-mixData(preparedIris)
  library(plyr)
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
    #   print("Iteration: ")
    #  print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+15):nrow(data),] )
    }else{
      trainData<-data[(start+15):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
    #print(start)
    t<-detailedVariance<-ddply(trainData, .(Species), summarize,  Sepal.Length=(var(Sepal.Length)), Sepal.Width=(var(Sepal.Width)),Petal.Length=(var(Petal.Length)), Petal.Width=(var(Petal.Width)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
      #   print(t)
      colVar=rbind(vpk)
      # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+15
  }
  #   print(allVar)
  print(allVar/10)
  print(colVar)
  
  #  print(oldVar)
  print(oldVar/10)
}
varIris()

varYeast<-function(){
  getwd()
  data1 <- read.table("datasets/yeast.txt",
                      header = FALSE)
  
  
  data<-prepare(data1)
  data<-updateFactor(data)
  data<-normaliseAndFactor(data)
  library(plyr)
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  nrsIts=10
  allVar<-data.frame()
  colVar=data.frame()
  oldVar=0
  while(i<=nrsIts){
    #   print("Iteration: ")
    #  print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+139):nrow(data),] )
    }else{
      trainData<-data[(start+140):nrow(data),]
    }
    t<-data.frame()
    #  print(nrow(trainData))
    #print(start)
    t<-detailedVariance<-ddply(trainData, .(class), summarize,  mcg=(var(mcg)), gvh=(var(gvh)), alm=(var(alm)), mit=(var(mit)), vac=(var(vac)), nuc=(var(nuc)))
    vpk<-getVariancePerKolumn(trainData)
    if(i==1){
      allVar=t
      #   print(t)
      colVar=rbind(vpk)
      # print(colVar)
    }else{
      allVar=allVar+t
      colVar=rbind(colVar, vpk)
    }
    head(trainData)
    oldVar=oldVar+getOldVariance(trainData)
    #  print(oldVar)
    i=i+1
    start=start+140
  }
  #   print(allVar)
  print(allVar/10)
  print(colVar)
  
  #  print(oldVar)
  print(oldVar/10)
}
varYeast()


