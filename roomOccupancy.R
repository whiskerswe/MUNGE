
performMungeForOccupancy<-function(){
  library(tree)
  library(ggplot2)
  #av någon anledning är inte data som behandlas som fakturor i munge faktor efter metoden. 
  
  
  updateFactor<-function(data){
    data[,6]<-as.factor(data[,6])
    return(data)
  }
  
  addColNames<-function(data){
    
    colnames(data) <- c("Temperature", "Humidity" ,"Light", "CO2", "HumidityRatio" ,"Occupancy")
    return(data)
  }

  # Ser till att all data kommer på rätt plats ta bort kolumner group och ID
  prepare<-function(data){
    data<-mixData(data)
    ndata<-data[,2:7]
    return(ndata)
  }

  singleTree<-function(trainData, testData){
    set.seed(34)
    test = testData[,1:5]
    mytree.train = tree(data=trainData, Occupancy ~  Temperature +Humidity +Light +CO2 +HumidityRatio)
    mytree.pred = predict(mytree.train, test, type="class")
    mytree.pred
    table(mytree.pred, testData[, 6])
    result<-(sum(diag(table(mytree.pred, testData[, 6]))))/nrow(testData)
    return(result)
  }  

  plotSelectedData<-function(dataA,data1, data2, data3){
    dataA<-subset(dataA[dataA$Occupancy == 1, ])
    data1<-subset(data1[data1[,6] == 1, ])
    data2<-subset(data2[data2[,6] == 1, ])
    data3<-subset(data3[data3[,6] == 1, ])
    ourdata=list(dataA$Humidity, data1[,2], data2[,2],data3[,2])
    percentDiagram(ourdata,"Room temperature where Occupancy == 1")    
  }
  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    
    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)
   
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    trainData<-updateFactor(trainData)
    testData<-updateFactor(testData)
    bagging = randomForest(formula = Occupancy ~ . , data = trainData, mtr=2)
    test = testData[,1:5]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    predicted.bagging<-as.integer(predicted.bagging)
    test<-cbind(test,predicted.bagging)
    return(test)
  }  

  bagging<-function(trainData,testData){
    set.seed(125)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = Occupancy ~ . , data =trainData, mtr=2)
    test = testData[,1:5]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
  #  print(predicted.bagging)
    x<-(sum(diag(table(predicted.bagging, testData[,6])))/(nrow(testData)))
    return(x)
  } 

  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    library(plyr)
    detailedVariance<-ddply(data, .(Occupancy), summarize,  Temperature=(var(Temperature)), Humidity=(var(Humidity)),Light=(var(Light)),CO2=(var(CO2)),  HumidityRatio=(var(HumidityRatio)))
    detailedVariance
    data<- data[order(data$Occupancy),] 
    x=sum(data$Occupancy == 0)
    y=sum(data$Occupancy == 1)
    freq<-c(x,y)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    dV<-as.data.frame(temp[,2:6])
    syntheticData<-data.frame()  
    newData<-data.frame()
    i=0
    nrSeed=321
    while (i < nrRows) {
      if(i==0){
        newData=rbind(mungeWithDetailedColumnVariance(data, syntheticData, dV, 4,(nrSeed+i)))     
      }else{
        temp<-mungeWithDetailedColumnVariance(data, syntheticData, dV, 4, (nrSeed+i))
        temp<-addColNames(temp)
        newData=rbind(newData,temp)     
      } 
      
      i=nrow(newData)
    }
    newData<-mixData(newData)
    newData<-addColNames(newData)
    return(newData[1:nrRows,])
  }

  collectData<-function(testData, trainData){
    trainData = updateFactor(trainData)
    testData = updateFactor(testData)
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-singleTree(trainData, testData)

    #Skapar syntetiska exempel  
    result1.1000<-munge1(20000, trainData)
    result1.500<-munge1(5000, trainData)
    result2.1000<-munge2(20000,trainData)
    result2.500<-munge2(5000,trainData)
    result3.1000<-munge3(20000,trainData)
    result3.500<-munge3(5000,trainData)
    
    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    result1.1000<-prepBagging(trainData, result1.1000 )
    result1.500<-prepBagging(trainData, result1.500)
    result2.1000<-prepBagging(trainData, result2.1000)
    result2.500<-prepBagging(trainData, result2.500)

    result3.1000<-prepBagging(trainData, result3.1000)
    result3.500<-prepBagging(trainData, result3.500)

    result1.1000<-as.data.frame(result1.1000)
    result2.1000<-as.data.frame(result2.1000)
    result3.1000<-as.data.frame(result3.1000)
    result1.500<-as.data.frame(result1.500)
    result2.500<-as.data.frame(result2.500)
    result3.500<-as.data.frame(result3.500)   
    result1.1000<-addColNames(result1.1000)
    result2.1000<-addColNames(result2.1000)
    result3.1000<-addColNames(result3.1000)
    result1.1000<-updateFactor(result1.1000)
    result2.1000<-updateFactor(result2.1000)
    result3.1000<-updateFactor(result3.1000)
    result1.500<-addColNames(result1.500)
    result2.500<-addColNames(result2.500)
    result3.500<-addColNames(result3.500)
    result1.500<-updateFactor(result1.500)
    result2.500<-updateFactor(result2.500)
    result3.500<-updateFactor(result3.500)

  #  print(plotSelectedData(trainData,result1.1000,result2.1000,result3.1000))
    
    result1.1000<-rbind(result1.1000, trainData)
    result2.1000<-rbind(result2.1000, trainData)
    result3.1000<-rbind(result3.1000, trainData)
    result1.500<-rbind(result1.500, trainData)
    result2.500<-rbind(result2.500, trainData)
    result3.500<-rbind(result3.500, trainData)      
    #Skapar en dataframe med resultat från träd 
    result<-data.frame()

    tempA<-c( singleTree(result1.1000, testData))
    tempB<-c( singleTree(result1.500, testData))
    temp<-c(tempB,tempA)
    
    result<-rbind(temp)
    tempA<-c( singleTree(result2.1000, testData))
    tempB<-c( singleTree(result2.500, testData))
    temp<-c(tempB,tempA)
    
    result<-rbind(result,temp)  
    tempA<-c( singleTree(result3.1000, testData))
    tempB<-c( singleTree(result3.500, testData))
    temp<-c(tempB,tempA)
    
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")
    
    
    #######################################################################################
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    print(singleTreeResult)
    print(singleBaggingResult)
    print(result)
    return(result)
    #######################################################################################
  }  
  
  
  #Dataframe med normaliserad och ihopblandad data
  getwd()
  roomOccupancy <- read.table("datasets/occupancy_data/datatraining.txt",
                             header = TRUE)
  roomOccupancy<-prepare(roomOccupancy)
  roomOccupancy<-addColNames(roomOccupancy)
  roomOccupancy<-updateFactor(roomOccupancy)
  roomOccupancy<-normaliseAndFactor(roomOccupancy)
  head(roomOccupancy)
  result<-data.frame()
  temp=data.frame()
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  ###########################################################################
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("room occupancy")
  ###########################################################################
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)
    trainData = data.frame()
    testData = data.frame()
    
    if(i==nrsIts){
      testData<-roomOccupancy[start:nrow(roomOccupancy),]      
    }else{
      testData<-roomOccupancy[start:(start+814),]      
    }
    #ta den delen som inte är train och gör till testdata
    if(i==nrsIts){
      trainData<-roomOccupancy[1:(start-1),]
    }
    else if(i>1){
      trainData<-roomOccupancy[1:(start-1),]
      trainData<-rbind(trainData,roomOccupancy[(start+814):nrow(roomOccupancy),] )
    }else{
      trainData<-roomOccupancy[(start+815):nrow(roomOccupancy),]
    }
    ###############################################################################
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]
    
    start=start+814
    print(nrow(trainData))
    print(nrow(testData))
    print("-------------------------------------------------------------------")
    tVector<-c(temp[1,],temp[2,],temp[3,],temp.tree,temp.bagging)
    if(i==1){
      result=temp
      allResults<-rbind(tVector)
      bagg=temp.bagging
      tree=temp.tree
    }else if(i>1){
      j=1
      while(j<=ncol(temp)){
        result[,j]=result[,j]+temp[,j]
        j=j+1
      }
      bagg=bagg+temp.bagging
      tree=tree+temp.tree
      allResults<-rbind(allResults,tVector)
    }
    ###################################################################################
    i=i+1
  } 
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)
  
  allResultsDiagram(allResults,"Room occupancy result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average Room Occupancy")
  
  colnames(allResults) <- c("M1:5000","M2:20000","M2:5000","M2,20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
}

performMungeForOccupancy()


