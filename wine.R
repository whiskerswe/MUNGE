#fixedAcidity och volatileAcidity används ej


performMungeForWine<-function(){
  library(tree)
  library(ggplot2)

  updateFactor<-function(data){
    data[,8]<-as.numeric(data[,8])
    data[,10]<-as.factor(data[,10])
    return(data)
  }

  addColNames<-function(aData){
    colnames(aData) <- c("citricAcid","residualSugar","chlorides", 
                                 "freeSulfurDioxide","totalSulfurDioxide", "density", "pH","sulphates","alcohol","quality")
    return(aData)
  }

  preparedWine<-function(data){
    data<-mixData(data)
    aData<-data[,3:12]  
    return(aData)
  }

  singleTree<-function(trainData, testData){
    set.seed(9889)
    testData <-updateFactor(testData)
    trainData <- updateFactor(trainData)
    test = testData[,1:9]
    mytree.train = tree(data=trainData, quality ~  citricAcid +residualSugar +chlorides  +freeSulfurDioxide +totalSulfurDioxide  +density +pH +sulphates)  
    mytree.pred = predict(mytree.train, test, type="class")
    mytree.pred
    table(mytree.pred, testData[, 10])
    result<-(sum(diag(table(mytree.pred, testData[, 10]))))/nrow(testData)
    return(result)
  }  

  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = quality ~ . , data = trainData, mtr=2)
    test = testData[,1:9]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(test)
  }  

  
  bagging<-function(trainData,testData){
    set.seed(125)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = quality ~ . , data =trainData, mtr=2)
    test = testData[,1:9]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,10])))/(nrow(testData)))
    return(x)
  } 
  
  plotSelectedData<-function(dataA,data1, data2, data3){
    dataA<-subset(dataA[dataA$quality == 6, ])
    data1<-subset(data1[data1[,10] == 6, ])
    data2<-subset(data2[data2[,10] == 6, ])
    data3<-subset(data3[data3[,10] == 6, ])
    ourdata=list(dataA$alcohol, data1[,9], data2[,9],data3[,9])
    print (percentDiagram(ourdata,"Quality 6 and alcohol percent"))    
  }
  
  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr) 
    detailedVariance<-ddply(data, .(quality), summarize,  citricAcid =(var(citricAcid)), residualSugar =(var(residualSugar)),chlorides =(var(chlorides)),freeSulfurDioxide =(var(freeSulfurDioxide)),  totalSulfurDioxide =(var(totalSulfurDioxide)), density=(var(density)), pH=(var(pH)), sulphates =(var(sulphates)), alcohol=(var(alcohol)))
    head(detailedVariance)
    head(data)
    data<- data[order(data$quality),] 
    a=sum(data$quality == 1)
    b=sum(data$quality == 2)
    c=sum(data$quality == 3)
    d=sum(data$quality == 4)
    e=sum(data$quality == 5)
    f=sum(data$quality == 6)
    g=sum(data$quality == 7)

    freq<-c(a,b,c,d,e,f,g)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    head(temp)
    dV<-as.data.frame(temp[,2:10])
    head(dV)
    syntheticData<-data.frame()  
    newData<-data.frame()
    i=0
    nrSeed=321
    data<-updateFactor(data)
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
    wineResult1.1000<-munge1(20000, trainData)
    wineResult1.500<-munge1(5000, trainData)
    wineResult2.1000<-munge2(20000,trainData)
    wineResult2.500<-munge2(5000,trainData)
    wineResult3.1000<-munge3(20000,trainData)
    wineResult3.500<-munge3(5000,trainData)
    
    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    wineResult1.1000<-prepBagging(trainData, wineResult1.1000)
    wineResult1.500<-prepBagging(trainData, wineResult1.500)
    wineResult2.1000<-prepBagging(trainData, wineResult2.1000)
    wineResult2.500<-prepBagging(trainData, wineResult2.500)
    wineResult3.1000<-prepBagging(trainData, wineResult3.1000)
    wineResult3.500<-prepBagging(trainData, wineResult3.500)
    
    wineResult1.1000<-addColNames(wineResult1.1000)
    wineResult2.1000<-addColNames(wineResult2.1000)
    wineResult3.1000<-addColNames(wineResult3.1000)
    wineResult1.1000<-updateFactor(wineResult1.1000)
    wineResult2.1000<-updateFactor(wineResult2.1000)
    wineResult3.1000<-updateFactor(wineResult3.1000)
    wineResult1.500<-addColNames(wineResult1.500)
    wineResult2.500<-addColNames(wineResult2.500)
    wineResult3.500<-addColNames(wineResult3.500)
    wineResult1.500<-updateFactor(wineResult1.500)
    wineResult2.500<-updateFactor(wineResult2.500)
    wineResult3.500<-updateFactor(wineResult3.500)
    
#    print(plotSelectedData(trainData, wineResult1.500,wineResult2.500,wineResult3.500))
    
    wineResult1.1000<-rbind(wineResult1.1000, trainData)
    wineResult2.1000<-rbind(wineResult2.1000, trainData)
    wineResult3.1000<-rbind(wineResult3.1000, trainData)
    wineResult1.500<-rbind(wineResult1.500, trainData)
    wineResult2.500<-rbind(wineResult2.500, trainData)
    wineResult3.500<-rbind(wineResult3.500, trainData)    
    
    
    #Skapar en dataframe med resultat från träd 
    result<-data.frame()
    head(wineResult3.1000)
    tempA<-c( singleTree(wineResult1.1000, testData))
    tempB<-c( singleTree(wineResult1.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(temp)
    tempA<-c( singleTree(wineResult2.1000, testData))
    tempB<-c( singleTree(wineResult2.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    tempA<-c( singleTree(wineResult3.1000, testData))
    tempB<-c( singleTree(wineResult3.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")

    #######################################################################################
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    return(result)
    #######################################################################################
  }  
  
  
  #Dataframe med normaliserad och ihopblandad data
  getwd()
  wine <- read.table("datasets/wine.txt",
                             header = TRUE)
  #head(wine)
  wine<-preparedWine(wine)
  wine<-addColNames(wine)
  wine<-updateFactor(wine)
  wine<-normaliseAndFactor(wine)
  head(wine)
  #data=wine
  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("wine")
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)

    trainData = data.frame()
    testData = data.frame()
    if(i==nrsIts){
      testData<-wine[start:nrow(wine),]      
    }else{
      testData<-wine[start:(start+489),]      
    }
    #ta den delen som inte är train och gör till testdata
    
    if(i==nrsIts){
      trainData<-wine[1:(start-1),]
    }
    else if(i>1){
      trainData<-wine[1:(start-1),]
      trainData<-rbind(trainData,wine[(start+489):nrow(wine),] )
    }else{
      trainData<-wine[(start+490):nrow(wine),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    start=start+490
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]
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
    i=i+1
  } 
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)
  
  allResultsDiagram(allResults,"Wine result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average wine")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
  
  
}

performMungeForWine()


