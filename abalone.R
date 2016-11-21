#native country används ej

performMungeForAbalone<-function(){
  library(tree)
  library(ggplot2)
  #av någon anledning är inte data som behandlas som fakturor i munge faktor efter metoden. 
  updateFactor<-function(data){
    data[,1]<-as.factor(data[,1])
    data[,9]<-as.factor(data[,9])
    return(data)
  }
  addColNames<-function(aData){
    
    colnames(aData) <- c("Sex","Length","Diameter","Height","Wholeweight","Shuckedweight","Visceraweight","Shellweight","Rings")
    return(aData)
  }
  
  prepareData<-function(data){
    data<-mixData(data)
    return(data)
  }
  
  singleTree<-function(trainData, testData){
    set.seed(9889)
    testData<-updateFactor(testData)
    trainData<-updateFactor(trainData)
    test = testData[,1:8]
    mytree.train = tree(data=trainData, Rings ~  Sex +Length+ Diameter +Height +Wholeweight +Shuckedweight + Visceraweight+Shellweight)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    mytree.pred
    table(mytree.pred, testData[, 9])
    result<-(sum(diag(table(mytree.pred, testData[, 9]))))/nrow(testData)
    return(result)
  }  
  
  plotSelectedData<-function(dataA,data1, data2, data3){
    dataA<-subset(dataA[dataA$Rings == 1, ])
    data1<-subset(data1[data1[,9] == 1, ])
    data2<-subset(data2[data2[,9] == 1, ])
    data3<-subset(data3[data3[,9] == 1, ])
    ourdata=list(dataA$Wholeweight, data1[,5], data2[,5],data3[,5])
    print(percentDiagram(ourdata,"Abalone Whole weight where Rings = 5 "))    
  }
  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = Rings ~ . , data = trainData, mtr=2)
    test = testData[,1:8]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(test)
  }  
  #Returnerar resultat av bagging 
  bagging<-function(trainData,testData){
    set.seed(125)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = Rings ~ . , data =trainData, mtr=2)
    test = testData[,1:8]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,9])))/(nrow(testData)))
    return(x)
  } 
  
  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr) 
    detailedVariance<-ddply(data, .(Rings), summarize, Length=(var(Length)), Diameter=(var(Diameter)),Height=(var(Height)),Wholeweight=(var(Wholeweight)), Shuckedweight=(var(Shuckedweight)), Visceraweight=(var(Visceraweight)), Shellweight=(var(Shellweight)))
    head(detailedVariance)
    head(data)
    data<- data[order(data$Rings),] 
    a=sum(data$Rings == 1)
    b=sum(data$Rings == 2)
    c=sum(data$Rings == 3)
    d=sum(data$Rings == 4)
    e=sum(data$Rings == 5)
    f=sum(data$Rings == 6)
    g=sum(data$Rings == 7)
    h=sum(data$Rings == 8)
    i=sum(data$Rings == 9)
    j=sum(data$Rings == 10)
    k=sum(data$Rings == 11)
    l=sum(data$Rings == 12)
    freq<-c(a,b,c,d,e,f,g,h,i,j,k,l)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    head(temp)
    dV<-as.data.frame(temp[,2:8])
    head(dV)
    syntheticData<-data.frame()  
    newData<-data.frame()
    i=0
    nrSeed=321
    data<-updateFactor(data)
    while (i < nrRows) {
      #  (trainingEx, syntheticData, dV,p)
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
  munge1<-function(nrRows, data){
    newData<-data.frame()
    i=0
    variance<-getOldVariance(data)
    while (i< nrRows) {
      if(i==0){
        newData=rbind(munge(data, syntheticData, variance, 4, (123+i)))     
      }else{
        newData=rbind(newData,munge(data, syntheticData, variance, 4,(123+i)))     
      } 
      i=nrow(newData)
    }
    return(newData[1:nrRows,])
  }
  munge2<-function(nrEx, data){
    variancePerColumn<-getVariancePerKolumn(data)
    newData<-data.frame()
    i=0
    seedNr=1234
    while (i < nrEx) {
      if(i==0){
        newData=rbind(mungeWithColumnVariance(data, syntheticData, variancePerColumn, 4,(seedNr+i)))     
      }else{
        newData=rbind(newData,mungeWithColumnVariance(data, syntheticData, variancePerColumn, 4, (seedNr+i)))     
      } 
      i=nrow(newData)
    }
    return(newData[1:nrEx,])
  }
  collectData<-function(testData, trainData){
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-singleTree(trainData, testData)
    #Skapar syntetiska exempel  
    result1.1000<-munge1(5000, trainData)
    result1.2000<-munge1(20000, trainData)
    result2.1000<-munge2(5000,trainData)
    result2.2000<-munge2(20000,trainData)
    result3.1000<-munge3(5000, trainData)
    result3.2000<-munge3(20000, trainData)

    
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    result1.1000<-as.data.frame(result1.1000)
    result2.1000<-as.data.frame(result2.1000)
    result3.1000<-as.data.frame(result3.1000)
    result1.1000<-addColNames(result1.1000)
    result2.1000<-addColNames(result2.1000)
    result3.1000<-addColNames(result3.1000)
    result1.1000<-updateFactor(result1.1000)
    result2.1000<-updateFactor(result2.1000)
    result3.1000<-updateFactor(result3.1000)
    result1.2000<-as.data.frame(result1.2000)
    result2.2000<-as.data.frame(result2.2000)
    result3.2000<-as.data.frame(result3.2000)
    result1.2000<-addColNames(result1.2000)
    result2.2000<-addColNames(result2.2000)
    result3.2000<-addColNames(result3.2000)
    result1.2000<-updateFactor(result1.2000)
    result2.2000<-updateFactor(result2.2000)
    result3.2000<-updateFactor(result3.2000)   
    
    result1.1000<-prepBagging(result1.1000, trainData)
    result1.2000<-prepBagging(result1.2000, trainData)
    result2.1000<-prepBagging(result2.1000, trainData)
    result2.2000<-prepBagging(result2.2000, trainData)
    result3.1000<-prepBagging(result3.1000, trainData)
    result3.2000<-prepBagging(result3.2000, trainData)

    result1.1000<-as.data.frame(result1.1000)
    result2.1000<-as.data.frame(result2.1000)
    result3.1000<-as.data.frame(result3.1000)
    result1.1000<-addColNames(result1.1000)
    result2.1000<-addColNames(result2.1000)
    result3.1000<-addColNames(result3.1000)
    result1.1000<-updateFactor(result1.1000)
    result2.1000<-updateFactor(result2.1000)
    result3.1000<-updateFactor(result3.1000)
    result1.2000<-as.data.frame(result1.2000)
    result2.2000<-as.data.frame(result2.2000)
    result3.2000<-as.data.frame(result3.2000)
    result1.2000<-addColNames(result1.2000)
    result2.2000<-addColNames(result2.2000)
    result3.2000<-addColNames(result3.2000)
    result1.2000<-updateFactor(result1.2000)
    result2.2000<-updateFactor(result2.2000)
    result3.2000<-updateFactor(result3.2000) 
    
    print(plotSelectedData(trainData,result1.1000,result2.1000,result3.1000))
    
    result1.1000<-rbind(result1.1000, trainData)
    result1.2000<-rbind(result1.2000, trainData)
    result2.1000<-rbind(result2.1000, trainData)
    result2.2000<-rbind(result2.2000, trainData)
    result3.1000<-rbind(result3.1000, trainData)
    result3.2000<-rbind(result3.2000, trainData)
    #Skapar en dataframe med resultat från träd 
    result<-data.frame()
    tempA<-c( singleTree(result1.1000, testData))
    tempB<-c( singleTree(result1.2000, testData))
    temp<-c(tempA,tempB)
    result<-rbind(temp)
    tempA<-c( singleTree(result2.1000, testData))
    tempB<-c( singleTree(result2.2000, testData))
    temp<-c(tempA,tempB)
    result<-rbind(result,temp)  
    tempA<-c( singleTree(result3.1000, testData))
    tempB<-c( singleTree(result3.2000, testData))
    temp<-c(tempA,tempB)
    result<-rbind(result,temp)
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")
    

    
    #######################################################################################
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    return(result)
    
  }  
  #Dataframe med normaliserad och ihopblandad data
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
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  while(i<=nrsIts){
    print("Iteration: ")
    print(i)
    trainData = data.frame()
    testData = data.frame()
    if(i==nrsIts){
      testData<-data[start:nrow(data),]      
    }else{
      testData<-data[start:(start+390),]      
    }
    #ta den delen som inte är train och gör till testdata

    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+390):nrow(data),] )
    }else{
      trainData<-data[(start+391):nrow(data),]
    }
    print(nrow(trainData))
    print(nrow(testData))
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
    }else{
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
    start=start+391
  } 
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)
  
  allResultsDiagram(allResults,"Result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average abalone")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2,20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
}
performMungeForAbalone()
