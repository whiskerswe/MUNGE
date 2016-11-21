#native country används ej

performMungeBanknote<-function(){
  library(tree)
  library(ggplot2)
  
  updateFactor<-function(data){
    data[,5]<-as.numeric(data[,5])
    data[,5]<-as.factor(data[,5])
    return(data)
  }
  
  addColNames<-function(data){
    colnames(data) <- c("variance","skewness", "curtosis","entropy", "class")
    return(data)
  }
  
  prepare<-function(data){
    data<-mixData(data)
    data<-addColNames(data)
    data<-updateFactor(data)
    return(data)
  }
  
  singleTree<-function(trainData, testData){
    set.seed(9898)
    trainData<-as.data.frame(trainData)
    trainData<-addColNames(trainData)
    trainData<-updateFactor(trainData)
    test = testData[,1:4]
    mytree.train = tree(data=trainData, class ~  variance + skewness + entropy + curtosis)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    result<-(sum(diag(table(mytree.pred, testData[, 5]))))/nrow(testData)
    return(result)
  }  
  
  plotSelectedData<-function(dA, d1, d2, d3){
    dA<-subset(dA[dA$class == 1, ])
    d1<-subset(d1[d1[,5] == 1, ])
    d2<-subset(d2[d2[,5] == 1, ])
    d3<-subset(d3[d3[,5] == 1, ])
    ourdata=list(dA$variance, d1[,1], d2[,1],d3[,1])
    percentDiagram(ourdata,"Banknote variance where class == 1")    
  }
  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)
    set.seed(127)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = class ~ . , data = trainData, mtr=2)
    test = testData[,1:4]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(test)
  }  
  #Returnerar resultat av bagging 
  bagging<-function(trainData,testData){
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = class ~ . , data =trainData, mtr=2)
    test = testData[,1:4]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,5])))/(nrow(testData)))
    return(x)
  } 
  
  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr)
    detailedVariance<-ddply(data, .(class), summarize, variance=(var(variance)), skewness=(var(skewness)), curtosis=(var(curtosis)), entropy=(var(entropy)))
    detailedVariance
    head(data)
    data2<- data[order(data$class),] 
    a=sum(data$class == 1)
    b=sum(data$class == 2)

    freq<-c(a, b)
    
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    head(temp)
    dV<-as.data.frame(temp[,2:5])
    
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
    head(newData)
    newData<-mixData(newData)
    return(newData[1:nrRows,])
  }
  
  collectData<-function(testData, trainData){
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-singleTree(trainData, testData)
    #Skapar syntetiska exempel  
    result1.2000<-munge1(20000, trainData)
    result1.500<-munge1(5000, trainData)
    result2.2000<-munge2(20000,trainData)
    result2.500<-munge2(5000,trainData)
    result3.2000<-munge3(20000,trainData)
    result3.500<-munge3(5000,trainData)
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    
    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    result1.2000<-prepBagging(trainData, result1.2000 )
    result1.500<-prepBagging(trainData, result1.500)
    result2.2000<-prepBagging(trainData, result2.2000)
    result2.500<-prepBagging(trainData, result2.500)
    result3.2000<-prepBagging(trainData, result3.2000)
    result3.500<-prepBagging(trainData, result3.500)
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    
    result2.2000<-as.data.frame(result2.2000)
    result3.2000<-as.data.frame(result3.2000)
    result2.500<-as.data.frame(result2.500)
    result3.500<-as.data.frame(result3.500)
    result1.2000<-addColNames(result1.2000)
    result2.2000<-addColNames(result2.2000)
    result3.2000<-addColNames(result3.2000)
    result1.2000<-updateFactor(result1.2000)
    result2.2000<-updateFactor(result2.2000)
    result3.2000<-updateFactor(result3.2000)
    result1.500<-addColNames(result1.500)
    result2.500<-addColNames(result2.500)
    result3.500<-addColNames(result3.500)
    result1.500<-updateFactor(result1.500)
    result2.500<-updateFactor(result2.500)
    result3.500<-updateFactor(result3.500)
    
   # print(plotSelectedData(trainData,result1.2000,result2.2000,result3.2000))
    result1.2000<-rbind(result1.2000, trainData)
    result2.2000<-rbind(result2.2000, trainData)
    result3.2000<-rbind(result3.2000, trainData)
    result1.500<-rbind(result1.500, trainData)
    result2.500<-rbind(result2.500, trainData)
    result3.500<-rbind(result3.500, trainData)    
    
    result<-data.frame()
    
    tempA<-c( singleTree(result1.2000, testData))
    tempB<-c( singleTree(result1.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(temp)
    tempA<-c( singleTree(result2.2000, testData))
    tempB<-c( singleTree(result2.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    tempA<-c( singleTree(result3.2000, testData))
    tempB<-c( singleTree(result3.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    return(result)
  }  

  #Dataframe med normaliserad och ihopblandad data

  getwd()
  data1 <- read.table("C:/home/datasets/banknote.txt",
                      header = FALSE)
  head(data1)
  
  data<-prepare(data1)
  data<-updateFactor(data)
  data<-normaliseAndFactor(data)
  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("banknote")
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)
    if(i==nrsIts){
      testData<-data[start:nrow(data),]      
    }else{
      testData<-data[start:(start+137),]      
    }
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+137):nrow(data),] )
    }else{
      trainData<-data[(start+138):nrow(data),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    start=start+138
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
  
  allResultsDiagram(allResults,"Banknote result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average banknote")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
  
  
}
performMungeBanknote()

