#native country används ej

performMungewilt<-function(){
  library(tree)
  library(ggplot2)
  
  updateFactor<-function(data){
    data[,6]<-as.numeric(data[,6])
    data[,6]<-as.factor(data[,6])
    return(data)
  }
  
  addColNames<-function(data){
    colnames(data) <- c("GLCM_pan","Mean_Green", "Mean_Red","Mean_NIR", "SD_Pan","class")
    return(data)
  }
  
  prepare<-function(data1){
    data<-data.frame()
    data<-cbind(data1[,2:6])
    data<-cbind(data, data1[,1])
    data<-mixData(data)
    data<-addColNames(data)
    data<-normaliseAndFactor(data)
    return(data)
  }
  
  singleTree<-function(trainData, testData){
    set.seed(9898)
    trainData<-as.data.frame(trainData)
    trainData<-addColNames(trainData)
    trainData<-updateFactor(trainData)
    test = testData[,1:5]
    mytree.train = tree(data=trainData, class ~  GLCM_pan + Mean_Green + Mean_Red + Mean_NIR+ SD_Pan)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    result<-(sum(diag(table(mytree.pred, testData[, 6]))))/nrow(testData)
    return(result)
  }  
  
  plotSelectedData<-function(dA, d1, d2, d3){
    dA<-subset(dA[dA$class == 1, ])
    d1<-subset(d1[d1[,6] == 1, ])
    d2<-subset(d2[d2[,6] == 1, ])
    d3<-subset(d3[d3[,6] == 1, ])
    ourdata=list(dA$GLCM_pan, d1[,1], d2[,1],d3[,1])
    percentDiagram(ourdata,"GLCM plan in wilted trees")    
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
    test = testData[,1:5]
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
    test = testData[,1:5]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,6])))/(nrow(testData)))
    return(x)
  } 
  
  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr)
    detailedVariance<-ddply(data, .(class), summarize, GLCM_pan=(var(GLCM_pan)), Mean_Green=(var(Mean_Green)), Mean_Red=(var(Mean_Red)), Mean_NIR=(var(Mean_NIR)), SD_Pan=(var(SD_Pan)))
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
    dV<-as.data.frame(temp[,2:6])
    
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
    result1.1000<-munge1(20000, trainData)
    result1.500<-munge1(5000, trainData)
    result2.1000<-munge2(20000,trainData)
    result2.500<-munge2(5000,trainData)
    result3.1000<-munge3(20000,trainData)
    result3.500<-munge3(5000,trainData)
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    result1.1000<-prepBagging(trainData, result1.1000)
    result1.500<-prepBagging(trainData, result1.500)
    result2.1000<-prepBagging(trainData, result2.1000)
    result2.500<-prepBagging(trainData, result2.500)
    result3.1000<-prepBagging(trainData, result3.1000)
    result3.500<-prepBagging(trainData, result3.500)
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    
    result2.1000<-as.data.frame(result2.1000)
    result3.1000<-as.data.frame(result3.1000)
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
    
   # print(plotSelectedData(trainData,result1.1000,result2.1000,result3.1000))
    result1.1000<-rbind(result1.1000, trainData)
    result2.1000<-rbind(result2.1000, trainData)
    result3.1000<-rbind(result3.1000, trainData)
    result1.500<-rbind(result1.500, trainData)
    result2.500<-rbind(result2.500, trainData)
    result3.500<-rbind(result3.500, trainData)    
    
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
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    return(result)
  }  
  
  #Dataframe med normaliserad och ihopblandad data
  
  getwd()
  data1 <- read.table("datasets/wilt/training.csv",
                      header = TRUE)
  head(data)
  
  data<-prepare(data1)
  data<-updateFactor(data)
  
  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("Wilt")
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)
    if(i==nrsIts){
      testData<-data[start:nrow(data),]      
    }else{
      testData<-data[start:(start+433),]      
    }
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+433):nrow(data),] )
    }else{
      trainData<-data[(start+434):nrow(data),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    start=start+434
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
  
  allResultsDiagram(allResults,"Wilt result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average wilt")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
  
  
}
performMungewilt()



