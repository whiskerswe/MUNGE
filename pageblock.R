#native country används ej

performMungePageBlock<-function(){
  library(tree)
  library(ggplot2)

  updateFactor<-function(data){
    data[,11]<-as.factor(data[,11])
    return(data)
  }
  
  addColNames<-function(data){
    colnames(data) <- c("height","lenght","area",
                              "eccen","p_black", "p_and","mean_tr", "blackpix", "blackand","wb_trans","category")
    return(data)
  }
  
  prepare<-function(data){
    data<-mixData(data)
    return(data)
  }
  
  singleTree<-function(trainData, testData){
    set.seed(9898)
    trainData<-as.data.frame(trainData)
    trainData<-addColNames(trainData)
    trainData<-updateFactor(trainData)
    test = testData[,1:10]
    mytree.train = tree(data=trainData, category ~  height + lenght + area + eccen + p_black + p_and + mean_tr + blackpix + blackand + wb_trans)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    mytree.pred
    table(mytree.pred, testData[, 11])
    result<-(sum(diag(table(mytree.pred, testData[, 11]))))/nrow(testData)
    return(result)
  }  
  
  plotSelectedData<-function(dA, d1, d2, d3){
    dA<-subset(dA[dA$category == 1, ])
    d1<-subset(d1[d1[,11] == 1, ])
    d2<-subset(d2[d2[,11] == 1, ])
    d3<-subset(d3[d3[,11] == 1, ])
    ourdata=list(dA$height, d1[,1], d2[,1],d3[,1])
    percentDiagram(ourdata,"Page block height where block == text")    
  }
  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)
    set.seed(127)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = category ~ . , data = trainData, mtr=2)
    test = testData[,1:10]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(test)
  }  
  #Returnerar resultat av bagging 
  bagging<-function(trainData,testData){
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = category ~ . , data =trainData, mtr=2)
    test = testData[,1:10]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,11])))/(nrow(testData)))
    return(x)
  } 
  
  munge3<-function(nrRows,data){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr)
    detailedVariance<-ddply(data, .(category), summarize,  height=(var(height)), lenght=(var(lenght)), area=(var(area)), eccen=(var(eccen)), p_black=(var(p_black)), p_and=(var(p_and)), mean_tr=(var(mean_tr)), blackpix=(var(blackpix)), blackand=(var(blackand)), wb_trans=(var(wb_trans)))
    detailedVariance
    head(data)
    data2<- data[order(data$category),] 
    a=sum(data$category == 1)
    b=sum(data$category == 2)
    c=sum(data$category == 3)
    d=sum(data$category == 4)
    e=sum(data$category == 5)

    freq<-c(a, b, c, d, e)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    head(temp)
    dV<-as.data.frame(temp[,2:11])

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
    data<-mixData(data)
    newData<-mixData(newData)
    newData<-addColNames(newData)
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
 
    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    result1.1000<-prepBagging(trainData, result1.1000 )
    result1.500<-prepBagging(trainData, result1.500)
    result2.1000<-prepBagging(trainData, result2.1000)
    result2.500<-prepBagging(trainData, result2.500)
    result3.1000<-prepBagging(trainData, result3.1000)
    result3.500<-prepBagging(trainData, result3.500)
 
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
    print(singleTreeResult)
    print(singleBaggingResult)
    return(result)
  }  
  
  
  #Dataframe med normaliserad och ihopblandad data
  getwd()
  page.block <- read.table("datasets/page-blocks.data",
                          header = FALSE)
  page.block<-updateFactor(page.block)
  page.block<-prepare(page.block)
  page.block<-addColNames(page.block)
  page.block<-normaliseAndFactor(page.block)
  #data=page.block
  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("pageblock")
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)
    if(i==nrsIts){
      testData<-page.block[start:nrow(page.block),]      
    }else{
      testData<-page.block[start:(start+547),]      
    }
    if(i==nrsIts){
      trainData<-page.block[1:(start-1),]
    }
    else if(i>1){
      trainData<-page.block[1:(start-1),]
      trainData<-rbind(trainData,page.block[(start+547):nrow(page.block),] )
    }else{
      trainData<-page.block[(start+547):nrow(page.block),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    start=start+547
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]
    print("-------------------------------------------------------------------")
    tVector<-c(temp[1,],temp[2,],temp[3,],temp.tree,temp.bagging)
    if(i==1){
      print("i är 1")
      result=temp
      allResults<-rbind(tVector)
      bagg=temp.bagging
      tree=temp.tree
    }else if(i>1){
      print("i större än 1")
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
  
  allResultsDiagram(allResults,"Result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average page block")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
  
  
}
performMungePageBlock()

