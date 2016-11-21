
#normalisera nummer. Kod från https://www.youtube.com/watch?v=GtgJEVxl7DY
normalise <-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

munge1<-function(nrRows, data){
  newData<-data.frame()
  i=1
  loops=(nrRows/nrow(data))+1
  variance<-getOldVariance(data)
  while (i< loops) {
    if(i==0){
      newData=rbind(munge(data, syntheticData, variance, 4, (123+i)))     
    }else{
      newData=rbind(newData,munge(data, syntheticData, variance, 4,(123+i)))     
    } 
    i=i+1
  }
  
  return(newData[1:nrRows,])
}
munge2<-function(nrEx, data){
  variancePerColumn<-getVariancePerKolumn(data)
  newData<-data.frame()
  i=0
  loops=(nrEx/nrow(data))+1
  seedNr=1234
  while (i < loops) {
    if(i==0){
      newData=rbind(mungeWithColumnVariance(data, syntheticData, variancePerColumn, 4,(seedNr+i)))     
    }else{
      newData=rbind(newData,mungeWithColumnVariance(data, syntheticData, variancePerColumn, 4, (seedNr+i)))     
    } 
    i=i+1
  }
  return(newData[1:nrEx,])
}

#Diagram with percent
percentDiagram<-function(ourdata, message){
  #https://gist.github.com/kyzhao/6384136#file-multhist_yperc-r-L22
  library(plotrix)
  library(RColorBrewer)
  l <-ourdata
  par(las=1,cex=1.1,yaxt="n")
  xx <-multhist(l, legend=T,freq=F,xlab=message, ylab="%",main="",breaks=c(-100,0, 0.2,0.4,0.6,0.8,1.0,100))
  t<-axTicks(2)
  par(yaxt="s")
  axis(2,at=t,labels=round(t/rowSums(xx[[2]])[1]*100,2))
  legend( "topleft", inset = c(0.03,0.2), cex = 0.7, 
           legend = c("Data", "M1","M2","M3"), 
        #  text.width = 2,

           pt.bg = c("#666666", "#808080","#d9d9d9","#f2f2f2"),
           pch = 21
  )
}

performMungeForIris()
#används ej
convertIntoPercent<-function(data,totalSize){
 pList<-vector()
 pList[1]<-sum(data < 0)
 pList[1]<-pList[1]/totalSize
 i=0.0
 pos=2
 while(i<1){
   pList[pos]<-sum(data < i)
   pList[pos]<-pList[pos]/totalSize
   i=i+0.1
   pos=pos+1
   print(pos)
 }
 temp<-sum(data < 0)
 pList <- c(pList, pList[1]/totalSize )
}

normaliseAndFactor<-function(dataSet){
  i=1
  while(i<=ncol(dataSet)){
    if(!is.factor(dataSet[,i])){
      dataSet[,i]<-normalise(dataSet[,i])
    }else{
      v<-as.numeric(dataSet[,i])
      dataSet[,i]<-as.factor(v)
    }
    i=i+1
  }
  return(dataSet)
}

mixData<-function(newdata){
  set.seed(54)
  gp<-runif(nrow(newdata))
  newdata<-newdata[order(gp),]
  return(newdata)
}
getOldVariance<-function(df){
  cols<-ncol(df)
  #Får fram lokala variansen på alla numeriska värden
  value=NULL
  nr=1
  while(nr<=cols){
    if(!is.factor(df[1,nr])){
        f<-vector()
        f<-(df[,nr])
        value=append(value, f)   
    }else{
    }
    nr=nr+1
  }
#  variance <-sqrt(var(value) )
  variance <-var(value) 
  return (variance)
}


getVariancePerKolumn<-function(df){
 variance<-apply(df,2, var)
 i=1
 while(i<=length(variance)){
  # variance[i]<-sqrt(variance[i])
   variance[i]<-variance[i]
   i=i+1
 }
  return (variance)  
}


#Returnerar vektor med nearest neighbour till kolumnen som skickas in.
getNearestNeighbour<-function(values, columnNr){
  library(spatstat)
  temp<-as.vector(values[,columnNr])
  d <- nnwhich(temp)
  newVector<-temp[order(d)]
  return(newVector)
}

munge<-function(trainingEx, syntheticData, variance, p, seedNr){
  set.seed(seedNr)
  dataClass<-vector()
  nrOfColumnsInTempFrame=ncol(trainingEx)
  s<-vector()
  temp<-data.frame()
  nearestTemp<-data.frame()
  x<-1
  while(x<=nrOfColumnsInTempFrame){
    if(!is.factor(trainingEx[1,x])){
      if(x==1){
        nearestTemp<-cbind(getNearestNeighbour(trainingEx, x))
      }
      else{
        nearestTemp<-cbind(nearestTemp,getNearestNeighbour(trainingEx, x))
      }
    }else{
      if(x==1){
        nearestTemp<-cbind(trainingEx[,x] )
      }
      else{
        nearestTemp<-cbind(nearestTemp,trainingEx[,x])
      }
    }
    x=x+1 
  }
  #Vi fyller S med syntetiska exempel
  y<-1
  while(y<nrOfColumnsInTempFrame+1){
    if(!is.factor(trainingEx[1,y])){
      stdDevOne<-abs((trainingEx[,y]-nearestTemp[,y])/variance)
      stdDevTwo<-abs((nearestTemp[,y]-trainingEx[,y])/variance)
      set.seed(seedNr+y)
      z<-1
      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        if(prob < p+1){
          s[z]<-rnorm(1,mean=nearestTemp[z,y],sd=stdDevOne[z])  
        }
        else{
          s[z]<-rnorm(1,mean=trainingEx[z,y],sd=stdDevTwo[z])  
        }   
        z=z+1
      }
    }else{#Faktorer tas här om hand. Slumpmässigt byte. 
      z<-1
      #s<-nearestTemp[,y]   
      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        if(prob < p+1){
          s[z]<-nearestTemp[z,y]      
        }
        else{
          s[z]<-(trainingEx[z,y])
       }   
        z=z+1
      }      
    } 
    if(y==1){
      syntheticData=cbind(s)     
    }else{
      syntheticData=cbind(syntheticData,s)     
    }  
    y=y+1
  }
  return (syntheticData)
}

mungeWithColumnVariance<-function(trainingEx, syntheticData, variancePerColumn,p,seedNr){
  #Formatterar p till procenttal
  set.seed(seedNr)
  turn=0
  temp=data.frame()
  dataClass<-vector()
  nrOfColumnsInTempFrame=ncol(trainingEx)
  s<-vector()
  temp<-data.frame()
  nearestTemp<-data.frame()
  x<-1
  while(x<=nrOfColumnsInTempFrame){
    if(!is.factor(trainingEx[,x])){
      if(x==1){
        nearestTemp<-cbind(getNearestNeighbour(trainingEx, x))
      }
      else{
        nearestTemp<-cbind(nearestTemp,getNearestNeighbour(trainingEx, x))
      }
    }else{
      if(x==1){
        nearestTemp<-cbind(trainingEx[,x] )
      }
      else{
        nearestTemp<-cbind(nearestTemp,trainingEx[,x])
      }
    }
    x=x+1 
  }
  ncol(nearestTemp)
  
  #Vi fyller S med syntetiska exempel
  y<-1
  while(y<=nrOfColumnsInTempFrame){

    if(!is.factor(trainingEx[,y])){
     
      stdDevOne<-abs((trainingEx[,y]-nearestTemp[,y])/variancePerColumn[y])

      stdDevTwo<-abs((nearestTemp[,y]-trainingEx[,y])/variancePerColumn[y])

      set.seed(seedNr+y)
      z<-1
      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        if(prob<=p){
 
          s[z]<-rnorm(1,mean=nearestTemp[z,y],sd=stdDevOne)  
        }
        else{

          s[z]<-rnorm(1,mean=trainingEx[z,y],sd=stdDevTwo) 
        }   
        z=z+1
      }
    }else{#Icke-numeriska värden tas här om hand. Slumpmässigt byte. 
      z<-1
     # s<-trainingEx[,y]

      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        if(prob<=p){
         s[z]<-nearestTemp[z,y]      
        }
        else{
          s[z]<-trainingEx[z,y]  
        }   
        z=z+1
      } 
    #  print(s[z])
    }
    if(y==1){
      syntheticData=cbind(s)     
    }else{
      syntheticData=cbind(syntheticData,s)     
    }   
    y=y+1
  }   
  return (syntheticData)
}

mungeWithDetailedColumnVariance<-function(trainingEx,syntheticData, dV,p,seedNr){
  turn=0
  set.seed(seedNr)
  dataClass<-vector()
  nrOfColumnsInTempFrame=ncol(trainingEx)
 # print(nrOfColumnsInTempFrame)
  s<-vector()
  temp<-data.frame()
  nearestTemp<-data.frame()
  x<-1
  # print(nrOfColumnsInTempFrame)
  while(x<=nrOfColumnsInTempFrame){
    if(!is.factor(trainingEx[1,x])){
      if(x==1){
        nearestTemp<-cbind(getNearestNeighbour(trainingEx, x))
      }
      else{
        nearestTemp<-cbind(nearestTemp,getNearestNeighbour(trainingEx, x))
      }
    }else{
      if(x==1){
        nearestTemp<-cbind(trainingEx[,x] )
      }
      else{
        nearestTemp<-cbind(nearestTemp,trainingEx[,x])
      }
    }
    x=x+1 
    # print(x)
  }
  #Vi fyller S med syntetiska exempel
  y<-1
  numCount<-1
  while(y<=nrOfColumnsInTempFrame){
    #    print(numCount)
    if(!is.factor(trainingEx[,y])){
      #Om std blir nan får den defaultvärdet 1. 
      stdDevOne<-abs((trainingEx[,y]-nearestTemp[,y])/dV[,numCount])
      if(is.nan(stdDevOne)){
        stdDevOne<-replace(stdDevOne, is.nan(stdDevOne), 1)       
      }
   #   mean.One<-mean(trainingEx[,y])
      stdDevTwo<-abs((nearestTemp[,y]-trainingEx[,y])/dV[,numCount])
      if(is.nan(stdDevTwo)){
        stdDevTwo<-replace(stdDevTwo, is.nan(stdDevTwo), 1)       
      }
  #    mean.Two<-mean(nearestTemp[,y])
      set.seed(seedNr+y)
      numCount=numCount+1
      z<-1
      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        if(prob<=p){
          s[z]<-rnorm(1,mean=nearestTemp[z,y],sd=stdDevOne[z])          
        }
        else{
          s[z]<-rnorm(1,mean=trainingEx[z,y],sd=stdDevTwo[z])   
        }   
        z=z+1
      }
    }else{#Icke-numeriska värden tas här om hand. Slumpmässigt byte. 
     # s<-trainingEx[,y] 
      z<-1
      while(z<=nrow(trainingEx)){
        prob<-runif(1, 0, 100)
        
        if(prob<=p){
          s[z]<-nearestTemp[z,y]      
        }
        else{
          s[z]<-trainingEx[z,y]  
        }   
       z=z+1
      }      
    }
    if(y==1){
      syntheticData=cbind(s)     
    }else{
      syntheticData=cbind(syntheticData,s)     
    }   
    y=y+1
  }   
  return (syntheticData)
}

averageResultDiagram<-function(bagging,tree, result,titleText){
  z=result[1,]
  x=result[2,]
  y=result[3,]
  if(min(result)<tree){
    if(min(result)<bagging){
      minLimit=min(result)-0.02
    }else{
      minLimit=bagging-0.02
    }
  }else if(min(result)>=tree){
    if(min(result)<tree){
      minLimit=min(result)-0.02
    }else{
      minLimit=tree-0.02
    }
  }
  if(max(result)>tree){
    if(max(result)>bagging){
      maxLimit=max(result)+0.02
    }else{
      maxLimit=bagging+0.02
    }
  }else if(max(result)<=tree){
    if(bagging>tree){
      maxLimit=bagging+0.02
    }else{
      maxLimit=tree+0.02
    }
  } 
  print("min o max")
  print(minLimit)
  print(maxLimit)
  plot(z,pch=20,type="b", col="green", xaxt="n",ylim=c(minLimit,maxLimit), xlim=c(0.9,2.3),xlab="",
       ylab="", legend=T, main="")
  title(main=titleText, line=0.5,cex.lab=0.7,mgp=c(1,1,0),adj=0.06 )
  title(ylab="Percent", line=2.8, cex.lab=0.7,mgp=c(1,1,0) )
  title(xlab="Generated data", line=0.6, cex.lab=0.7,mgp=c(1,1,0) )
  
  abline(h=bagging, col="pink")
  abline(h=tree,col="purple")
  points(x,type="b", pch=6, col="blue")
  points(y,type="b",pch=8, col="red")
  axis(1, at = seq(1, 2, by = 1), las=1,labels =c("5000", "20000"),lwd=0.5)
  #labels <- c("5000", "20000")

  #text(1:2, par("usr")[1] - 1.2, ps = 5,
  #     labels = labels,xpd = TRUE)
  legend("bottomright",cex=0.65, col=c("green", "blue", "red", "purple", "pink"), bty="o",
         pch=c(20,6,8,23,23), title="Result",
         legend=c("M1", "M2", "M3","Single tree", "Bagging"),
         box.lty=3) 
}

allResultsDiagram<-function(allResults,titleText){
  z1=allResults[,1]
  z2=allResults[,2]
  y1=allResults[,3]
  y2=allResults[,4]
  x1=allResults[,5]
  x2=allResults[,6]
  tree=allResults[,7]
  bagg=allResults[,8]
  minLimit=min(allResults)-0.04
  maxLimit=max(allResults)+0.04
  
  plot(z1, col='white',xaxt="n",xlim=c(0.998, (nrow(allResults))+0.35), ylim=c(minLimit, 1.01), xlab="",
       ylab="", legend=T, main="")
  title(main=titleText, line=0.5,cex.lab=0.7,mgp=c(1,1,0),adj=0.06 )
  title(ylab="Percent", line=2.8, cex.lab=0.7,mgp=c(1,1,0) )
  title(xlab="Iteration in ten fold cross validation", line=1.6, cex.lab=0.7,mgp=c(1,1,0) )
  lines(z1, col='blue')
  lines(z2, col='blue4')
  lines(y1, col='chartreuse1')
  lines(y2, col='chartreuse4')
  lines(z1, col='darkorange')
  lines(z2, col='darkorange3')
  lines(tree, col='purple')
  lines(bagg, col='pink')
  axis(1, at = seq(1,10, by = 1), las=1)

  legend("bottomright",cex=0.65, col=c('blue', 'blue4', 'chartreuse1','chartreuse4','darkorange','darkorange3', "purple", "pink"), bty="o",
         pch=16, title="Result",
         legend=c("M1:5000", "M1:20000", "M2:5000", "M2:20000", "M3:5000", "M3:20000","Single tree", "Bagging"),
         box.lty=3) 
}
