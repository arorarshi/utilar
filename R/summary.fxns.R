##################################
# Arshi Arora
#summary table 1 functions:
# 1. get.summary
# 2. get.summary2
##################################


##################################
#summarize vector values, can be discrete or continuous.
#continuous - median(range) or median(sd)
#discrete - tabbulate

get.summary<-function(ff,type, type2="range"){
  if(type==1){
    #discrete
    tt = table(as.character(ff),useNA="ifany")
    ttf = round(prop.table(tt)*100,2)
    summary = mapply( function(x,y) paste0(x,"(",y,"%)"),tt,ttf)
    return(summary)
  }

  if(type==2){
    #continuous
    ff = as.numeric(as.character(ff))
    median.value = round(median(ff, na.rm=T),2)
    if(type2=="range"){
      range.value = round(range(ff, na.rm=T),2)
      return(paste0(median.value,"(",range.value[1],"-",range.value[2] ,"); NA=",length(which(is.na(ff))) ))}
    if(type2=="sd"){
      sd.value = round(sd(ff, na.rm=T),2)
      return(paste0(median.value,"(",sd.value,"); NA=",length(which(is.na(ff))) ))}
  }
}

##################################
#association tests of continuous and discrete variables in a neat table format


get.summary2<-function(fac,var,type,skip.test=FALSE,para="np", var.n=NULL){

  if(length(fac) != length(var)){ stop("unequal lengths of fac and var")}

  #categorical data
  if (type==1){
    fac = as.character(fac)
    var = as.character(var)
    x = table(var,fac,useNA="ifany")
    #get row sum and freq
    csum = apply(x,2,sum)
    rsum = apply(x,1,sum)
    xperc = apply(x,2,prop.table)
    xperc = round(xperc,2)
    xtab = matrix("",nrow=nrow(x),ncol=ncol(x))
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        xtab[i,j] = as.character(paste0(x[i,j],"(",xperc[i,j]*100,"%)"))
      }
    }
    rownames(xtab) = names(rsum)
    colnames(xtab) = mapply(function(a,b) paste0(a,"(n=",b,";", round(((b/sum(csum)) * 100),2), "%)"),colnames(x), csum)

    rs = rep("", (length(rsum)+1))
    pval= rep("",length(rs))
    var.name = rep("", ncol(xtab))

    #fisher test handles NA
    if(skip.test==TRUE){
      pp = fisher.test(fac,var,workspace=2e8)$p.value
      pp = ifelse(pp<0.0001, "P<0.0001", round(pp,digits=4))
      pval[1] = pp}

    for (i in 1:length(rsum)){
      rs[i+1] = as.character(paste0(rsum[i],"(",round((rsum[i]/sum(rsum)*100),2),"%)"))
    }

    fmat = (cbind(rbind(var.name,xtab),rs,pval))
    colnames(fmat)[which(colnames(fmat)=="rs")] = "RowTotal"

    if(!(is.null(var.null))){ rownames(fmat)[which(rownames(fmat) == "var.name")] = var.n  }

  }

  #continuous
  if (type==2){
    fac = as.character(fac)
    var = as.numeric(as.character(var))

    mm =cbind.data.frame(fac, var, stringsAsFactors=F)

    if(para=="p"){

      tt.summary = ddply(mm, c("fac"), summarize, mean=mean(var,na.rm=T),
                         sd=sd(var,na.rm=T), NAs = length(which(is.na(var))) )
    }

    if(para=="np"){
      tt.summary = ddply(mm, c("fac"), summarize, median=median(var,na.rm=T),
                      min=range(var,na.rm=T)[1],
                      max=range(var,na.rm=T)[2], NAs = length(which(is.na(var))) )
    }



    cnames = as.character(tt.summary[,1])
    tt.summary = tt.summary[,-1]
    tt.summary = apply(tt.summary, 2,function(x) round(as.numeric(as.character(x)),2))

    if(para=="np"){fmat = mapply( function(x,y,z) paste0(x,"[",y,"-",z,"]"),tt.summary[,1],tt.summary[,2], tt.summary[,3])}

    if(para=="p"){fmat = mapply( function(x,y,z) paste0(x,"(",y,")"),tt.summary[,1],tt.summary[,2])}

    #y is a numeric, x is a factor wilcox_test(y~x)
    #always perform wilcox test for ties
    if (skip.test==TRUE){

      way = length(unique(na.omit(fac)))
      if(way ==1){ stop("fac has only one class of variables to compare? Need atleast 2 levels")}
      if(way==2 & para =="p"){pval =t.test(var ~ as.factor(fac))$p.value }
      if(way==2 & para =="np"){ pval = pvalue(wilcox_test(var ~ as.factor(fac)))}

      if(way>2 & para =="p"){ pval = ftest()}
      if(way>2 & para =="np"){ pval = kruskal.test(var, as.factor(fac))$p.value}

      pval = ifelse(pval<0.0001, "P<0.0001", round(pval,digits=3))
    }

    if(skip.test==FALSE){ pval = NA}

    fmat = c(fmat,"",pval)
    names(fmat) = c(cnames,"rs","pval")
    nas = tt.summary[,4]
    fmat = rbind(fmat,c(nas,"",""))
  }

  return(fmat)
}


#take in a list of results after get.summary2, and sort them according to pvalue
sort.get.summary<-function(){}


