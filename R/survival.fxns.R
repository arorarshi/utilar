####################################
# Survival related functions
#####################################

my.plot.km<-function(ff,my.os,my.status,xlab,legendv,col,main,mylty=1,lr=TRUE, med.surv=TRUE,mylcex=1, ...){

  ff = as.factor(ff)
  km=survfit(Surv(my.os,my.status)~ff)
  km.chi=NA
  if(length(unique(ff)) > 1){
    km.stats<-survdiff(Surv(my.os,my.status)~ff)
    #select the correct df for log rank test
    #unique also counts NA
    km.chi= 1 - pchisq(km.stats$chisq, length(na.omit(unique(ff)))-1)
    if(lr){
      plot(km,mark.time=T,xlab=paste0("Time ", xlab),col=col,sub = paste0("logrank p-value=",round(km.chi,5)),main=main, lty=mylty, frame.plot =F, ...)
      legend.text = rep(NA, length(legendv))}

    if(!lr){
      plot(km,mark.time=T,xlab=paste0("Time ", xlab),col=col,main=main,  lty=mylty, frame.plot =F, ...)
      legend.text = rep(NA, length(legendv))}
    for (i in 1:length(legendv)){

      if(med.surv){
      legend.text[i] = paste0(legendv[i]," ,Median surv time=",round(summary(km)$table[i,"median"],2)," (n=",summary(km)$table[i,"records"]," events=",summary(km)$table[i,"events"],")")
      }

      if(!med.surv){
        legend.text[i] = paste0(legendv[i], "(n=",summary(km)$table[i,"records"]," events=",summary(km)$table[i,"events"],")")
      }
    }
  }

  if( length(unique(ff))==1){
    plot(km,mark.time=T,xlab=paste0("Time ", xlab),col=col,main=main, lty=mylty, frame.plot =F, ...)
    if(med.surv){
    legend.text = paste0(legendv," ,Median surv time=",round(summary(km)$table["median"],2)," (n=",summary(km)$table["records"]," events=",summary(km)$table["events"],")")
    }

    if(!med.surv){
      legend.text = paste0(legendv," (n=",summary(km)$table["records"]," events=",summary(km)$table["events"],")")
    }
  }

  legend("bottomleft",legend.text,col=col,lty=mylty,lwd=2,bty="n", cex=mylcex)
}


uvar.surv<-function(time, event,group.var,varname, type){

  time<-as.numeric(as.character(time))
  event<-as.numeric(as.character(event))

  #if(type==1){group.var<-as.factor(as.character(group.var))}
  if(type==2){group.var<-as.numeric(as.character(group.var))}

  xx = table(group.var)

  if(length(xx) > 1){

    ttcox = summary(coxph(Surv(time,event) ~ group.var) )
    if (type==1){
      kk = survfit( Surv(time,event) ~ group.var)
      kkt =  round(summary(kk)$table,2)
      final = matrix("",nrow=nrow(kk),ncol=4)
      final[,1] = mapply( function(x,y) paste0(x,"(",y,")"), kkt[,1], kkt[,4])
      final[,2] = mapply( function(x,y,z) paste0(x,"[",y,"-",z,"]"), kkt[,7], kkt[,8],kkt[,9])
      cicox<-round(ttcox$conf.int,2)

      final[2:nrow(final),3] =mapply( function(x,y,z) paste0(x,"[",y,"-",z,"]"), cicox[,1], cicox[,3],cicox[,4])
      final[2:nrow(final),4] <- ifelse( ttcox$coefficients[,5]< 0.0001, "P<0.0001", round(ttcox$coefficients[,5],4))

      final = rbind(rep("",4),final)
      final[1,4] = ifelse( ttcox$logtest[3]< 0.0001, "P<0.0001",  round(ttcox$logtest[3],4))

      colnames(final) = c("N(events)","Median(95%CI)","HR(95%CI)","p-value")
      rownames(final) =  gsub("group.var=", "",c(varname,rownames(kkt)))
      return(final)}


    if(type==2){
      final = matrix("",nrow=1,ncol=4)
      final[1,1] = paste0(ttcox$n,"(",ttcox$nevent,")" )
      cicox<-round(ttcox$conf.int,2)
      final[1,3] = paste0(cicox[,"exp(coef)"],"(",cicox[,"lower .95"],"-", cicox[,"upper .95"],")" )
      final[1,4] = ifelse( ttcox$coefficients[,5]< 0.0001, "P<0.0001", round(ttcox$coefficients[,5],4))
      colnames(final) = c("N(events)","Median(95%CI)","HR(95%CI)","p-value")
      rownames(final) = c(varname)
      return(final)
    }
  }


  if(length(xx) == 1){ return(matrix(NA,nrow=1,ncol=4))}

}

mvar.surv<-function(time, event,group.var,varname=NULL, data, modelp=FALSE, anovap=FALSE, type="III"){

  if(is.null(varname)){varname = group.var}
  dep.x = paste(group.var, collapse="+")
  dep.y = paste0("Surv(", time, ",", event, ")")
  formu = paste(dep.y , dep.x, sep="~")

  ttcox = summary(coxph(as.formula(formu), data=data))

  mat.hr = round(ttcox$coefficients[,2],3); mat.p = ttcox$coefficients[,5]
  mat.p.round = sapply(mat.p, function(x) ifelse(x >= 0.001, round(x,4), "P<0.001"))
  mat.conf = round(ttcox$conf.int[,c(3:4)],3)

  cicox<- paste0("(",mat.conf[,"lower .95"],"-", mat.conf[,"upper .95"],")" )

  fmat = NULL
  for(i in 1:length(group.var)){

    idx = grep(group.var[i], names(mat.hr), fixed=T)
    tt = cbind(mat.hr[idx], cicox[idx], mat.p.round[idx])
    rtt = c(gsub(group.var[i], "", names(mat.hr)[idx]))

    gtt = rep("", 4); gtt[1] = varname[i]

    fmat = rbind(fmat, rbind(gtt, cbind(rtt, tt)))

  }

  colnames(fmat) = c(paste0("group n=(", ttcox$n, ",events=", ttcox$nevent, ")"), "HR", "95% CI", "P-value")
  if(modelp){ tt = rep("", ncol(fmat))
  mp = ttcox$logtest["pvalue"]
  mp = ifelse(mp >= 0.001, round(mp,4), "P<0.001")
  tt[1] = paste0("Model p-val = ",mp)
  fmat = rbind(fmat,tt)
  }

  if(anovap){
    ap = car::Anova(coxph(as.formula(formu), data=data), type=type)
    overall.p = rep("", nrow(fmat))
    for(i in 1:length(group.var)){
      idx = which(fmat[,1] == group.var[i])
      overall.p[idx] = ifelse(ap[4][2,1] >= 0.001,round(ap$`Pr(>|Chi|)`[i+1],4), "P<0.001")

      }
    fmat = cbind(fmat, overall.p)
  }
  rownames(fmat) <- NULL
  return(fmat)
}
