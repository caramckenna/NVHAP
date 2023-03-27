# Load # Load packages
library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(splines)
library(haven)
library(tidyr)
library(data.table)
library(purrr)



#Fixing spline parameters for bootstrap   
splines<-vector(5, mode ="list")
names(splines)<- c("current", "lag1", "lag2", "age", "elixhauser")
for(i in 3){
	splines[[i]]<-vector(9, mode="list")
	names(splines[[i]])<-lab_var_tags
}
for(j in c(1:3, 6, 7)){
	splines[[3]][[j]]<-
		eval(parse(text=paste0(
			"ns(simdat1$last_", lab_var_tags[j], "_lag2_t, df=4)"
		)))
}
splines[[4]][[1]]<-ns(simdat1$age, df=4)
splines[[5]][[1]]<-ns(simdat1$elixhauser, df=4)



# Define number of time points (i.e. days of follow-up)
K <- max(simdat1$dayt)+1

n <- length(table(simdat1$PatID)) 


#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3


#Formula for denominator weight model

base_vars<-c(
	"ns(age, knots=attributes(splines[[4]][[1]])$knots, Boundary.knots =attributes(splines[[4]][[1]])$Boundary.knots)",
	"male", "race",
	"bedsize", "tchtype", "region",
	"diabetes", "cancer", "CPD", "RENLFAIL", "LIVER", "CHF", "cardiovasc", "NEURO", "ANEMDEF", "AIDS", "ARTH",
	"OBESE","ALCOHOL", "WGHTLOSS", "admission_prev90")
tv_vars<-c(
	paste0("ns(last_", lab_var_tags[c(1:3, 6, 7)], "_lag2_t, knots=attributes(splines[[3]][[", c(1:3, 6, 7), "]])$knots, Boundary.knots =attributes(splines[[3]][[", c(1:3, 6, 7), "]])$Boundary.knots)"),
	"as.factor(last_lab_meas_cat)",
	"as.factor(last_alt_org_lag2_cat)", 
	"as.factor(last_bilirubin_org_lag2_cat)", 
	"as.factor(last_albumin_org_lag2_cat)",
	"as.factor(last_spo2_lag2_cat)", "as.factor(baseline_oxygen_lag2)", 
	"as.factor(service_group_cat)", "as.factor(icu_lag2)"
)

denom_form<-as.formula(paste0("A~", paste0(base_vars, collapse = " + "), " + ", paste0(tv_vars, collapse = " + ")))  
simdat1$male<-simdat1$sex=="MALE"  
vars<-c(
	"age", "male", "race", "bedsize", "tchtype", "region",
	"diabetes", "cancer", "CPD", "RENLFAIL", "LIVER", "CHF", "cardiovasc", "NEURO", "ANEMDEF", "AIDS", "ARTH", "OBESE","ALCOHOL", "WGHTLOSS",
	paste0("last_", lab_var_tags[c(1:3, 6, 7)], "_lag2_t"),    
	"last_alt_org_lag2_cat",  "last_bilirubin_org_lag2_cat", "last_albumin_org_lag2_cat","last_spo2_lag2_cat", "baseline_oxygen_lag2",
	"last_lab_meas_cat", "service_group_cat",
	"A", "Alag1", "t", "t0", "Y", "D", "siteID", "PatID",
	"elixhausercat", "agecat",  "icu3", "icu_lag2",  "admission_prev90", "service_group_cat_bl"
)


#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3


#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3


#utility functions for IPW estimators to compute weighted cumulative incidence for each cause of failure (here Y or D)
nonParametricCumHaz <- function(weightVector, inputdata, outcomeEvent=TRUE){
	outputHazards <- rep(NA, length.out=length(cutTimes))
	counter <- 1 
	for(i in cutTimes){
		if(outcomeEvent){
			indices <- inputdata$t==i & inputdata$D==0 
			eventIndicator <- indices & inputdata$Y==1 
		}else{
			indices <- inputdata$t==i 
			eventIndicator <- indices & inputdata$D==1 
		}
		if(is.nan(sum(weightVector[eventIndicator]) / sum(weightVector[indices]))){
			outputHazards[counter] <-0
		}else{
			outputHazards[counter] <- sum(weightVector[eventIndicator]) / sum(weightVector[indices])
			
		}
		counter <- counter+1
	}
	return(outputHazards)
}

nonParametricCumInc <- function(hazard1,hazard2,competing=FALSE){
	inc <- rep(NA, length.out=length(cutTimes))
	cumulativeSurvival <- c(1, cumprod( (1-hazard1) * (1-hazard2) ))
	counter <- 1 
	for(i in 1:length(cutTimes)){
		if(!competing){
			inc[i] <- hazard1[i] * (1-hazard2[i]) * cumulativeSurvival[i]
		}else{
			inc[i] <- hazard2[i] * cumulativeSurvival[i]
		}
	}
	cumInc <- cumsum(inc)
	return(cumInc)
}


#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3

maxtime<-K-1
cutTimes <- c(0:maxtime)


#Defining variables for stratified analyses
stratvars<-c("agecat", "service_group_cat_bl", "icu3", "elixhausercat", "bedsize", "region", "tchtype")
strat_levs<-vector(mode="list", 7)
names(strat_levs)<-stratvars
strat_levs[[1]]<-as.list(1:2)
strat_levs[[2]]<-as.list(0:5)
strat_levs[[3]]<-as.list(0:1)
strat_levs[[4]]<-as.list(1:4)
strat_levs[[5]]<-as.list(names(table(simdat1[, stratvars[5]])))
strat_levs[[6]]<-as.list(names(table(simdat1[, stratvars[6]])))
strat_levs[[7]]<-as.list(names(table(simdat1[, stratvars[7]])))
strat_levs_v<-sapply(FUN=function(x){rep(stratvars[x], length(strat_levs[[x]]))}, 1:7) %>% unlist
strat_levs_f<-flatten(strat_levs)




#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3
#######################################################################################################################3

#number of bootstraps
bN<-500

#list objects for storing results
haz_cums_bysite_b<-vector(3, mode="list")
names(haz_cums_bysite_b)<-c("int", "nc", "bootID")
for(j in 1:2){
	haz_cums_bysite_b[[j]]<-vector(25, mode="list")
	names(haz_cums_bysite_b[[j]])<-c("Overall", sapply(FUN=function(x){paste0(stratvars[x], strat_levs[[x]])}, 1:7) %>% unlist)
	for(h in 1:25){
		haz_cums_bysite_b[[j]][[h]]<-vector(4, mode="list")
		names(haz_cums_bysite_b[[j]][[h]])<-c("hazard_death", "hazard_discharge", "cumincdeath", "cumincdischarge")
		for(i in 1:4){
			haz_cums_bysite_b[[j]][[h]][[i]]<-matrix(NA, bN, K)
		}
	}
}
haz_cums_bysite_b[[3]]<-matrix(NA, bN, 10)
statistics_b<-matrix(NA, bN, 25*9)

#New ID for bootstrapping with VA
simdat1$NewID<-as.numeric(as.factor(simdat1$PatID))

#sample sizes of HCA and VA sites (code run separately by site)
nVA=2259032
#nHCA=1429479

#Bootstrap function
boot<-function(N){
	###########################################################
	set.seed(N)
	
	bootsamp<-sample(1:(n+nVA), (n+nVA), replace=T)
	#bootsamp<-sample(1:(n+nVA), (n+nHCA), replace=T)
	bootsampHCA<-subset(bootsamp, bootsamp<=n)
	haz_cums_bysite_b[[3]][N, ]<-head(bootsampHCA, 10)
	bootsampHCA<-data.frame(NewID=bootsampHCA, bootID=1:(length(bootsampHCA)))
	newdat<-bootsampHCA %>% left_join(simdat1) %>% rename(originalID=NewID) %>% rename(NewID=bootID)
	#bootsampVA<-subset(bootsamp, bootsamp>n)
	#bootsampVA<-data.frame(NewID=bootsampVA, bootID=(n+1):(n+length(bootsampVA)))
	#newdat<-bootsampVA %>% left_join(simdat1) %>% rename(originalID=NewID) %>% rename(NewID=bootID)
	
	
	###########################################################
	denomprob <- glm(denom_form,  y=F, model=F,
									 data=newdat[newdat$Alag1==0 & newdat$baseline_oxygen_lag2 %in% 0:7,],
									 family=binomial(link = "logit"))
	
	###########################################################
	wgt_temp<-rep(1, nrow(newdat))
	
	wgt_temp[newdat$A==0 & newdat$baseline_oxygen_lag2 %in% 0:7] <-
		1/(1-(predict(denomprob,
									newdat[newdat$A==0 & newdat$baseline_oxygen_lag2 %in% 0:7,],
									type = "response")))
	
	
	wgt_temp[newdat$A==1 ] <- 0
	
	
	# Calc. final IP weights for complete data
	temp<- data.frame(wgt_temp=wgt_temp, NewID=newdat$NewID) %>%  group_by(NewID)  %>% mutate(stabwts = cumprod(wgt_temp)) %>% ungroup()
	
	stabwts<-temp$stabwts
	ncwts<-rep(1,length(stabwts))
	
	
	#Computing results using utility functions and assigning results to list objects defined above
	###########################################################
	haz_cums_bysite_b[[1]][[1]][[1]][N, ]<<-nonParametricCumHaz(stabwts, inputdata=newdat, outcomeEvent=TRUE)
	haz_cums_bysite_b[[1]][[1]][[2]][N, ]<<-nonParametricCumHaz(stabwts, inputdata=newdat, outcomeEvent=FALSE)
	haz_cums_bysite_b[[1]][[1]][[3]][N, ]<<-nonParametricCumInc(haz_cums_bysite_b[[1]][[1]][[1]][N, ],haz_cums_bysite_b[[1]][[1]][[2]][N, ])
	haz_cums_bysite_b[[1]][[1]][[4]][N, ]<<-nonParametricCumInc(haz_cums_bysite_b[[1]][[1]][[1]][N, ],haz_cums_bysite_b[[1]][[1]][[2]][N, ],competing=TRUE)
	haz_cums_bysite_b[[2]][[1]][[1]][N, ]<<-nonParametricCumHaz(ncwts, inputdata=newdat, outcomeEvent=TRUE)
	haz_cums_bysite_b[[2]][[1]][[2]][N, ]<<-nonParametricCumHaz(ncwts, inputdata=newdat, outcomeEvent=FALSE)
	haz_cums_bysite_b[[2]][[1]][[3]][N, ]<<-nonParametricCumInc(haz_cums_bysite_b[[2]][[1]][[1]][N, ],haz_cums_bysite_b[[2]][[1]][[2]][N, ])
	haz_cums_bysite_b[[2]][[1]][[4]][N, ]<<-nonParametricCumInc(haz_cums_bysite_b[[2]][[1]][[1]][N, ],haz_cums_bysite_b[[2]][[1]][[2]][N, ],competing=TRUE)
	
	#By strata
	for(h in 2:25){
		#Overall
		haz_cums_bysite_b[[1]][[h]][[1]][N, ]<<- {newdat[, strat_levs_v[h-1]]==strat_levs_f[h-1]} %>% {nonParametricCumHaz(stabwts[.], inputdata=newdat[., ], outcomeEvent=TRUE)}
		haz_cums_bysite_b[[1]][[h]][[2]][N, ]<<- {newdat[, strat_levs_v[h-1]]==strat_levs_f[h-1]} %>% {nonParametricCumHaz(stabwts[.], inputdata=newdat[., ], outcomeEvent=FALSE)}
		haz_cums_bysite_b[[1]][[h]][[3]][N, ]<<-                                       {nonParametricCumInc(haz_cums_bysite_b[[1]][[h]][[1]][N, ],haz_cums_bysite_b[[1]][[h]][[2]][N, ])}
		haz_cums_bysite_b[[1]][[h]][[4]][N, ]<<-                                       {nonParametricCumInc(haz_cums_bysite_b[[1]][[h]][[1]][N, ],haz_cums_bysite_b[[1]][[h]][[2]][N, ],competing=TRUE)}
		haz_cums_bysite_b[[2]][[h]][[1]][N, ]<<- {newdat[, strat_levs_v[h-1]]==strat_levs_f[h-1]} %>% {nonParametricCumHaz(ncwts[.], inputdata=newdat[., ], outcomeEvent=TRUE)}
		haz_cums_bysite_b[[2]][[h]][[2]][N, ]<<- {newdat[, strat_levs_v[h-1]]==strat_levs_f[h-1]} %>% {nonParametricCumHaz(ncwts[.], inputdata=newdat[., ], outcomeEvent=FALSE)}
		haz_cums_bysite_b[[2]][[h]][[3]][N, ]<<-                                                         {nonParametricCumInc(haz_cums_bysite_b[[2]][[h]][[1]][N, ],haz_cums_bysite_b[[2]][[h]][[2]][N, ])}
		haz_cums_bysite_b[[2]][[h]][[4]][N, ]<<-                                                         {nonParametricCumInc(haz_cums_bysite_b[[2]][[h]][[1]][N, ],haz_cums_bysite_b[[2]][[h]][[2]][N, ],competing=TRUE)}
	}    
	###########################################################
	statistics_b[N, 1]<<-haz_cums_bysite_b[[1]][[1]][[3]][N, K]
	statistics_b[N, 2]<<-haz_cums_bysite_b[[2]][[1]][[3]][N, K]
	statistics_b[N, 3]<<-haz_cums_bysite_b[[1]][[1]][[3]][N, K]/haz_cums_bysite_b[[2]][[1]][[3]][N,K]
	statistics_b[N, 4]<<-haz_cums_bysite_b[[1]][[1]][[3]][N, K]-haz_cums_bysite_b[[2]][[1]][[3]][N,K]
	statistics_b[N, 5]<<-haz_cums_bysite_b[[1]][[1]][[4]][N, K]
	statistics_b[N, 6]<<-haz_cums_bysite_b[[2]][[1]][[4]][N, K]
	statistics_b[N, 7]<<-haz_cums_bysite_b[[1]][[1]][[4]][N, K]/haz_cums_bysite_b[[2]][[1]][[4]][N, K]
	statistics_b[N, 8]<<-haz_cums_bysite_b[[1]][[1]][[4]][N, K]-haz_cums_bysite_b[[2]][[1]][[4]][N, K]
	statistics_b[N, 9]<<- {newdat[, "t"]==0} %>% {newdat[., ]} %>% nrow
	
	for(h in 1:24){
		statistics_b[N, h*(9)+1]<<-haz_cums_bysite_b[[1]][[h+1]][[3]][N, K]
		statistics_b[N, h*(9)+2]<<-haz_cums_bysite_b[[2]][[h+1]][[3]][N, K]
		statistics_b[N, h*(9)+3]<<-haz_cums_bysite_b[[1]][[h+1]][[3]][N, K]/haz_cums_bysite_b[[2]][[h+1]][[3]][N,K]
		statistics_b[N, h*(9)+4]<<-haz_cums_bysite_b[[1]][[h+1]][[3]][N, K]-haz_cums_bysite_b[[2]][[h+1]][[3]][N,K]
		statistics_b[N, h*(9)+5]<<-haz_cums_bysite_b[[1]][[h+1]][[4]][N, K]
		statistics_b[N, h*(9)+6]<<-haz_cums_bysite_b[[2]][[h+1]][[4]][N, K]
		statistics_b[N, h*(9)+7]<<-haz_cums_bysite_b[[1]][[h+1]][[4]][N, K]/haz_cums_bysite_b[[2]][[h+1]][[4]][N, K]
		statistics_b[N, h*(9)+8]<<-haz_cums_bysite_b[[1]][[h+1]][[4]][N, K]-haz_cums_bysite_b[[2]][[h+1]][[4]][N, K]
		statistics_b[N, h*(9)+9]<<-{newdat[, strat_levs_v[h]]==strat_levs_f[h] & newdat[, "t"]==0} %>% {newdat[., ]} %>% nrow
		
	}   
	return(NULL)
}

Start<-Sys.time()
test<-boot(1)
Finish<-Sys.time()
Finish-Start


