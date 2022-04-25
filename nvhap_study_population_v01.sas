/***************************************************************************************/
/* PURPOSE:       Macro to create daily NV-HAP study population datasets.
/* AUTHOR:        Cara McKenna
/* CREATION DATE: 11/21/19
/*---------------------------------------------------------------------------------------
/* PARAMETERS:    r=required o=optional
/*                
/*                episode    (r): Input episode dataset, with or without libname 
/*                daily      (r): Input daily dataset, , with or without libname 
/*                studyidpfx (o): Character prefix to add to patid to create unique study 
/*                                identifier for combining data. If omitted patid is  
/*                                used as studyid. 
/*---------------------------------------------------------------------------------------
/*  CHANGE LOG: 
/*
/*   Version   Date       Initials   Comment 
/*   -------   --------   --------   ----------------------------------------------------
/*   1.0       11/21/19   CM         Initial Version. 
/***************************************************************************************/

%macro study_population(episode=,daily=,studyidpfx=,debug=N);
  
	%let version=01;
  %let program=nvhap_study_population_v&version;
  %put =====> MACRO CALLED: &program;

  data _null_;
      title;
      file print;
      put "Created on %sysfunc(date(),worddate.) at %sysfunc(time(),timeampm.)";
      put "By &program";
  run;
  
  /* parse libname and datasets  */
  %if %index(&episode,%str(.)) > 0 %then %do;
    %let lib=%scan(&episode,1,%str(.));
    %let epdsn=%scan(&episode,2,%str(.));
  %end;
  %else %do;
    %let lib=WORK;
    %let epdsn=&episode;
  %end;

  %put &=lib;
  %put &=epdsn;
  
  proc sql;
    title "Nobs &episode";
    select count(*)
    from &episode;

    title "Nobs &daily";
    select count(*)
    from &daily;

  quit;
  title;

  proc sort data=&episode out=tmp.in_episode; by patid admission_date;
  run;

  /* add episode indicator and create studyid */
  data tmp.episode;
    set tmp.in_episode;
    epid+1;
    by patid;

    if first.patid then epid=1;

    patepid=catx('-',patid,epid);

    %if %length(&studyidpfx) > 0 %then %do;
      studyid=cats("&studyidpfx",patid);
      studyepid=catx('-',studyid,epid);
    %end;
    %else %do;
      studyid=patid;
      studyepid=patepid;
    %end;

    hosplos=discharge_date-admission_date+1;

    label epid= "Hospitalization Episode Identifier"
          patepid = "Patient-Hospitalization Identifier"
          studyid = "Study Patient Identifier"
          studyepid = "Study Patient-Hospitalization Identifier"
          hosplos   = "Hospital Length of Stay";

  run;

  %if &debug=Y %then %do;
    proc print data= tmp.episode (obs=10);
      var patid epid patepid studyid studyepid hosplos;
    run;
  %end;


  proc sort data=&daily out=in_daily; by patid date;
  run;

  /* create hospital day */
  data tmp.daily1;
    set in_daily;
    retain day 1;
    
    if patid ne lag(patid) or date ne lag(date)+1 then day=1; 
    else day=day+1;
   run;
 
  /* add study and episode ids to daily dataset */
  proc sql;
    create table tmp.daily2 as 
    select b.studyepid, b.studyid, b.patepid, b.epid, a.*
    from tmp.daily1 as a left join tmp.episode as b
    on a.patid=b.patid and a.date between b.admission_date and b.discharge_date
    order by studyid, epid, day;
  quit;

  %if &debug=Y %then %do;
    proc print data=tmp.daily2 (obs=40);
      var patid epid date day studyepid ;
    run;
  %end;

  /* get number of dx vars */
  proc sql noprint;
    /* select count(name) 
    into :Npdx trimmed
    from sashelp.vcolumn
    where upcase(libname)="INDATA" and upcase(memname)="%upcase(&episode)"
          and upcase(name) like 'PRINCIPLE_DIAGNOSIS_%';
*/
    select count(name) 
    into :Nadx trimmed
    from sashelp.vcolumn
    where upcase(libname)="%upcase(&lib)" and upcase(memname)="%upcase(&epdsn)"
          and upcase(name) like 'ADMITTING_DIAGNOSIS_%';

    select count(name) 
    into :Ndx trimmed
    from sashelp.vcolumn
    where upcase(libname)="%upcase(&lib)" and upcase(memname)="%upcase(&epdsn)"
          and upcase(name) like 'DIAGNOSIS_%' and upcase(name) ne 'DIAGNOSIS_CODE_TYPE';
  quit;

  /* %put &=Npdx;*/
  %put &=Nadx;
  %put &=Ndx;

 
 /* count number of non-missing dxs; create dataset of patients with 0 dxs*/
  data tmp.dx_0;
      set tmp.episode (keep=patepid admitting_diagnosis_: /* principle_diagnosis_:*/ diagnosis_:) ;
      
      array d admitting_diagnosis_1-admitting_diagnosis_&Nadx /* principle_diagnosis_1-principle_diagnosis_&Npdx */
              diagnosis_1-diagnosis_&Ndx;
      dxct=0;
      do i=1 to dim(d);
          if not missing(d{i}) then dxct+1;
      end;

      if dxct=0 then output;
  run;


  /* remove study exclusions */  
  proc sql;
    title "N episodes with 0 dxs";
    select count(distinct patepid)
    from tmp.dx_0;

    create table lookup_svc_exc as 
    select distinct patepid
    from tmp.daily2
    where upcase(service_group)='EXCLUDE';

    title "N episodes service_group=EXCLUDE";
    select count(distinct patepid)
    from lookup_svc_exc;

    create table day2psyc as 
    select distinct patepid
    from tmp.daily2
    where day=2 and upcase(service_group)="PSYCHIATRY"
    order by patepid;

    title "N episodes day2 service_group=PSYCHIATRY";
    select count(*)
    from day2psyc;

    /* remove service=exclude */
    create table tmp.episode_exc_1 as 
    select *
    from tmp.episode
    where patepid not in (select patepid from lookup_svc_exc)
    order by patepid;

    /* remove day2 service_group=PSYCHIATRY */
    create table tmp.episode_exc_2 as 
    select *
    from tmp.episode_exc_1
    where patepid not in (select patepid from day2psyc)
    order by patepid;

    /* remove 0 dxs */
    create table tmp.episode_exc_3 as 
    select *
    from tmp.episode_exc_2
    where patepid not in (select patepid from tmp.dx_0)
    order by patepid;

    title "N episodes age < 18 or gender exclusion";
    select count(*)
    from tmp.episode
    where enc_age lt 18 or missing(gender) or upcase(gender) = 'UNKNOWN' ;

    create table tmp.episode_exc_4 as 
    select * 
    from tmp.episode_exc_3
    where enc_age ge 18 and not missing(gender) and upcase(gender) ne 'UNKNOWN' 
      and hosplos ge 3;


    create table derived.population_daily as 
    select *
    from tmp.daily2
    where patepid in (select patepid from tmp.episode_exc_4)
    order by patid, epid, day;

    title "Nobs derived.population_daily (all exclusions removed, hosplos >=3)";
    select count(*)
    from derived.population_daily;

quit;
  
  /* add dxct var */
  data tmp.episode_exc_5;
    set tmp.episode_exc_4; 
    
    array d admitting_diagnosis_1-admitting_diagnosis_&Nadx /* principle_diagnosis_1-principle_diagnosis_&Npdx*/ 
              diagnosis_1-diagnosis_&Ndx;
      dxct=0;
      do i=1 to dim(d);
        if not missing(d{i}) then dxct+1;
      end;

      label dxct = "Count of non-missing dx vars";

      drop i;
  run;

  proc sort data=tmp.episode_exc_5 out=derived.population_episode; by patid epid; run;

  title "Nobs derived.population_episode (all exclusions removed, hosplos >=3)";
  proc sql;
    select count(*)
    from derived.population_episode
  quit;

  /* some checks */
  proc means data=derived.population_episode maxdec=2 nolabels;
    var enc_age hosplos dxct ;
  run;
  
  title "service_group on day 2 - should not include PSYCHIATRY";
  proc freq data= derived.population_daily;
    where day=2;
    table service_group;
  run;

  title "device frequecy";
  proc freq data=derived.population_daily;
    table firstdevice_type lastdevice_type;
  run;

  title "missing patepid";
  proc sql;
    select count(*)
    from derived.population_episode
    where missing(patepid);
  quit;
  title;

  proc contents data=derived.population_episode;
  run;
  
  proc contents data=derived.population_daily;
  run;

  /* delete all temporary datasets */
  %if &debug=N %then %do;
    proc datasets library=tmp kill;
    run;
  %end;


%mend;
