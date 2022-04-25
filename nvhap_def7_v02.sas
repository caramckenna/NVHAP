/***************************************************************************************/
/* PURPOSE:       Macro to create dataset with NV-HAP candidate definition #7 population:
/*                Meets Def1 and Def2 w/ chest imaging and either WBC or fever critera.
/* AUTHOR:        Cara McKenna    
/* CREATION DATE: 11/15/19
/*---------------------------------------------------------------------------------------
/* PARAMETERS:    tempunit (r): Temperature unit of measurement 
/*                                F: Fahrenheit 
/*                                C: Celsius
/*                wbcunit  (r): WBC unit of measurement
/*                                mm3: cells per cubic millimeter e.g. 12,000 
/*                                kmm3: thousand cells per cubic millimeter e.g. 12
/*                                 
/*---------------------------------------------------------------------------------------
/*  CHANGE LOG: 
/*
/*   Version   Date       Initials	 Comment 
/*   -------   --------   --------   ----------------------------------------------------
/*   1.0       11/15/19   CM         Initial Version. 
/*
/*   2.0       02/05/2021 CM         Added def1ID. Output def1_startDT,def2_startDT,
/*                                   def7_startDT.
/***************************************************************************************/
%macro nvhap_def7(tempunit=,wbcunit=,debug=N);
	
	%let version=02;
	%let program=nvhap_def7_v&version;
	%put =====> MACRO CALLED: &program;

	data _null_;
      title;
      file print;
      put "Created on %sysfunc(date(),worddate.) at %sysfunc(time(),timeampm.)";
      put "By &program";
    run;
	

  	/* get temp, wbc, and chest_imaging from day=def1_start and day=def1_start+1*/
  	proc sql;
  		create table tmp.def7_rdday as 
  			select a.*, b.day, b.date, b.max_temp, b.max_wbc, b.min_wbc, b.chest_imaging
  			from derived.nvhap_def2 as a left join derived.population_daily as b 
  			on a.patepid=b.patepid and (b.day=a.def1_start or b.day=def1_start+1)
  			order by patepid, def1ID, /* def1_start,*/ day;
  	quit;

  	title "Patients meeting def 1 & def 2 criteria";
  	proc means data=tmp.def7_rdday n nmiss min max median maxdec=2 nolabels;
  		var min_wbc max_wbc max_temp;
  	run;

  	/* dataset tmp.def7_rdday has 2 records per def1_start */
	data tmp.nvhap_def7_1;
		set tmp.def7_rdday;
		by patepid def1ID /* def1_start*/;

		retain def7_start def7_end def7_startDT;
		

		/* convert celsius to fahrenheit */
		%if %upcase(&tempunit)=C %then %do;
			max_Ftemp = 1.8*max_temp + 32;
		%end;
		%else %do;
			max_Ftemp = max_temp;
		%end;

		%if %upcase(&wbcunit)=MM3 %then %do;
			K_min_wbc= min_wbc/1000;
			K_max_wbc= max_wbc/1000;
		%end;
		%else %do;
			K_min_wbc= min_wbc;
			K_max_wbc= max_wbc;
		%end;

		if max_Ftemp > 100.4 or K_max_wbc > 12 or (not missing(K_min_wbc) and K_min_wbc < 4) then temp_wbc=1;
		lag_wbc=lag(temp_wbc);
		lag_image=lag(chest_imaging);

		if first.def1ID then do;
			call missing(def7_start,def7_end,def7_startDT,lag_wbc,lag_image);
			if chest_imaging=1 and temp_wbc=1 then do;
				def7_start=day;
				def7_end=day;
				def7_startDT=date;
				output;
			end;
			else if chest_imaging=1 or temp_wbc=1 then do;
				def7_start=day;
				def7_startDT=date;
			end;
		end;
		else if missing(def7_end) and (chest_imaging=1 or lag_image=1) and (temp_wbc=1 or lag_wbc=1) then do;
			if missing(def7_start) then do;
				def7_start=day;
				def7_startDT=date;
			end;
			def7_end=day;
			output;
		end;

		format def7_startDT MMDDYY10.;

		label def7_start = "Candidate Definition #7 Start Day"
		      def7_end   = "Candidate Definition #7 End Day"
		      def7_startDT = "Candidate Definition #7 Start Date";
	run;

	/* check log for 0 dups*/
	proc sort data=tmp.nvhap_def7_1 nodupkey; by patepid def1_start def7_start; run;

	proc sql;
		create table derived.nvhap_def7 (label="NV-HAP Candidate Definition #7") as 
		select distinct siteid, patepid, patid, epid, def1ID, def1_start, def2_start, 
		               def7_start, def7_end, def1_startDT,def2_startDT,def7_startDT, enc_death, con_abxdays 
		from tmp.nvhap_def7_1
		order by patepid, def1ID;

		title "N Def 7 Events";
		select count(*)
		from derived.nvhap_def7;
	quit ;
	title;

	proc contents data=derived.nvhap_def7 varnum;
	run;
	
	/* delete all temporary datasets */
	%if &debug=N %then %do;
    	proc datasets library=tmp kill;
      	run;
    %end;

    %put ********END OF MACRO: &program********;

%mend nvhap_def7;
