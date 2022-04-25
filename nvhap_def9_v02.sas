/***************************************************************************************/
/* PURPOSE:       Macro to create dataset with NV-HAP candidate definition #9 population:
/*                Meets Def1 and Def2 w/ either chest imaging or pulmonary culture and 
/*                either WBC or fever critera.
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
/*                                   def9_startDT.
/***************************************************************************************/
%macro nvhap_def9(tempunit=,wbcunit=,debug=N);
	
	%let version=02;
	%let program=nvhap_def9_v&version;
	%put =====> MACRO CALLED: &program;

	data _null_;
      title;
      file print;
      put "Created on %sysfunc(date(),worddate.) at %sysfunc(time(),timeampm.)";
      put "By &program";
    run;
	

  	/* get temp, wbc, and chest_imaging from day=def1_start and day=def1_start+1*/
  	proc sql;
  		create table tmp.def9_rdday as 
  			select a.*, b.day, b.date, b.max_temp, b.max_wbc, b.min_wbc, b.chest_imaging, b.pul_specimen
  			from derived.nvhap_def2 as a left join derived.population_daily as b 
  			on a.patepid=b.patepid and (b.day=a.def1_start or b.day=def1_start+1)
  			order by patepid, def1ID, /* def1_start,*/ day;
  	quit;

  	title "Patients meeting def 1 & def 2 criteria";
  	proc means data=tmp.def9_rdday n nmiss min max median maxdec=2 nolabels;
  		var min_wbc max_wbc max_temp;
  	run;

  	/* dataset tmp.def9_rdday has 2 records per def1_start */
	data tmp.nvhap_def9_1;
		set tmp.def9_rdday;
		by patepid def1ID /* def1_start*/;

		retain def9_start def9_end def9_startDT;
		

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
		if chest_imaging=1 or pul_specimen=1 then image_spec=1;

		lag_tmpwbc=lag(temp_wbc);
		lag_imagespec=lag(image_spec);

		if first.def1ID then do;
			call missing(def9_start,def9_end,def9_startDT,lag_tmpwbc,lag_imagespec);
			if image_spec=1 and temp_wbc=1 then do;
				def9_start=day;
				def9_end=day;
				def9_startDT=date;
				output;
			end;
			else if image_spec=1 or temp_wbc=1 then do;
				def9_start=day;
				def9_startDT=date;
			end;
		end;
		else if missing(def9_end) and (image_spec=1 or lag_imagespec=1) and (temp_wbc=1 or lag_tmpwbc=1) then do;
			if missing(def9_start) then do;
				def9_start=day;
				def9_startDT=date;
			end;
			def9_end=day;
			output;
		end;

		format def9_startDT MMDDYY10.;

		label def9_start = "Candidate Definition #9 Start Day"
		      def9_end   = "Candidate Definition #9 End Day"
		      def9_startDT = "Candidate Definition #9 Start Date";
	run;

	proc sort data=tmp.nvhap_def9_1 nodupkey; by patepid def1_start def9_start; run;

	proc sql;
		create table derived.nvhap_def9 (label="NV-HAP Candidate Definition #9") as 
		select siteid, patepid, patid, epid, def1ID, def1_start, def2_start, def9_start, def9_end,
					def1_startDT,def2_startDT,def9_startDT,enc_death, con_abxdays
		from tmp.nvhap_def9_1
		order by patepid, def1ID;

		title "N Def 9 Events";
		select count(*)
		from derived.nvhap_def9;
	quit ;
	title;

	proc contents data=derived.nvhap_def9 varnum;
	run;
	
	/* delete all temporary datasets */
	%if &debug=N %then %do;
    	proc datasets library=tmp kill;
      	run;
    %end;

    %put ********END OF MACRO: &program********;

%mend nvhap_def9;
