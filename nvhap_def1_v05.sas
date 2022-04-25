/***************************************************************************************/
/* PURPOSE:       Macro to create dataset with NV-HAP candidate definition #1 population:
/*                respiratory deterioration population
/* AUTHOR:        Cara McKenna    
/* CREATION DATE: 11/21/19
/*---------------------------------------------------------------------------------------
/* PARAMETERS:    r=required o=optional
/*
/*                devicefile (o): Filename (with extension) to output cross tab of 
/*                                respiratory deterioration devices to excel file.
/*                                E.g. checkdevices.xlsx 
/*---------------------------------------------------------------------------------------
/*  CHANGE LOG: 
/*
/*   Version   Date       Initials	 Comment 
/*   -------   --------   --------   ----------------------------------------------------
/*   1.0       11/21/19   CM         Initial Version. 
/*   2.0       12/03/20   CM         Fixed issue with program flagging no device and spo2 
/*                                   >= 95% -> no device and missing spo2 as worsening O2.
/*                                   Changed baseline same or improvement -> higher device code
/*                                   from baseline1>1 to baseline1>=1 and added baseline2 > 0.
/*   3.0       02/05/21   CM         Exclude patients with LAST device = ventilator on baseline
/*                                   day 1. Added eof indicator to tmp.base1 to prevent loss of 
/*                                   last day. Removed 14 day filter between def1 events to allow
/*                                   for multiple def1 events within 14 days per hospitalization.
/*                                   Added def1ID to identify multiple def1 event within 
/*                                   hospitalization. Output def1startDT.
/*
/*   4.0       02/19/21   CM         1. Added NASAL CANNULA,lower device -> higher devices criteria 
/*                                   2. All escalations to higher devices have to be greater than
/*                                      last device on the first baseline day
/*                                   3. Rearranged baseline groupings
/*                                   4. Renamed rdnum format to blnum 
/*                                   5. Fixed no device & max_spO2 >= 95 -> no device & max_spO2 < 95
/*                                   6. Added no device & max_spO2 >= 95 -> no device & max_spO2 < 95, device
/*                                   7. Any escalations to higher devices: rd2 >= rd1
/*
/*	5.0        03/01/21   CM         1. Removed basegroup and created baseline1spO2 and baseline2spO2.
/*                                      Groupings no longer work because no device & max_spO2 >= 95 +
/*                                      no device & missing max_spO2 ok baseline, but
/*                                      no device & max_spO2 >= 95 + no device & max_spO2 between 90-94
/*                                      would be an escalation so not ok as baseline.                  
/***************************************************************************************/
%macro nvhap_def1(debug=N,devicefile=);
	%let version=05;
	%let program=nvhap_def1_v&version;
	%put =====> MACRO CALLED: &program;

	data _null_;
      title;
      file print;
      put "Created on %sysfunc(date(),worddate.) at %sysfunc(time(),timeampm.)";
      put "By &program";
    run;

    proc format;
    	value $devord
    	    ' '                        = 1
    		'NONE'                     = 1
    		'NASAL CANNULA'            = 3
    		'SIMPLE MASK'              = 4
    		'OXYGEN CONSERVING DEVICE' = 5
    		'NON-REBREATHER'           = 6
    		'HIGH FLOW'                = 7
    		'BIPAP'                    = 8
    		'VENTILATOR'               = 9
    		other                      = 0
    		;

    	value devnum
    		1 = 'NONE/MISSING'
			3 = 'NASAL CANNULA'
			4 = 'SIMPLE MASK'
			5 = 'OXYGEN CONSERVING DEVICE'
			6 = 'NON-REBREATHER'
			7 = 'HIGH FLOW' 
			8 = 'BIPAP'
			9 = 'VENTILATOR'

    ;
 	

    /* dataset to start with */
	%let indata=derived.population_daily;

	data tmp.base;
    	set &indata (keep=siteid patepid patid epid studyid studyepid day date max_spO2 med_O2FlRt firstdevice_type lastdevice_type);
    	by patid epid;
    	if eof=0 then set &indata (firstobs=2 keep=day date max_spO2 lastdevice_type med_O2FlRt 
                                rename=(day=next_day date=next_date max_spO2=next_max_spO2 med_O2FlRt=next_med_O2FlRt
			                    	    lastdevice_type=next_ld)) end=eof;
      
      if last.epid then call missing(next_day,next_date,next_max_spO2,next_ld,next_med_O2FlRt);

      firstdevicen = input(put(strip(upcase(firstdevice_type)), $devord.),3.);
	  lastdevicen = input(put(strip(upcase(lastdevice_type)), $devord.),3.);
	  nextlastdevicen=input(put(strip(upcase(next_ld)), $devord.),3.);

    run;

	/* check all devices were mapped  */ 
	title "If any = 0 then something is not mapped correctly";
 	proc freq data=tmp.base;
 		table firstdevicen lastdevicen;
 	run;

	proc sort data=tmp.base; by patid epid day; run; 

	/* look for respiratory deterioration */
	data tmp.def1;
		set tmp.base;

		/* these will overlap patients or episodes so only use with day condition */
		baseline1=lag2(firstdevicen);
		baseline2=lag(firstdevicen);

		baseline1spO2=lag2(max_spO2);
		baseline2spO2=lag(max_spO2);

		baselineO2FlRt_1=lag2(med_O2FlRt);
		baselineO2FlRt_2=lag(med_O2FlRt);

		rd1=lastdevicen;
		rd2=nextlastdevicen;

		/* last device on first baseline (rd -2) */
		baseline1LD=lag2(lastdevice_type);
		baseline1LDn=lag2(lastdevicen);

		/* rd2 could be missing but nothing can be < missing so ok */
		if day ge 3 then do;
			rd_day=day;
			rd_date=date;

			/* NO DEVICE */
			if baseline1=1 and baseline2=1 then do;
				/* max_spO2 >= 95 -> max_spO2 < 95 */
				if baseline1spO2 ge 95 and baseline2spO2 ge 95 
					and max(rd1,rd2)=1 and not missing(max_spO2) and not missing(next_max_spO2)
					and max(max_spO2,next_max_spO2) < 95 then output; 

				/* max_spO2 >= 95 -> max_spO2 < 95, device */
				else if baseline1spO2 ge 95 and baseline2spO2 ge 95 
					and rd1=1 and rd2 >1 and not missing(max_spO2) and max_spO2 < 95 
					and rd2 > baseline1LDn then output;

				/* max_spO2 >= 95 -> devices */
				else if baseline1spO2 ge 95 and baseline2spO2 ge 95 
					and rd1 > 1 and rd2 > 1 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then output;

				/* max_spO2 >= 95 + missing max_spO2 -> devices */
				else if baseline1spO2 ge 95 and missing(baseline2spO2)
					and rd1 > 1 and rd2 > 1 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then output;

				/* max_spO2 < 95 or missing -> devices */
				else if (missing(baseline1spO2) or (baseline1spO2 ge 90 and baseline1spO2 lt 95)) 
					and (baseline2spO2 ge 90 or missing(baseline2spO2)) 
					and rd1 > 1 and rd2 > 1 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then output;

			end;

			/* NASAL CANNULA,lower device ->  */
			/* When one baseline device is NONE, only use flowrate from NASAL CANNULA day*/   
			/* [CM20210219] max_spO2 can be missing in this case but if not missing  
			   has to be >= 90 */
			else if baseline1=3 and baseline2=1 and (missing(baseline2spO2) or baseline2spO2 >= 90) then do;
				med_O2FlRt_ch=med_O2FlRt-baselineO2FlRt_1;
				/* NASAL CANNULA,NASAL CANNULA */
				if rd1=3 and rd2=3 and med_O2FlRt-baselineO2FlRt_1 ge 3 and next_med_O2FlRt-baselineO2FlRt_1 ge 3 then output;
				
				/* NASAL CANNULA,higher device */
				else if rd1=3 and rd2 > 3 and med_O2FlRt-baselineO2FlRt_1 ge 3 and rd2 > baseline1LDn then output;		

				/* higher devices */
				else if rd1 > 3 and rd2 > 3 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then do;
					med_O2FlRt_ch=.;
					output;
				end;
			end;

			/* NASAL CANNULA,NASAL CANNULA -> */                                                                                          
			else if baseline1=3 and baseline2=3 and baselineO2FlRt_1 ge baselineO2FlRt_2 then do;
				med_O2FlRt_ch=med_O2FlRt-baselineO2FlRt_1;
				/* NASAL CANNULA,NASAL CANNULA */       
				if rd1=3 and rd2=3 and med_O2FlRt-baselineO2FlRt_1 ge 3 and next_med_O2FlRt-baselineO2FlRt_1 ge 3 then output;
				
				/* NASAL CANNULA,higher device */
				else if rd1=3 and rd2 > 3 and med_O2FlRt-baselineO2FlRt_1 ge 3 and rd2 > baseline1LDn then output;

				/* higher devices*/
				else if rd1 >3 and rd2 >3 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then do;
					med_O2FlRt_ch=.;
					output;
				end;
			end;

			/* SIMPLE MASK, lower device -> */
			/* [CM20210219] max_spO2 can be missing in this case but if not missing  
			   has to be >= 90 */
			else if baseline1=4 and (baseline2=3 or (baseline2=1 and (missing(baseline2spO2) or baseline2spO2 >= 90))) then do;
				med_O2FlRt_ch=med_O2FlRt-baselineO2FlRt_1;
				/* SIMPLE MASK,SIMPLE MASK */
				if rd1=4 and rd2=4 and med_O2FlRt-baselineO2FlRt_1 ge 4 and next_med_O2FlRt-baselineO2FlRt_1 ge 4 then output;
				
				/* SIMPLE MASK,higher device */
				else if rd1=4 and rd2 > 4 and med_O2FlRt-baselineO2FlRt_1 ge 4 and rd2 > baseline1LDn then output;

				/* higher devices */
				else if rd1 >4 and rd2 >4 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then do;
					med_O2FlRt_ch=.;
					output;
				end;
			end;

			/* SIMPLE MASK,SIMPLE MASK ->  */
			else if baseline1=4 and baseline2=4 and baselineO2FlRt_1 ge baselineO2FlRt_2 then do;
				med_O2FlRt_ch=med_O2FlRt-baselineO2FlRt_1;
				/* SIMPLE MASK,SIMPLE MASK*/
				if rd1=4 and rd2=4 and med_O2FlRt-baselineO2FlRt_1 ge 4 and next_med_O2FlRt-baselineO2FlRt_1 ge 4 then output;
					
				/* SIMPLE MASK,higher device */
				else if rd1=4 and rd2 > 4 and med_O2FlRt-baselineO2FlRt_1 ge 4 and rd2 > baseline1LDn then output;

				/* higher devices */
				else if rd1 >4 and rd2 >4 and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn then do;
					med_O2FlRt_ch=.;
					output;
				end;
			end;
			

			/* baseline same or improvement -> higher device */
			else if baseline1 >=3 
				and baseline1 >= baseline2 /* same or improvement*/ 
				and (baseline2 >1 or (baseline2=1 and (missing(baseline2spO2) or baseline2spO2 >= 90))) /* for NONE spO2 either missing or >= 90 */
				and max(baseline1,baseline2) < min(rd1,rd2) 
				and rd2 >= rd1 and min(rd1,rd2) > baseline1LDn 
					then output;

		
		end;
		format baseline1 baseline2 /* blnum.*/ rd1 rd2 baseline1LDn devnum.
		       rd_date MMDDYY10.;
	run;

	/* exclude patients with last device = VENT on baseline day 1 (rd -2) */
	data tmp.def1_2;
		set tmp.def1;
		if strip(baseline1LD) ne "VENTILATOR";
	run;

	%if &debug=Y %then %do;
		title1 "none -> none";
		title2 "max should be < 95";
		proc means data=tmp.def1_2 nolabels maxdec=2 n nmiss min max;
			where rd1=1 and rd2=1;
			var max_spO2 next_max_spO2;
		run;

		title1 "none -> none, NC";
		title2 "max should be < 95";
		proc means data=tmp.def1_2 nolabels maxdec=2 n nmiss min max;
			where rd1=1 and rd2=3;
			var max_spO2;
		run;

		proc print data=tmp.def1_2;
			where rd1=1 and rd2=1 and max_spO2 > 95;
		run;

		title "nasal cannula -> nasal cannula";
		proc means data=tmp.def1_2 nolabels maxdec=2;
			where rd1=3 and rd2=3;
			var med_O2FlRt_ch;
		run;

		proc print data=tmp.def1_2;
			where baseline1=3 and baseline2 <= 3 and rd1=3 and rd2=3 and next_med_O2FlRt lt med_O2FlRt;
			var patid epid baseline1 baseline2 rd1 rd2 med_O2FlRt baselineO2FlRt_1 baselineO2FlRt_2 next_med_O2FlRt;
		run;

		title "simple mask -> simple mask";
		proc means data=tmp.def1_2 nolabels maxdec=2;
			where rd1=4 and rd2=4;
			var med_O2FlRt_ch;
		run;
	%end;

	/* print crosstab of RD devices */
	%if %length(&devicefile) > 0 %then %do;
		/* should have no missing */
		ods excel file="&devicefile";
			proc freq data=tmp.def1_2;
				table baseline1*baseline2*rd1*rd2 / list nocum;
			run;

			proc means n nmiss min max;
				where baseline1=1 or baseline2=1;
				var baseline1spO2 baseline2spO2 max_spO2 next_max_spO2;
			run;

			proc freq data=tmp.def1_2;
				where baseline1=1 or baseline2=1 or rd1=1 or rd2=1;
				table baseline1*baseline1spO2*baseline2*baseline2spO2*rd1*max_spO2*rd2*next_max_spO2 / missing list nocum;
			run;
			

		ods excel close;
	%end;
	
	proc sort data=tmp.def1_2; by patepid rd_day; run;

	/* calculate days between multiple events per hospital episode */
	/* data tmp.def1_2 ;
		set tmp.def1;
		by patepid;

		diff=day-lag(day);

		if first.patepid or patepid ne lag(patepid) then call missing(diff);

	run;*/
	
	/* add def1 identifier */
	data tmp.def1_3 ;
		set tmp.def1_2;
		def1ID+1;
		by patepid;


		if first.patepid then def1ID=1;

	run;
	

	/* final dataset; one record per respiratory deterioration event */
	proc sql;
		create table derived.nvhap_def1 as 
		select siteid, patepid, patid, epid, studyid, studyepid, 
		       def1ID label= 'Candidate Definition #1 ID',
		       rd_day as def1_start label='Candidate Definition #1 Start Day',  
		       rd_day+1 as def1_end label='Candidate Definition #1 End Day', 
		       rd_date as def1_startDT label='Candidate Definition #1 Start Date' format=MMDDYY10.,  
		       rd_date+1 as def1_endDT label='Candidate Definition #1 End Date' format=MMDDYY10., 
		       put(baseline1, devnum.) as baseline1 label='Baseline Day 1 Device', 
		       put(baseline2, devnum.) as baseline2 label='Baseline Day 2 Device',  
		       put(rd1, devnum.) as rd1 label='Respiratory Deterioration Day 1 Device',
		       put(rd2, devnum.) as rd2 label='Respiratory Deterioration Day 2 Device',
		       baseline1spO2 label='Baseline Day 1 Max Sp02',
		       baseline2spO2 label='Baseline Day 2 Max Sp02',
		       max_spO2 as rd1_max_spO2 label='Respiratory Deterioration Day 1 Max Sp02',
		       next_max_spO2 as rd2_max_spO2 label='Respiratory Deterioration Day 2 Max Sp02',
		       baselineO2FlRt_1 as baseline1_med_O2FlRt label='Baseline Day 1 Median Oxygen Flow Rate', 
		       baselineO2FlRt_2 as baseline2_med_O2FlRt label='Baseline Day 2 Median Oxygen Flow Rate', 
		       med_O2FlRt as rd1_med_O2FlRt label='Respiratory Deterioration Day 1 Median Oxygen Flow Rate', 
		       next_med_O2FlRt as rd2_med_O2FlRt label='Respiratory Deterioration Day 2 Median Oxygen Flow Rate',
		       med_O2FlRt_ch label='Median Oxygen Flow Rate Change from baseline' 
		from tmp.def1_3
		/* where missing(diff) or diff >14*/
		order by patepid,def1ID;


     title "N def1 events";
    select count(*)
    from derived.nvhap_def1 ;
	quit;	

	title;
	proc contents data=derived.nvhap_def1 varnum;
	run;

	 /* delete all temporary datasets */
	%if &debug=N %then %do;
		proc datasets library=tmp kill;
		run;
	%end;

    %put ********END OF MACRO: &program********;
%mend;

