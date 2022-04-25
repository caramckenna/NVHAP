/***************************************************************************************/
/* PURPOSE:       Macro to create dataset with patients meeting nvhap definition # 2:
/*                new abx started on the first day of respiratory deterioration or the 
/*                day following respiratory deterioration and continued for at least 3 
/*                calendar days. 
/* AUTHOR:        Cara McKenna      
/* CREATION DATE: 11/12/19
/*---------------------------------------------------------------------------------------
/* PARAMETERS:    r=required o=optional
/*
/*                abxlist (r): List of antibiotics to use to check for new abx. Must be 
/*                             in xlsx file with sheet name 'antibiotics' and column 
/*                             names antibiotic_name and 'include'.
/*---------------------------------------------------------------------------------------
/*  CHANGE LOG: 
/*
/*   Version   Date         Initials   Comment 
/*   -------   --------     --------   ----------------------------------------------------
/*   1.0       11/21/2019   CM         Initial Version. 
/*
/*   2.0       01/27/2020   CM         - Replaced libname map xlsx to be compatible with SAS 9.3
/*                                     - Added code to check antibiotic route and only include 
/*                                       abx with IV or PO route. 
/*                                     - compressed abx names to allow for different delimeter types
/*
/*   2.1       02/27/2020   CM         Replaced dbms=excel with dbms=xlsx for import. This was 
/*                                     causing an issue on HCA server.   
/*
/*   3.0       02/05/2021   CM         Added def1ID to allow for multiple events within 
/*                                     hospitalization. Output startDTs.
/*   
/*   4.0       03/23/2021   CM         Updated code to only count new abxs as consecutive
/*                                     abx days. 
/*
/*   5.0       04/12/2021   CM         - Fixed issue with input data abx = "vancomycin - [route]"
/*                                       not counting skip days. 
/*                                     - Removed code allowing for cefepime skip days
/*                                     - Only IV vancomycin counts towards consecutive
/*                                       abx days. 
/*                                     - Removed tot_abx from abx_1
/*   
/*   5.1       04/21/2021   CM         - Fixed issue with multiple new abx not being counted 
/*                                       as consecutive.
/*                                     - Changed acceptable days of abx if patient died to >=
/***************************************************************************************/
%macro nvhap_def2(abxlist=,debug=N,checknum=15);
  
  %let version=05_01;
  %let program=nvhap_def2_v&version;
  %put =====> MACRO CALLED: &program;

  data _null_;
      title;
      file print;
      put "Created on %sysfunc(date(),worddate.) at %sysfunc(time(),timeampm.)";
      put "By &program";
    run;
	
  %put Using mapping file &abxlist;

  /* import list of abx used to treat pneumonia */
  proc import out=abx_list
    datafile="&abxlist"
    dbms=xlsx replace;
  run;


	proc sql noprint;
		/* lookup table of abx to include */
		create table include_abx as 
		select strip(upcase(antibiotic_name)) as antibiotic_name
		from abx_list
		where include = 1;

		/* make indicator variables */
		select upcase(strip(compress(antibiotic_name,,'ak')))
		into :ABXVARSx separated by ' '
		from include_abx
		where not missing(antibiotic_name) and upcase(antibiotic_name) not like '%VANCOMYCIN%';

		/* add 1 for VANCOMYCIN */
		select count(*)+1
		into :NABX trimmed
		from include_abx
		where not missing(antibiotic_name) and upcase(antibiotic_name) not like '%VANCOMYCIN%';

	    select max(length(antibiotic_name))
	    into :ABXL trimmed
	    from include_abx
	    where not missing(antibiotic_name);
	quit ;

	/* add vancomycin here so we only have one vancomycin variable */
	%let ABXVARS = &ABXVARSx VANCOMYCIN;

	%put &=ABXVARS;
  	%put &=ABXL;

  proc sql;
    create table def1_daily as 
    select a.def1_start, a.def1_startDT, a.def1_end, a.def1ID, b.*
    from derived.nvhap_def1 as a left join derived.population_daily as b
    on a.patepid = b.patepid
    order by patepid,def1ID,date;
  quit;


    /* temp datasets in case there are issues to figure out*/
    /* 1 - wide dataset with abx indicators
       2 - wide dataset with skip days flagged (levoflg)*/

	 /* dataset with abx indicators */
	data tmp.abx_1;
		set def1_daily (keep=siteid patepid patid epid def1ID date day def1_start def1_startDT antibiotic_1-antibiotic_7 route_1-route_7);
		length abxs $220;

		/* put abx in one variable to make easier to check data */
    abxs=catx(';', of antibiotic_1-antibiotic_7); 

    /* initialize abx indicators - need to do this so all obs will have these vars set to 0 not missing */   
		%do i=1 %to &nabx;
			%let abxvar=%scan(&ABXVARS,&i,%quote( ));
			&abxvar=0;
    /* loop through abx vars */
		%do j=1 %to 7;

			/* multiple input vancomycin values allowed, ex vancomycin - [route] */
			/* only counting vancomycin if route = IV */
			if index(upcase(antibiotic_&j),"VANCOMYCIN") > 0 and strip(upcase(route_&J)) = 'IV' then VANCOMYCIN=1;

			else if upcase(strip(compress(antibiotic_&J,,'ak')))="&abxvar" and strip(upcase(route_&J)) in ('IV','PO') then &abxvar=1;

    	%end;
    	%end;

		/* array a &ABXVARS; */
		/* tot_abx=sum(of a(*)); */

	run;


	proc sort data = tmp.abx_1; by patepid def1ID day; run;

  
  /* Flag levofloxacin and vancomycin skip days */
  data tmp.abx_2;
    set tmp.abx_1;
    by patepid def1ID;
      
    if eof=0 then
      set tmp.abx_1 (firstobs=2 rename=(Levofloxacin=nextLevo Vancomycin=nextVanco
                                        patid=npatid epid=nepid def1ID=ndef1ID day=nday) 
                           keep=Levofloxacin Vancomycin patid epid def1ID day) end=eof;

      if last.def1ID then call missing(nextLevo,nextVanco,nextCefepime,npatid,nepid,nday);

      
      if nextLevo=1 and lag(Levofloxacin)=1 and Levofloxacin=0 and patid=npatid and epid=nepid and def1ID=ndef1ID and day=nday-1 then levoflg=1;
      else levoflg=0;
      
      if nextVanco=1 and lag(Vancomycin)=1 and Vancomycin=0 and patid=npatid and epid=nepid and def1ID=ndef1ID and day=nday-1 then Vancoflg=1;
      else Vancoflg=0;

    run;


    /* flag new abx */
    data abx_3 (keep=siteid patepid patid epid def1ID def1_startDT def1_start date day new abx levoflg Vancoflg);
        set tmp.abx_2 ;

        array a &ABXVARS;
       
        do i=1 to dim(a);
            if  a{i}=1 and lag(a{i})=0 and lag2(a{i})=0 then new = 1;
            else new = 0;
            
            if date >= def1_startDT and a{i} = 1 then do;
              abx = vname(a{i});
              output;
            end;

        end;

        /* output dummy data for skip days */
        if levoflg=1 and date >= def1_startDT then do;
          abx = "LEVOFLOXACIN";
          output;
        end;

        if Vancoflg=1 and date >= def1_startDT then do;
          abx = "VANCOMYCIN";
          output;
        end;

    run;

    
    proc sort data=abx_3 ;
      by patepid def1ID date abx;
    run;

    %let NEWABXL = %eval(&ABXL*8);
    %put &=NEWABXL;

    /* dataset with only abx that count towards consecutive abx days */
    data abx_4;
      set abx_3;
      by patepid def1ID;
      length new_abx $ &NEWABXL;

      if first.def1ID then do;
        new_abx="";
      end;

      retain new_abx;
      if new=1 then new_abx=cats(new_abx,';',abx,';');
      if new=1 or indexw(new_abx,strip(abx),';') > 0 then output; 

    run;

    proc sort data=abx_4;
      by patepid def1ID date;
    run;

  
    /* flag consecutive days of abx */
    data abx_5;
      set abx_4;
      by patepid def1ID; 

      lagDate=lag(date);

      if first.def1ID then do;
       lagDate=date;
       conAbxID=1;
      end;

      if date > lagDate+1 then conAbxID+1;
      format lagDate MMDDYY10.; 
    run;


    proc sql;
      /* summarize abx episodes */
      create table abx_6 as
        select siteid, patepid, patid, epid, conAbxID, def1ID, def1_startDT, def1_start,
          min(date) as abx_startDT format=MMDDYY10. label="New Antibiotic Start Date", 
          min(day) as abx_start                     label="New Antibiotic Start Day", 
          max(date) as abx_endDT format=MMDDYY10.   label="New Antibiotic End Date", 
          max(day) as abx_end                       label="New Antibiotic End Day", 
          count(distinct date) as con_abxdays       label="Total Consecutive Inpatient Antibiotic Days"
      from abx_5
      group by siteid, patepid, patid, epid, def1ID, def1_startDT, def1_start, conAbxID;

      create table abx_7 as 
        select * 
        from abx_6
        where abx_startDT = def1_startDT or abx_startDT = def1_startDT+1
       /*  select b.siteid, a.patepid, b.patid, b.epid, a.def1ID, b.def1_start, a.def1_startDT,
              a.abx_start, a.abx_startDT, a.abx_end, a.abx_endDT, a.con_abxdays  
      from abx_6 as a 
        inner join derived.nvhap_def1 as b
        on a.patepid = b.patepid and (a.abx_startDT = b.def1_startDT or a.abx_startDT = b.def1_startDT+1) */
      order by patepid;

      create table deathday as 
        select patepid, /* patid, epid, */ enc_death, hosplos as death_day label="Hospital Day Patient Died"
      from derived.population_episode
      where enc_death=1
      order by patepid;

    quit;


    data abx_8;
      merge abx_7   (in=a)
            deathday (in=b)
          ;
      by patepid;
      if a;
      if missing(enc_death) then enc_death=0;
    run;


    data final;
      set abx_8;

      def2_startDT=abx_startDT;
      def2_start=abx_start;

      if con_abxdays >= 3 then do;
        /* sent def2 end as min criteria to meet definition (3 days) */
        def2_end=abx_start+2;
        def2_endDT=abx_startDT+2;
        output;
      end;
      else if enc_death = 1 then do;
        def2_end=abx_end;
        def2_endDT=abx_endDT;
        /* day of death is on second day of impaired oxygenation or third day after the first day of impaired oxygenation */
        if death_day=def1_start+1 or death_day=def1_start+2 then do;
          /* antibiotics were stared on the first day of impaired oxygenation - 2 days of abx acceptable */
          if abx_startDT = def1_startDT and con_abxdays >= 2 then output;
          /* antibiotics were started on the second day of impaired oxygenation - 1 day of abx acceptable  */
          else if abx_startDT = def1_startDT+1 and con_abxdays >= 1 then output;
        end;

        /* day of death is on the fourth day after the first day of impaired oxygenation  */
        /* antibiotics were started on the second day of impaired oxygenation - 2 days of abx acceptable */
        else if death_day=def1_start+3 and abx_startDT = def1_startDT+1 and con_abxdays = 2 then output;
      
      end;

      label def2_start   = "Candidate Definition #2 Start Day"
            def2_startDT = "Candidate Definition #2 Start Date"
            def2_end     = "Candidate Definition #2 End Day"
            def2_endDT   = "Candidate Definition #2 End Date";
        
      format def2_startDT def2_endDT MMDDYY10.;
    run;


   	proc sort data=final out=derived.nvhap_def2; by patid epid def1ID; run;

	  proc contents data=derived.nvhap_def2 varnum; run;

	/* can use this to check death exceptions */
   	%if &debug=Y %then %do;
	   	title1 "death on second day of RD";
	   	title2 "abx started on rd day 1";
	   	title3 "2 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+1) and abx_start=def1_start;
	 	run;

	 	title2 "abx started on rd day 2";
	 	title3 "1 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+1) and abx_start=def1_start+1;
	 	run;

	 	title1 "death on day after second day of RD";
	 	title2 "abx started on rd day 1";
	 	title3 "2-3 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+2) and abx_start=def1_start;
	 	run;

	 	title2 "abx started on rd day 2";
	 	title3 "1-2 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+2) and abx_start=def1_start+1;
	 	run;

	 	title1 "death on +2 day after second day of RD";
	   	title2 "abx started on rd day 1";
	   	title3 "3-4 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+3) and abx_start=def1_start;
	 	run;

	 	title2 "abx started on rd day 2";
	 	title3 "2-3 ok";
	 	proc freq data=derived.nvhap_def2;
	 		table con_abxdays;
	 		where (death_day=def1_start+3) and abx_start=def1_start+1;
	 	run;

    title "Summary of consecutive abx days, patient did not die";
    proc means data=derived.nvhap_def2 maxdec=2 NOLABELS ;
      where enc_death=0;
      var con_abxdays;
    run; 
    title;

    /* check a random sample */
    proc surveyselect data=derived.nvhap_def2 
        method=SRS sampsize=&checknum /* seed=2468 */ out=RS;
    run;
    
    proc sql noprint;
        select patepid
        into :SIDS separated by ' '
        from RS;
    quit ;
    
    %do i=1 %to %sysfunc(countw(&SIDS));
      %let sid=%scan(&SIDS,&i,' ');
    
      title "&sid";
      proc print data=derived.nvhap_def2;
          where patepid="&sid";
      run;
    
      proc print data=derived.population_daily;
          where patepid="&sid";
          var patid epid day antibiotic_: ;
      run;
    
    %end;
	%end;

	
	/* delete all temporary datasets */
	%if &debug=N %then %do;
    	proc datasets library=tmp kill;
      run;
  %end;


%mend;
