**********************************
** &RequestID. sub-program only **
*********************************;

***************************Assign Libraries*****************************************************;

**%LET curHARSLibrary=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\;
**%LET curHARS=eHars_person;
**%LET curHARSLibLabs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\;
**%LET labdataset=eHars_labs;
**%LET curHARSLibFacs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\;
**%LET facildataset=eHARS_facilities;

/*
%LET curHARSLibrary=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\test_space\;
%LET curHARS=ehars_person;
%LET curHARSLibLabs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\test_space\;
%LET labdataset=eHars_labs;
%LET curHARSLibFacs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\test_space\;
%LET facildataset=eHARS_facilities;
*/
%LET curHARSLibrary=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\2019\;
%LET curHARS=ehars_person_2019_02_22;
%LET curHARSLibLabs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\2019\;
%LET labdataset=eHars_labs_2019_02_22;
%LET curHARSLibFacs=S:\EPI_Data\Communicable Diseases\HIV\ISDH eHARS datasets\Historical HIV Data from eHARS\2019\;
%LET facildataset=eHARS_facilities;


LIBNAME eHARS    "&curHARSLibrary." ;
LIBNAME LabeHARS "&curHARSLibLabs." ;
LIBNAME FaceHARS "&curHARSLibFacs." ;

***************************Assign Macro Variables*****************************************************;

%LET  Year=&Year.; ** Edit for year incidence and prevalence should be obtained;
%LET  SMD=01JAN; ** Edit for month and year of Inc/Prev START date ;
%LET  EMD=31DEC; ** Edit for month and year of Inc/Prev END date ;
 
%LET StartDt=&SMD.&Year.;
%LET EndDt=&EMD.&Year.;

%LET asofdate=&EndDt.; **Date that labs data are to date;
%LET prdendtxt=&EndDt.; **Update this macro variable to adjust the end date for data processing;
%LET prdbegtxt=&StartDt.; **Update this macro variable to adjust the start date for data processing;
%LET prevdttxt=&EndDt.; **Update this macro variable to adjust the date for analyzing TGA prevalence data;

%LET reportdttxt=&EndDt.; **Update this macro variable to reflect the report date;
%LET prdendtxt=&EndDt.; **Update this macro variable to adjust the end date for data reporting;
%LET yrs_rev=40; **Number of years to go back in reporting data;

**Create new macro variables off of the user defined macro variables;

DATA _NULL_;
  **End date for data processing;
  y = INPUT("&prdendtxt.", date9.); **Convert text date to a Julian date;
  yyr = YEAR(y); **Extract year from the date;
  CALL SYMPUT("prdenddt", STRIP(PUT(y, 5.))); **Store the Julian date as a macro variable;
  CALL SYMPUT("endyr", STRIP(PUT(yyr, 4.))); **Store the year as a macro variable;
  
  **start date for incidence processing - 1 year prior to &prdendtxt.;
  CALL SYMPUT("prdbegdt", STRIP(PUT(INTNX('year', y, -1, 'sameday')+1, 5.))); **Store the Julian date as a macro variable;
  CALL SYMPUT("begyr", STRIP(PUT(yyr, 4.))); **Store the year as a macro variable;
  CALL SYMPUT("prdbegtxt", STRIP(PUT(INTNX('year', y, -1, 'sameday')+1, DATE9.))); **Store the date as a text macro variable;
  
  **start date for reporting - 1 years prior to &prdendtxt.;
  CALL SYMPUT("repbegdt", STRIP(PUT(INTNX('year', y, -1, 'sameday')+1, 5.))); **Store the Julian date as a macro variable;
  CALL SYMPUT("repyr", STRIP(PUT(yyr, 4.))); **Store the year as a macro variable;
  CALL SYMPUT("repbegtxt", STRIP(PUT(INTNX('year', y, -1, 'sameday')+1, DATE9.))); **Store the date as a text macro variable;
RUN;

%PUT start date=&prdbegtxt. start year=&begyr. Julian date:&prdbegdt.; **Check macro variables;
%PUT end date=&prdendtxt. end year=&endyr. Julian date:&prdenddt.; **Check macro variables;
%PUT report start date=&repbegtxt. report start year=&repyr. Julian date:&repbegdt.; **Check macro variables;
  
**  2) Bring in eHARS formats and data and process data;

%LET numincyears=1; **Update this macro variable to adjust the number of years over which to count incident cases;
**As new data processing is deemed necessary, this file should always be updated and will thus always be the most up to date;
%INCLUDE "S:\EPI\Data Requests\DR2888 Master HIV Analytic and Template Programs\eHARS Data Processing.sas" ;

** Create new gender variable looking for change that would indicate transgender identity ;
** Create new eHARS_UID variable with * removed because it cannot be stored in RISE ;
** Apply formats to eHARS person data set ;

DATA &RequestID.eHARS ;
	SET curHARS_rev ;
	IF birth_sex = "F" AND current_gender IN ("CD","DQ","F","SM",".","")
		THEN sextgcnd = 1 ;
	IF birth_sex = "U" AND current_gender = "F"
		THEN sextgcnd = 1 ;
	IF birth_sex = "F" AND current_gender IN ("M","FM")
		THEN sextgcnd = 3 ;
	IF birth_sex = "F" AND current_gender = "I"
		THEN sextgcnd = 5 ;
	IF birth_sex = "F" AND current_gender = "MF"
		THEN sextgcnd = 3 ;
	IF birth_sex = "M" AND current_gender IN ("CD","DQ","M","SM",".","")
		THEN sextgcnd = 2 ;
	IF birth_sex = "U" AND current_gender = "M"
		THEN sextgcnd = 2 ;
	IF birth_sex = "M" AND current_gender IN ("F","MF")
		THEN sextgcnd = 3 ;
	IF birth_sex = "M" AND current_gender = "I"
		THEN sextgcnd = 5 ;
	IF birth_sex = "M" AND current_gender = "FM"
		THEN sextgcnd = 3 ;
	IF birth_sex = "U" AND current_gender = "I"
		THEN sextgcnd = 5 ;
	IF birth_sex = "U" AND current_gender IN (".","")
		THEN sextgcnd = 6 ;
  ** Combine exposure categories into VL protocol categories ;
	IF expo_categ IN ("01", "06") THEN Risk = 1 ; ** MSM, alone or in combination with heterosexual ;
	IF expo_categ IN ("02", "05") THEN Risk = 2 ; ** IDU, alone or in combination with heterosexual ;
	IF expo_categ IN ("03")       THEN Risk = 3 ; ** Heterosexual only ;
	IF expo_categ IN ("04", "07") THEN Risk = 4 ; ** MSM and IDU ;
	IF expo_categ IN ("08")       THEN Risk = 5 ; ** Perinatal ;
	IF expo_categ IN ("09")       THEN Risk = 6 ; ** Other ;
	IF expo_categ IN ("10", "11") THEN Risk = 7 ; ** Not Identified/Reported ;
  ** Drop * from eHARS_UID because it cannot be stored in RISE as per Scott Gardner;
	ehars_uid_rwg = COMPRESS(ehars_uid, "*", 'S');
	FORMAT sextgcnd sextgcnd. race_rec USCrace.
		   cur_age_num hiv_age_yrs_num aids_age_yrs_num death_age_yrs_num age.
		   rsh_county_name rsa_county_name cur_county_name $county.
		   risk risk. ;
	LABEL hiv_dx_dtdt           = "Date of First Positve HIV Test or Physician Dx"
		  dna_first_pos_dt      = "Date of First Positive Viral DNA Test"
          sextgcnd              = "Gender"
          cd4_recent_cnt_valueN = "Most Recent CD4 Count Value"
          cd4_recent_pct_valueN = "Most Recent CD4 Percent Value"
          vl_first_det_valueN   = "Value of First Detectable Viral Load"
          death_age_yrs_num     = "Age at Death" ;
RUN ;

**  Bring in and merge eHARS lab data ;
*************************************************;

%INCLUDE "&LogLstDir.&RequestID. eHARS Labs Sub-Prog.sas" ;

** Limit data set to only those summary variables necessary for viral load analysis ;

DATA HIVlabsDat (KEEP=document_uid MinTestDt MaxTestDt eHARS_calc_VLcount
                      eHARS_calc_VLstart eHARS_calc_VLend) ;
	SET HIVlabsStd ;
RUN ;

** Check for duplicates and merge flattened CD4/VL lab data with eHARS_Person data ;

PROC SORT DATA=&RequestID.eHARS NODUPKEY ;
	BY document_uid ;
RUN ;

** Link in labs data;
PROC SORT DATA=HIVlabsDat NODUPKEY ;
	BY document_uid ;
RUN ;

** Merge and process eHARS person and labs data sets for &Year. ;

DATA &RequestID.MergeDat (DROP=DOB) ;
	MERGE &RequestID.eHARS
          HIVlabsDat ;
	BY document_uid ;
   **convert dates from character variables to SAS dates;
  %LET char1 = aids_categ_7_rep_dt aids_categ_a_rep_dt antigen_first_pos_dt asymp_dt;   
  %LET numr1 = aids_categ_7_rep_dtdt aids_categ_a_rep_dtdt antigen_first_pos_dtdt asymp_dtdt; 
  %LET char2 = cd4_first_hiv_dt cd4_stage3_dt cur_address_dt last_med_eval_dt modify_dt;  
  %LET numr2 = cd4_first_hiv_dtdt cd4_stage3_dtdt cur_address_dtdt last_med_eval_dtdt modify_dtdt;
  %LET char3 = oi_stage3_dt phys_hiv_dt screen_last_neg_dt vl_first_det_dt vl_last_non_det_dt;  
  %LET numr3 = oi_stage3_dtdt phys_hiv_dtdt screen_last_neg_dtdt vl_first_det_dtdt vl_last_non_det_dtdt;
  ARRAY _char &char1. &char2. &char3.;
  ARRAY _numr &numr1. &numr2. &numr3.;
  DO over _char;
    tempchar = _char;
    IF SUBSTR(_char, 5, 2) = ".." THEN SUBSTR(tempchar, 5, 2) = "07"; **IF month is missing, set month to middle of year (July);
    IF SUBSTR(_char, 7, 2) = ".." THEN SUBSTR(tempchar, 7, 2) = "15"; **IF date is missing, set date to middle of month (15th);
    IF _char = "........" THEN tempchar = " "; **IF entire date is missing set date to missing;
    _numr = INPUT(tempchar, yymmdd8.);
  END; ** End of "DO i = 1 to 5";
	LABEL af_facility_type_newcd = "New Type Code of Facility at AIDS Diagnosis "
          aids_age_yrs_num = "Age at AIDS Diagnosis (Yrs.) "
          aids_dx_dtdt = "Date of Earliest Condition Classifying Stage 3 Infection"
          aids_insurance = "Primary Reimbursement for AIDS Medical Treatment "
          aids_rep_dtdt = "Date reported as AIDS "
          AIDSdxYear = "AIDS Dx Year"
          AIDStoDth = "Time from AIDS Dx to Death (Days)"
          bacterial_infection_dtdt = "Date of bacterial infections, multiple or recurrent (including Salmonella septicemia) "
          birth_sex = "Sex at Birth "
          cand_esoph_dtdt = "Date of Candidiasis, bronchi, trachea, or lungs "
          cand_lung_dtdt = "Date of candidiasis, bronchi, trachea, or lungs"
          cd4_first_hiv_valueN = "First CD4 test result value after HIV diagnosis "
          cd4_low_cnt_dtdt = "Lowest CD4 count test result value date "
          cd4_low_cnt_valueN = "Lowest CD4 count test result value "
          cd4_low_pct_dtdt = "Lowest CD4 percent test result value date "
          cd4_low_pct_valueN = "Lowest CD4 percent test result value "
          cd4_recent_cnt_dtdt = "Most recent CD4 count test result date "
          cd4_recent_cnt_pct_dtdt = "Most recent CD4 test result (count or percent) date "
          cd4_vl_first_hiv_dtdt = "First CD4 or viral load test result date "
          cervical_cancer_dtdt = "Date of carcinoma, invasive cervical "
          cmv_dtdt = "Date of cytomegalovirus disease (other than in liver, spleen, or nodes) onset at >1 mo. of age "
          cmv_retinitis_dtdt = "Date of Cytomegalovirus retinitis (with loss of vision) "
          cocci_dtdt = "Date of coccidioidomycosis, disseminated or extrapulmonary "
          cryptoco_dtdt = "Date of cryptococcosis, extrapulmonary "
          cryptosp_dtdt = "Date of cryptosporidiosis, chronic intestinal (>1 mo. duration) "
          cur_age_num = "Current Age (Yrs.) "
          cur_county_name2 = "Current County of Residence"
          DaysRptToEnter_HIV = "Days from Report to Entry of HIV "
          death_age = "Age at death (Yrs.)"
          death_age_yrs_num = "Age at Death (Yrs.)"
          dobdt = "Date of Birth "
          doc = "Department of Corrections"
          doddt = "Date of Death "
          dthYear = "Year of Death"
          ethnicity1 = "Ethnicity (Hispanic or Non-Hispanic) "
          herpes_simplex_dtdt = "Date of Herpes simplex: chronic ulcer(s) (>1 mo. duration) or bronchitis, pneumonitis, or esophagitis "
          hf_facility_type_newcd = "New Type code of facility at HIV diagnosis"
          histo_dtdt = "Date of histoplasmosis, disseminated or extrapulmonary "
          hiv_age_yrs_num = "Age at HIV Diagnosis (Yrs.)"
          hiv_aids_age_yrs_num = "Age at HIV Disease Diagnosis (Yrs.)"
          hiv_aids_dx_dtdt = "HIV disease diagnosis date "
          hiv_aids_rep_dtdt = "Date reported as HIV disease "
          hiv_enceph_dtdt = "Date of HIV encephalopathy "
          hiv_insurance = "Primary Reimbursement for HIV (Non-AIDS) Medical Treatment "
          hiv_rep_dtdt = "Date reported as HIV positive "
          HIVdthYear = "Year of HIV Death"
          HIVdxYear = "Year of HIV Diagnosis"
          iso_dtdt = "Date of Isosporiasis, chronic intestinal (>1 mo. duration) "
          ks_dtdt = "Date of Kaposi's sarcoma "
          linktocare = "Time from HIV Dx to First CD4 or Viral Load Test (Categorical)"
          lip_dtdt = "Date of lymphoid interstitial pneumonia and/or pulmonary lymphoid hyperplasia "
          lymph_brain_dtdt = "Date of lymphoma, primary in brain "
          lymph_burkitts_dtdt = "Date of lymphoma, Burkitts "
          lymph_immuno_dtdt = "Date of lymphoma, immunoblastic "
          mthtoAIDS = "Months from HIV to AIDS Dx"
          myco_avium_dtdt = "Date of Mycobacterium avium complex or M.kansaslii, disseminated or extrapulmonary "
          myco_other_dtdt = "Date of Mycobacterium, of other species or unidentified species, disseminated or extrapulmonary "
          myco_tb_dissem_dtdt = "Date of M. tuberculosis, disseminated or extrapulmonary "
          myco_tb_pulm_dtdt = "Date of M. tuberculosis, pulmonary "
          pcp = "Pneumocystis Carinii Pneumonia"
		  pcp_dtdt = "Date of Pneumocystis Carinii Pneumonia"
          pct_poverty = "Residents Living at <FPL in Current Zip Code (%)"
          pml_dtdt = "Date of progressive multifocal leukoencephalopathy "
          pneumonia_recurrent_dtdt = "Date of pneumonia, recurrent, in 12 mo. period "
          quickconvert = "Time from HIV to AIDS Dx"
          race = "Race/Ethnicity"
          race_rec = "Race/Ethnicity (W/B/H/A/Other)"
          raceeth = "Race/Ethnicity (W/B/H/Other)"
          salmonella_dtdt = "Date of Salmonella septicemia, recurrent "
          screen_first_pos_dtdt = "Date of First Positive HIV Screening Test"
          ssn_rev = "Social security number (SSN) "
          test_recent_dtdt = "Most Recent CD4 or Viral Load Test Date "
          timetocare = "Time from HIV Dx to First CD4 or Viral Load Test (Days)"
          toxo_brain_dtdt = "Date of toxoplasmosis of brain, onset at >1 mo. of age "
          vl_recent_dtdt = "Most recent viral load test result date "
          vl_recent_valueN = "Most recent viral load test result value "
          wasting_dtdt = "Date of wasting syndrome due to HIV "
		  AIDSdthYear = "Year of AIDS-Related Death"
          aids_categ_7_rep_dtdt = "Date reported as AIDS (immunologic) "
          aids_categ_a_rep_dtdt = "Date reported as AIDS (OI) "
          antigen_first_pos_dtdt = "First positive HIV antigen test result d "
          asymp_dtdt = "Date patient diagnosed as asymptomatic "
          cd4_first_hiv_dtdt = "First CD4 test result date after HIV dia "
          cd4_stage3_dtdt = "The earliest date on which the immunolog "
          cur_address_dtdt = "Date address of current residence "
          last_med_eval_dtdt = "Date of last medical evaluation "
          modify_dtdt = "Date person view was last modified "
          oi_stage3_dtdt = "Date of the eariliest stage 3 defining o "
          phys_hiv_dtdt = "Date confirmed by physician as HIV infec "
          screen_last_neg_dtdt = "Most recent negative HIV screening test "
          vl_first_det_dtdt = "First detectable viral load test result "
          vl_last_non_det_dtdt = "Last non-detectable viral load test resu "
          cur_phone = "Phone number of current residence"
          death_place = "Place of death"
          sex_bisexual_male  = "Adult heterosexual contact with bisexual male"
          sex_female	     = "Adult sex with female"
          sex_hemo	         = "Adult heterosexual contact with person with hemophilia"
          sex_hiv	         = "Adult heterosexual contact with person with HIV infection"
          sex_idu	         = "Adult heterosexual contact with IDU"
          sex_male	         = "Adult sex with male"
          sex_transfusion	 = "Adult heterosexual contact with transfusion recipient with HIV infection"
          sex_transplant	 = "Adult heterosexual contact with transplant recipient with HIV infection"
          dobdt     = "Date of Birth"
		  ssn	    = "Social Security Number"
		  ssn_rev   = "Social Security Number (No hyphens)"
		  ssn_alias = "Social Security Number (Alias)"
	;
	FORMAT aids_categ_7_rep_dtdt aids_categ_a_rep_dtdt antigen_first_pos_dtdt
		   asymp_dtdt cd4_first_hiv_dtdt cd4_stage3_dtdt cur_address_dtdt
           last_med_eval_dtdt modify_dtdt oi_stage3_dtdt phys_hiv_dtdt
           screen_last_neg_dtdt vl_first_det_dtdt vl_last_non_det_dtdt DATE9.
	;
RUN ;

** Create HIV continuum of care variables and process prior to merge with RISE ;

DATA &RequestID.HIVdatAll (DROP=prisno doc last_name name_suffix) ;
	SET &RequestID.MergeDat ;
  ** Combine last name and suffix prior merge with RISE data ;
	lastname = ""||TRIM(last_name)||" "||TRIM(name_suffix)||"" ;
  ** Rename date of birth variable name to match other data sets pre-merge ;
	RENAME DOBDT = DOB ;
  ** Create a variable indicating current or past Hx of incarceration;
	IF prisno NOT IN (" ", ".") OR (0 <= doc <= 1) OR doc = 3 THEN DOChx = 1 ;
                                                              ELSE DOChx = 0 ; 
  ** Create a variable, SrvInYr, that designates whether or not at least one CD4/VL was performed during the period of interest;
	IF &prdbegdt. <= MinTestDt <= &prdenddt. THEN SrvInYr = 1; ** Had at least one CD4/VL during the one-year period of interest ;
		                                     ELSE SrvInYr = 0;
  ** Create a variable indicating if first lab was during the first half of the period of interest;
	IF SrvInYr = 1 AND &prdbegdt. <= MinTestDt <= "30JUN&Year."d
	THEN SrvInFrstHlf = 1 ; ** Had at least one CD4/VL during the first half of the one-year period of interest ;
	IF SrvInYr = 1 AND "01JUL&Year."d  <= MinTestDt <= &prdenddt.
	THEN SrvInFrstHlf = 0 ; ** No CD4/VL during the first half of the one-year period of interest ;
  ** Create a variable indicating if there was a gap in HIV medical visits;
	IF SrvInFrstHlf = 1 AND "01JUL&Year."d <= MaxTestDt <= &prdenddt.
	THEN MedVstGap = 0 ; ** Did not experience a gap in medical visits ;
	IF SrvInFrstHlf = 1 AND &prdbegdt.   <= MaxTestDt <= "30JUN&Year."d
	THEN MedVstGap = 1 ; ** Experienced a gap in medical visits ;
  ** Select PLWH/A who had at least two CD4/VL tests performed during the measurement period, with at least 90 days between.
     This is the new CDC definition of retained in care (see RWG Part A/MAI grant application for FY 2016);
	IF (MaxTestDt - MinTestDt => 90) THEN Retained = 1; ** Retained in care;
	IF (MaxTestDt - MinTestDt  < 90) THEN Retained = 0; ** Not retained in care;
  ** Apply formats and labels ;
	FORMAT eHARS_calc_VLstart eHARS_calc_VLend vl. DOChx SrvInYr MedVstGap Retained yn. ;
	LABEL SrvInYr       = "Received at Least One CD4/VL during Period of Interest (Y/N)"
		  SrvInFrstHlf  = "First CD4/VL Rec'd. during First Half of Period of Interest (Y/N)"
          Retained      = "Retained in Care"
		  MedVstGap     = "Gap in HIV Medical Visits (Y/N)"
		  DOChx         = "Past/Current Incarceration (Y/N)"
		  ehars_uid     = "eHARS Unique Identifier"
		  ehars_uid_rwg = "eHARS UID w/RWG Format (no *)" ;
RUN ;

/*  Test Code  */
/*
PROC FREQ DATA=&RequestID.HIVdatAll ;
	TABLES SrvInYr&Year. SrvIn&Year.FrstHlf Retained&Year. MedVstGap&Year. DOChx / LIST ;
RUN ;
*/

** Rename last name var to original name pre-RISE merge, create indicator
   variables for year of interest, HIV/AIDS prevalence, and deaths during
   &Year., and narrow data set down to those living in the TGA with HIV
   on 01JAN&Year. ;

DATA &RequestID.Prev&Year. ;
	SET &RequestID.HIVdatAll ;
	WHERE (cur_TGA = 1 OR rad_TGA = 1)
      AND hiv_aids_dx_dtdt <= &prdenddt.
      AND (doddt > &prdbegdt. OR doddt = .); ** Keep only those alive in the TGA as of &prdenddt., and those who died there during the period of interest;
  ** Create indicator variable for AIDS deaths from &prdbegdt. to &prdenddt.;
	IF &prdbegdt. <= doddt <= &prdenddt. THEN prevDth = 1 ;
	                                     ELSE prevDth = . ;
  ** Create indicator vars for status since dx_status is as of current date and we want status on &prevdtdt.;
	IF prevdth = 1 THEN prevdx_status = 0 ; ** Not a prevalent case, dead as of &prdenddt. ;
	ELSE IF hiv_dx_dtdt <= &prdenddt. AND (aids_dx_dtdt >= &prdenddt. OR aids_dx_dtdt = .) THEN prevdx_status = 1 ; ** HIV not-AIDS ;
	ELSE IF aids_dx_dtdt <= &prdenddt.                                                     THEN prevdx_status = 2 ; ** AIDS ;
	ELSE PUT "WAR""NING: prevdx_status not assigned when all categories should be accounted for." hiv_dx_dtdt= aids_dx_dtdt= ;
	RENAME lastname = last_name ;
	Year = &Year. ;
	FORMAT prevdx_status prevdx_status. ;
	LABEL prevdx_status = "Prevalent Status as of &EndDt."
          lastname      = "Last Name"
          year          = "Year of Interest" ;
RUN ;

** Calculate estimated number of HIV-positive TGA residents who are undiagnosed and unaware of their serostatus for the continuum of care;

PROC SORT DATA=&RequestID.Prev&Year.; BY year; RUN;
PROC MEANS DATA=&RequestID.Prev&Year. NOPRINT N ;
	WHERE prevdx_status > 0 ;
	BY year ;
	VAR prevdx_status ;
	OUTPUT OUT=&RequestID.Yr&Year.EstUnDx N=NoObs ;
RUN ;

DATA &RequestID.Yr&Year.EstUnDx (KEEP= Year NoObs EstUnDx TotPrev) ;
	SET &RequestID.Yr&Year.EstUnDx ;
	EstUnDx = CEIL(NoObs * (0.15/(1-0.15))) ;
	TotPrev = (NoObs + EstUnDx) ;
	LABEL NoObs   = "PLWH/A"
	      EstUnDx = "Estimated Undiagnosed"
          TotPrev = "Total Estimated PLWH/A" ;
RUN ;

** Create incidence data set for linkage to care variable only ;

DATA &RequestID.Yr&Year.Inc ;
	SET &RequestID.HIVdatAll ;
 ** Create HIV, non-AIDS at Dx incidence indicator variable ;
	IF rsh_state_cd IN ("IN") AND rsh_TGA = 1
   AND disease_categ_dx ^= "3"
   AND (&prdbegdt. <= hiv_aids_rep_dtdt <= &prdenddt.) THEN hiv_na_incidence = 1 ;
 ** Create HIV, AIDS at Dx incidence indicator variable ;
	IF rsh_state_cd IN ("IN") AND rsh_TGA = 1
   AND disease_categ_dx = "3"
   AND &prdbegdt. <= hiv_aids_rep_dtdt <= &prdenddt.   THEN latedx_aids_incidence = 1 ;
 ** Create total HIV incidence indicator variable ;
	IF rsh_state_cd IN ("IN") AND rsh_TGA = 1
   AND (&prdbegdt. <= hiv_aids_rep_dtdt <= &prdenddt.) THEN hiv_incidence = 1 ;
 ** Create HIV-to-AIDS conversion indicator variable ;
	IF rsa_state_cd IN ("IN") AND rsa_TGA = 1
   AND &prdbegdt. <= aids_rep_dtdt <= &prdenddt.
   AND latedx_aids_incidence ^= 1                      THEN hiv_aids_conversion = 1 ;
 ** Create total AIDS incidence indicator variable ;
	IF rsa_state_cd IN ("IN") AND rsa_TGA = 1
   AND (&prdbegdt. <= aids_rep_dtdt <= &prdenddt.)     THEN aids_incidence = 1 ;
	Year = &Year. ;
	LABEL aids_incidence        = "Total New AIDS Dx"
	      hiv_incidence         = "Total New HIV Dx"
	      hiv_na_incidence      = "New HIV (non-AIDS) Dx"
	      latedx_aids_incidence = "New HIV Dx at AIDS"
	      hiv_aids_conversion   = "HIV-to-AIDS Conversion"
          year                  = "Year of Interest" ;
RUN ;

** Limit incidence data set to only incident cases ;

DATA &RequestID.Yr&Year.Inc ;
	SET &RequestID.Yr&Year.Inc ;
	WHERE hiv_incidence  = 1
	   OR aids_incidence = 1 ;
RUN ;

**  Close library ;

LIBNAME eHARS clear ; ** The lab library is closed in the lab sub-program ;

QUIT ;
