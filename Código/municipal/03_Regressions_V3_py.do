*****************************
* 							*
*		Regressions			*
*							*
*****************************
/*
ssc install csdid
ssc install drdid
*/

clear all
set more off

local usuario=c(username)

if "`usuario'"=="sebas"{
	global mainpath= "C:\Users\sebas\Dropbox\Documents\Proyecto_UsosDeTierra"
}
*else if{
*	global mainpath= ""
*}

cd "${mainpath}"

use "CreatedData/dataset_landuse_v_py.dta",clear

order codmpio year, first
sort codmpio year

label var codmpio "Municipality ID"
label var total_analyzed "Area analyzed by DW ($km^2$)"
label var total_area_km2 "Municipality area($km^2$)"
* Precipitation measures
label var avg_ppm_muni "Average precipitation(mm)" 
label var avg_weighted_ppm_muni "Average precipitation weightened by subhydrographic zone(mm)"
label var avg_ppm_muni_hist "Historic average precipitation(mm)"
label var sd_ppm_muni_hist "Historic std. deviation precipitation(mm)"
label var deviation "Deviation of the historic mean"
label var deviation_weighted "Deviation weighted of the historic mean"
*Treatment dummies
label var treat_2SD "Atypic rain 2SD"
label var treat_group_2SD "First extreme weather 2SD"
label var treat_dummy_2SD "since atypic rain 2SD"
* Area variables in percentages
label var p_total_analyzed "Total area analyzed(\%)"
label var p_trees "Trees area(\%)"
label var p_crops "Crops area(\%)"
label var p_grass "Grass area(\%)"
label var p_water "Water area(\%)"
label var p_flooded_vegetation "Flooded vegetation area(\%)"
label var p_built "Built area(\%)"
label var p_bare "Bare area(\%)" 
label var p_scrub "Shrub and scrub area(\%)"
* Production covariates
label var produccion_ton_anual "Annual crops yield(tn)"
label var produccion_ton_permanente "Permanent crops yield(tn)"
label var produccion_ton_transitorio "No permanent crops yield(tn)"
* Gathered area
label var p_area_cosechada_km2_anual "Annual crops gathered(\%)"
label var p_area_cosechada_km2_permanente "Permanent crops gathered(\%)"
label var p_area_cosechada_km2_transitorio "No permanent crops gathered(\%)"
label var p_muni_area_cosechada_km2 "Gathered area (\%)"
* Planted area
label var p_area_sembrada_km2_anual "Annual crops planted(\%)"
label var p_area_sembrada_km2_permanente "Permanent crops planted(\%)"
label var p_area_sembrada_km2_transitorio "No permanent crops planted(\%)"
label var p_muni_area_sembrada_km2 "Planted area (\%)"
* Geographic covariates 
label var area_new "Municipal area CRS 9377"
label var p_area_within_pnn "PNN area(\%) "
label var p_area_within_resguardos "Resguardos area(\% )"
label var length_within_main_roads "Length main roads ($km$)"
label var length_within_other_roads "Length terciary roads ($km$)"
** Census data
* Method for land improvements
label var fer_org "Organic fertilizer use(\%)"
label var fer_quim "Chemic fertilizer use(\%)"
label var corr_acid "Acid correction(\%)"
label var quemas "Land burn(\%)"
label var no_mejora "No use of any land improvement (\%)"
label var rezos "Prayers(\%)"
label var ritos "Rituals(\%)"
label var pagamentos "Payment rituals(\%)"
* Problems with use of water
label var disaster "Water use dificulties because of a natural disaster(\%)"
* UPAs percentage by municipality
label var less5 "UPA $<$ 5 ha(\% total UPAs)"
label var bt5_10 "UPA 5ha$-$10ha(\% total UPAs)"
label var bt10_50 "UPA 10ha$-$50ha(\% total UPAs)"
label var bt50_100 "UPA 50ha$-$100ha(\% total UPAs)"
label var bt100_500 "UPA 100ha$-$500ha(\% total UPAs)"
label var bt500_1000 "UPA 500ha$-$1000ha(\% total UPAs)"
label var more1000 "UPA $+$1000(\%)"
* Land concetration
label var p_less5_ha "\% mun. area owned by UPA less than 5ha"
label var p_5_10ha "\% mun. area owned by UPA 5-10ha"
label var p_10_50ha "\% mun. area owned by UPA 10-50ha"
label var p_50_100ha "\% mun. area owned by UPA 50-100ha" 
label var p_100_500_ha "\% mun. area owned by UPA 100-500ha"
label var p_500_1000ha "\% mun. area owned by UPA 500-1000ha"
label var p_more1000ha "\% mun. area owned by UPA more than 1000ha"
label var p_more500 "Large land ownership (\%)"
* Treatment
label var ever_treat "Mun. treated"
label var ever_treat_no2015 "Mun. treated exluding always treated"

// ---------------------------------------------------- //
* Final edits
rename  p_flooded_vegetation p_f_veg
gen coddepto = (codmpio-mod(codmpio,1000))/1000

*gen log_p_max_trees = log(p_max_trees)
*tw scatter p_max_trees sd_before2000
*tw scatter log_p_max_trees sd_before2000
*tw scatter log_max_trees sd_before2000

/// ------------------------------------------------------------------------------------------------------- ///
///									Table 1: Descriptive Statistics											///
/// -------------------------------------------------------------------------------------------------------	///
/// 									Municipal characteristics											///
/// -------------------------------------------------------------------------------------------------------	///
/// Area analyzed: 						total_area_km2 														///
/// Indigena or PNN land (%):			p_area_within_pnn p_area_within_resguardos							///
/// Roads length:						length_within_main_roads length_within_other_roads					///
/// Soil improvement:					fer_org fer_quim corr_acid quemas rezos ritos pagamentos no_mejora	///
/// Water use prolem:					disaster															///
/// % # UPA within municipality 		less5 bt5_10 bt10_50 bt50_100 bt100_500 bt500_1000 more1000			///
/// area_cna_muni:						Censed municipality's area by DANE									///
/// weight UPA length on municipality	p_5_10ha p_10_50ha p_50_100ha p_100_500_ha p_500_1000ha p_more1000ha///
/// Large land ownership				p_more500															///
///	Historic precipitation 				avg_ppm_muni_hist sd_ppm_muni_hist									///
/// 																										///
/// ------------------------------------------------------------------------------------------------------- ///
/// Area analyzed by DW:				total_analyzed 											 			///
/// Percentage area analyzed: 			p_trees p_crops p_grass p_water p_f_veg p_built p_bare				///
/// Average precipitation:				avg_ppm_muni avg_weighted_ppm_mun									///
/// Deviation: 							deviation deviation_weighted										///
/// 																										///
/// ------------------------------------------------------------------------------------------------------- ///

// Municipal data
preserve
	duplicates drop codmpio, force
	sum total_area_km2 p_area_within_pnn p_area_within_resguardos length_within_main_roads length_within_other_roads fer_org fer_quim corr_acid quemas rezos ritos pagamentos no_mejora disaster less5 bt5_10 bt10_50 bt50_100 bt100_500 bt500_1000 more1000 area_cna_muni p_more500 avg_ppm_muni_hist sd_ppm_muni_hist
restore

// Panel data
sum total_analyzed p_total_analyzed p_trees p_crops p_grass p_water p_f_veg p_built p_bare avg_ppm_muni avg_weighted_ppm_muni treat_2SD treat_group_2SD treat_dummy_2SD /*p_muni_area_sembrada_km2 p_muni_area_cosechada_km2 p_area_cosechada_km2_anual p_area_cosechada_km2_permanente p_area_cosechada_km2_transitorio p_area_sembrada_km2_anual p_area_sembrada_km2_permanente p_area_sembrada_km2_transitorio*/

*** ---- Slides ---- ****

*** ---- Paper ---- ****

// Municipal information descriptive statistics
preserve
	duplicates drop codmpio, force
	eststo DS_1: estpost summarize total_area_km2 p_area_within_pnn p_area_within_resguardos /*fer_org fer_quim corr_acid quemas rezos ritos pagamentos*/ no_mejora disaster /*less5 bt5_10 bt10_50 bt50_100 bt100_500 bt500_1000 more1000 area_cna_muni*/ p_more500 length_within_main_roads length_within_other_roads avg_ppm_muni_hist sd_ppm_muni_hist // Municipal characteristics
esttab . using "Paper/Tables/desstats_1.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace
restore

// Panel information descriptive statistics
eststo DS_2: estpost summarize total_analyzed p_total_analyzed p_trees p_crops p_grass p_water p_f_veg p_built p_bare avg_ppm_muni avg_weighted_ppm_muni  
esttab . using "Paper/Tables/desstats_2.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace

eststo DS_3: estpost summarize treat_2SD treat_group_2SD treat_dummy_2SD
esttab . using "Paper/Tables/desstats_3.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace

eststo DS_4: estpost summarize p_area_cosechada_km2_anual p_area_cosechada_km2_permanente p_area_cosechada_km2_transitorio p_area_sembrada_km2_anual p_area_sembrada_km2_permanente p_area_sembrada_km2_transitorio
esttab . using "Paper/Tables/desstats_4.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace

// --------------------------------------------- //
//				Regressions - TWFE				 //
// --------------------------------------------- // 
//						TWFE					 //
// --------------------------------------------- //
//												 //
// 		Portion of trees in municipal area		 //
//												 //
// --------------------------------------------- //
preserve

local SDs "1SD 1_5SD 2SD 2_5SD" // robust errors
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	foreach SD of local SDs{
		// Regression
		qui eststo `var_reg'_`SD': reghdfe `var_reg' treat_dummy_`SD' , abs(codmpio year) cluster(codmpio)
		qui estadd scalar m_c= _b[_cons]
		qui unique codmpio if treat_dummy_`SD'==1
		qui estadd local munitreated = r(unique)
		qui unique codmpio
		qui estadd local totmunis = r(unique)
	}
	
	* ---------------- Exporting all regressions ---------------- *
	* -- Slides -- *
	estout `var_reg'_1SD `var_reg'_1_5SD `var_reg'_2SD `var_reg'_2_5SD ///
	using "Slides/Tables/TWFE/TWFE_`var_reg'_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%6.3f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "N. Obs" "\$R^2\$" )) replace
	* -- Paper --*
	estout `var_reg'_1SD `var_reg'_1_5SD `var_reg'_2SD `var_reg'_2_5SD ///
	using "Paper/Tables/TWFE/TWFE_`var_reg'_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%6.3f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "Municipalities treated" "N. Obs" "\$R^2\$" )) replace
}
* ---------------- Exporting final tables ---------------- *
* -- Slides -- *
estout p_trees_2SD  p_grass_2SD  p_crops_2SD p_water_2SD p_f_veg_2SD p_built_2SD p_bare_2SD p_scrub_2SD ///
using "Slides/Tables/TWFE_landuse_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace
* -- Paper --*
estout p_trees_2SD  p_grass_2SD  p_crops_2SD p_water_2SD p_f_veg_2SD p_built_2SD p_bare_2SD p_scrub_2SD ///
using "Paper/Tables/TWFE/TWFE_landuse_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace

restore

/// --------------------------------------------------- ///
///			Regressions - Callaway Sant' Anna			///
/// ---------------------------------------------------	///
set scheme s1mono

local SDs "1SD 1_5SD 2SD 2_5SD" // robust errors
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	foreach SD of local SDs{
		csdid `var_reg', ivar(codmpio) time(year) gvar(treat_group_`SD') agg(group) notyet clu(codmpio)
		estat event, estore(cs_`var_reg'_`SD')
		estat simple, estore(cs_sim_`var_reg'_`SD')
		estat group, estore(cs_gp_`var_reg'_`SD')
		estat calendar, estore(cs_cal_`var_reg'_`SD')
		estat event, window(-4 6)
		csdid_plot
		/*
		event_plot cs_`var_reg'_p00_`SD', default_look  ///
		graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
		xlabel(-6(1)6)) stub_lag(Tm4#) stub_lead(Tm#) ciplottype(rcap) together
		*/
		graph export "Slides/Figures/CS/ES_`var_reg'_`SD'.pdf", replace
		graph export "Paper/Figures/CS/ES_`var_reg'_`SD'.pdf", replace
	}	
}

// ------------------------------------------------------- //
// 				Export table: main results				   //
// ------------------------------------------------------- //
// only ATT
local SDs "1SD 1_5SD 2SD 2_5SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_sim_p_trees_`SD' cs_sim_p_grass_`SD' cs_sim_p_crops_`SD' cs_sim_p_water_`SD' cs_sim_p_f_veg_`SD' cs_sim_p_built_`SD' cs_sim_p_bare_`SD' /// 
	using "Slides/Tables/CS/CS_estimation_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_sim_p_trees_`SD' cs_sim_p_grass_`SD' cs_sim_p_crops_`SD' cs_sim_p_water_`SD' cs_sim_p_f_veg_`SD' cs_sim_p_built_`SD' cs_sim_p_bare_`SD' cs_sim_p_scrub_`SD' /// 
	using "Paper/Tables/CS/CS_estimation_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// With lags
local SDs "1SD 1_5SD 2SD 2_5SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_p_trees_`SD' cs_p_grass_`SD' cs_p_crops_`SD' cs_p_water_`SD' cs_p_f_veg_`SD' cs_p_built_`SD' cs_p_bare_`SD' /// 
	using "Slides/Tables/CS/CS_estimationES_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout  cs_p_trees_`SD' cs_p_grass_`SD' cs_p_crops_`SD' cs_p_water_`SD' cs_p_f_veg_`SD' cs_p_built_`SD' cs_p_bare_`SD' ///
	using "Paper/Tables/CS/CS_estimationES_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Group analysis
local SDs "1SD 1_5SD 2SD 2_5SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_gp_p_trees_`SD' cs_gp_p_grass_`SD' cs_gp_p_crops_`SD' cs_gp_p_water_`SD' cs_gp_p_f_veg_`SD' cs_gp_p_built_`SD' cs_gp_p_bare_`SD' cs_gp_p_scrub_`SD' /// 
	using "Slides/Tables/CS/CS_estimation_gp_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_gp_p_trees_`SD' cs_gp_p_grass_`SD' cs_gp_p_crops_`SD' cs_gp_p_water_`SD' cs_gp_p_f_veg_`SD' cs_gp_p_built_`SD' cs_gp_p_bare_`SD' cs_gp_p_scrub_`SD' ///
	using "Paper/Tables/CS/CS_estimation_gp_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Calendar analysis
local SDs "1SD 1_5SD 2SD 2_5SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_cal_p_trees_`SD' cs_cal_p_grass_`SD' cs_cal_p_crops_`SD' cs_cal_p_water_`SD' cs_cal_p_f_veg_`SD' cs_cal_p_built_`SD' cs_cal_p_bare_`SD' cs_cal_p_scrub_`SD' ///
	using "Slides/Tables/CS/CS_estimation_cal_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_cal_p_trees_`SD' cs_cal_p_grass_`SD' cs_cal_p_crops_`SD' cs_cal_p_water_`SD' cs_cal_p_f_veg_`SD' cs_cal_p_built_`SD' cs_cal_p_bare_`SD' cs_cal_p_scrub_`SD' ///
	using "Paper/Tables/CS/CS_estimation_cal_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// ---------------------------------- //
//				Graphics			  //
// ---------------------------------- //
local SDs "1SD 1_5SD 2SD 2_5SD"
foreach SD of local SDs{
	preserve
		set scheme plotplain
		duplicates drop codmpio,force
		keep if treat_group_`SD'>0
		graph bar (count), over(treat_group_`SD') blabel(bar) ytitle("# municipalities treated")
		graph export "Slides/Figures/treatment_count`SD'.pdf", replace
		graph export "Paper/Figures/treatment_count`SD'.pdf", replace
	restore
}

foreach var_reg in p_trees p_crops p_grass p_water p_f_veg  p_built p_bare p_scrub{
	coefplot ///
	(cs_`var_reg'_1SD, label(1SD) mcolor(dkgreen) ciopts(color(dkgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) /// 
	(cs_`var_reg'_1_5SD, label(1_5SD) mcolor(blue) ciopts(color(blue%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
	(cs_`var_reg'_2SD, label(2SD) mcolor(red) ciopts(color(red%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
	(cs_`var_reg'_2_5SD, label(2_5SD) mcolor(purple) ciopts(color(violet%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")), ///
	vertical yline(0) keep(Pre_avg Post_avg)
graph export "Slides/Figures/pre_pos_`var_reg'.pdf", replace
graph export "Paper/Figures/pre_pos_`var_reg'.pdf", replace
}

eststo clear

* ============================================= *
*												*
*				Group regressions				*
*												*
* ============================================= *

* ============================================================================================================ *
* Land concentration *
* ================== *
* ----   TWFE	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- There is municipalities with lower land concentration than the mean (< 40.61) -1.4
* p_grass <- Nothing
* p_water <- There is municipalities with high land concentration than the mean (> 40.61) 1.2
* p_f_veg <- Nothing
* p_built <- Nothing
* p_bare  <- There is municipalities with high land concentration than the mean (> 40.61) 0.4
* p_scrub <- Nothing
* ================== *
* reghdfe p_scrub treat_dummy_2SD, abs(codmpio year) cluster(codmpio) 
* reghdfe p_scrub treat_dummy_2SD if p_more500 > 40.61, abs(codmpio year) cluster(codmpio)
* reghdfe p_scrub treat_dummy_2SD if p_more500 < 40.61, abs(codmpio year) cluster(codmpio)
* ================== *
* ----    CS 	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- There is municipalities with lower land concentration than the mean (< 40.61) -1.46
* p_grass <- Nothing
* p_water <- Nothing
* p_f_veg <- Nothing
* p_built <- Nothing
* p_bare  <- There is municipalities with high land concentration than the mean (> 40.61) 0.36
* p_scrub <- Nothing
* ================== *
* csdid p_scrub, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
* csdid p_scrub if p_more500 > 40.61, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
* csdid p_scrub if p_more500 < 40.61, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
* ============================================================================================================ *
preserve
	duplicates drop codmpio, force
	tab ever_treat_no2015 if   p_more500 > 40.61
	tab ever_treat if   p_more500 > 40.61
restore

* ==========
* Above mean Land Concentration
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_more500 > 40.61, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_high_LC)
	estat simple, estore(cs_sim_`var_reg'_high_LC)
	estat group, estore(cs_gp_`var_reg'_high_LC)
	estat calendar, estore(cs_cal_`var_reg'_high_LC)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_high_LC.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_high_LC.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_high_LC cs_gp_p_grass_high_LC cs_gp_p_crops_high_LC cs_gp_p_water_high_LC cs_gp_p_f_veg_high_LC cs_gp_p_built_high_LC cs_gp_p_bare_high_LC cs_gp_p_scrub_high_LC /// 
using "Slides/Tables/CS/CS_estimation_gp_high_LC.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_high_LC cs_gp_p_grass_high_LC cs_gp_p_crops_high_LC cs_gp_p_water_high_LC cs_gp_p_f_veg_high_LC cs_gp_p_built_high_LC cs_gp_p_bare_high_LC cs_gp_p_scrub_high_LC /// 
using "Paper/Tables/CS/CS_estimation_gp_high_LC.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ==========
* Below mean Land Concentration
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_more500 < 40.61, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_low_LC)
	estat simple, estore(cs_sim_`var_reg'_low_LC)
	estat group, estore(cs_gp_`var_reg'_low_LC)
	estat calendar, estore(cs_cal_`var_reg'_low_LC)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_low_LC.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_low_LC.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_low_LC cs_gp_p_grass_low_LC cs_gp_p_crops_low_LC cs_gp_p_water_low_LC cs_gp_p_f_veg_low_LC cs_gp_p_built_low_LC cs_gp_p_bare_low_LC cs_gp_p_scrub_low_LC /// 
using "Slides/Tables/CS/CS_estimation_gp_low_LC.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_low_LC cs_gp_p_grass_low_LC cs_gp_p_crops_low_LC cs_gp_p_water_low_LC cs_gp_p_f_veg_low_LC cs_gp_p_built_low_LC cs_gp_p_bare_low_LC cs_gp_p_scrub_low_LC ///
using "Paper/Tables/CS/CS_estimation_gp_low_LC.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ============================================================================================================ *
* 		 PNN 		 *
* ================== *
* ----   TWFE	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- Nothing
* p_grass <- There is reduction on %PNN > 12 (-2.31) and increasing in %PNN < 12 (1.0)
* p_water <- There is increasing in both groups
* p_f_veg <- Nothing
* p_built <- There is increasing on %PNN < 12 (0.28)
* p_bare  <- There is increasing on %PNN < 12 (0.24) 
* p_scrub <- Nothing
* ================== *
/*
reghdfe p_crops treat_dummy_2SD if p_area_within_pnn > 12, abs(codmpio year) cluster(codmpio)
reghdfe p_crops treat_dummy_2SD if p_area_within_pnn < 12, abs(codmpio year) cluster(codmpio)
*/
* ================== *
* ----    CS 	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- There is reduction on %PNN < 12 (-0.98)
* p_grass <- Nothing
* p_water <- Nothing
* p_f_veg <- Nothing
* p_built <- There is increasing on %PNN < 12 (0.33)
* p_bare  <- There is increasing on %PNN < 12 (0.30) 
* p_scrub <- Nothing
* ================== *
/*
csdid p_scrub if p_area_within_pnn > 12, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
csdid p_scrub if p_area_within_pnn < 12, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
*/
* ============================================================================================================ *

* ==========
* Above mean PNN
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_area_within_pnn > 12, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_high_PNN)
	estat simple, estore(cs_sim_`var_reg'_high_PNN)
	estat group, estore(cs_gp_`var_reg'_high_PNN)
	estat calendar, estore(cs_cal_`var_reg'_high_PNN)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_high_PNN.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_high_PNN.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_high_PNN cs_gp_p_grass_high_PNN cs_gp_p_crops_high_PNN cs_gp_p_water_high_PNN cs_gp_p_f_veg_high_PNN cs_gp_p_built_high_PNN cs_gp_p_bare_high_PNN cs_gp_p_scrub_high_PNN /// 
using "Slides/Tables/CS/CS_estimation_gp_high_PNN.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_high_PNN cs_gp_p_grass_high_PNN cs_gp_p_crops_high_PNN cs_gp_p_water_high_PNN cs_gp_p_f_veg_high_PNN cs_gp_p_built_high_PNN cs_gp_p_bare_high_PNN cs_gp_p_scrub_high_PNN /// 
using "Paper/Tables/CS/CS_estimation_gp_high_PNN.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ==========
* Below mean PNN
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_area_within_pnn < 12, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_low_PNN)
	estat simple, estore(cs_sim_`var_reg'_low_PNN)
	estat group, estore(cs_gp_`var_reg'_low_PNN)
	estat calendar, estore(cs_cal_`var_reg'_low_PNN)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_low_PNN.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_low_PNN.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_low_PNN cs_gp_p_grass_low_PNN cs_gp_p_crops_low_PNN cs_gp_p_water_low_PNN cs_gp_p_f_veg_low_PNN cs_gp_p_built_low_PNN cs_gp_p_bare_low_PNN cs_gp_p_scrub_low_PNN /// 
using "Slides/Tables/CS/CS_estimation_gp_low_PNN.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_low_PNN cs_gp_p_grass_low_PNN cs_gp_p_crops_low_PNN cs_gp_p_water_low_PNN cs_gp_p_f_veg_low_PNN cs_gp_p_built_low_PNN cs_gp_p_bare_low_PNN cs_gp_p_scrub_low_PNN ///
using "Paper/Tables/CS/CS_estimation_gp_low_PNN.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ============================================================================================================ *
* 	   Resguardos	 *
* ================== *
* ----   TWFE	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- Nothing
* p_grass <- There is a reduction on %resguardos < 5 (-0.96)
* p_water <- There is an increasing on %resguardos > 5 (2.99)
* p_f_veg <- Nothing
* p_built <- Nothing
* p_bare  <- There is an increasing on %resguardos > 5 (1.11) 
* p_scrub <- Nothing
* ================== *
/*
reghdfe p_scrub treat_dummy_2SD if p_area_within_resguardos > 5, abs(codmpio year) cluster(codmpio)
reghdfe p_scrub treat_dummy_2SD if p_area_within_resguardos < 5, abs(codmpio year) cluster(codmpio)
*/
* ================== *
* ----    CS 	---- *
* ================== *
* p_trees <- Nothing
* p_crops <- Nothing
* p_grass <- Nothing
* p_water <- There is an increasing on %resguardos > 5 (0.87)
* p_f_veg <- Nothing
* p_built <- Nothing
* p_bare  <- There is an increasing on %resguardos < 5 (0.16)
* p_scrub <- Nothing
* ================== *
/*
csdid p_scrub if p_area_within_resguardos > 5, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
csdid p_scrub if p_area_within_resguardos < 5, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
*/
* ============================================================================================================ *

* ==========
* Above mean Resguardos
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_area_within_resguardos > 5, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_high_ind)
	estat simple, estore(cs_sim_`var_reg'_high_ind)
	estat group, estore(cs_gp_`var_reg'_high_ind)
	estat calendar, estore(cs_cal_`var_reg'_high_ind)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_high_ind.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_high_ind.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_high_ind cs_gp_p_grass_high_ind cs_gp_p_crops_high_ind cs_gp_p_water_high_ind cs_gp_p_f_veg_high_ind cs_gp_p_built_high_ind cs_gp_p_bare_high_ind cs_gp_p_scrub_high_ind ///  
using "Slides/Tables/CS/CS_estimation_gp_high_ind.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_high_ind cs_gp_p_grass_high_ind cs_gp_p_crops_high_ind cs_gp_p_water_high_ind cs_gp_p_f_veg_high_ind cs_gp_p_built_high_ind cs_gp_p_bare_high_ind cs_gp_p_scrub_high_ind /// 
using "Paper/Tables/CS/CS_estimation_gp_high_ind.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ==========
* Below mean Resguardos
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if p_area_within_resguardos < 5, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_low_ind)
	estat simple, estore(cs_sim_`var_reg'_low_ind)
	estat group, estore(cs_gp_`var_reg'_low_ind)
	estat calendar, estore(cs_cal_`var_reg'_low_ind)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_low_ind.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_low_ind.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_low_ind cs_gp_p_grass_low_ind cs_gp_p_crops_low_ind cs_gp_p_water_low_ind cs_gp_p_f_veg_low_ind cs_gp_p_built_low_ind cs_gp_p_bare_low_ind cs_gp_p_scrub_low_ind ///
using "Slides/Tables/CS/CS_estimation_gp_low_ind.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_low_ind cs_gp_p_grass_low_ind cs_gp_p_crops_low_ind cs_gp_p_water_low_ind cs_gp_p_f_veg_low_ind cs_gp_p_built_low_ind cs_gp_p_bare_low_ind cs_gp_p_scrub_low_ind ///
using "Paper/Tables/CS/CS_estimation_gp_low_ind.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ============================================================================================================ *
* 	 No mejora suelos	 *
* ====================== *
/*
csdid p_scrub if no_mejora > 42.9, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
csdid p_scrub if no_mejora < 42.9, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
*/
* ============================================================================================================ *

* ==========
* Above mean No mejora suelo
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if no_mejora > 42.9, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_high_no_mej)
	estat simple, estore(cs_sim_`var_reg'_high_no_mej)
	estat group, estore(cs_gp_`var_reg'_high_no_mej)
	estat calendar, estore(cs_cal_`var_reg'_high_no_mej)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_high_no_mejora.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_high_no_mejora.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_high_no_mej cs_gp_p_grass_high_no_mej cs_gp_p_crops_high_no_mej cs_gp_p_water_high_no_mej cs_gp_p_f_veg_high_no_mej cs_gp_p_built_high_no_mej cs_gp_p_bare_high_no_mej cs_gp_p_scrub_high_no_mej ///  
using "Slides/Tables/CS/CS_estimation_gp_high_no_mejora.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_high_no_mej cs_gp_p_grass_high_no_mej cs_gp_p_crops_high_no_mej cs_gp_p_water_high_no_mej cs_gp_p_f_veg_high_no_mej cs_gp_p_built_high_no_mej cs_gp_p_bare_high_no_mej cs_gp_p_scrub_high_no_mej ///
using "Paper/Tables/CS/CS_estimation_gp_high_no_mejora.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* ==========
* Below mean Resguardos
* ==========
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg' if no_mejora < 42.9, ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_low_no_mej)
	estat simple, estore(cs_sim_`var_reg'_low_no_mej)
	estat group, estore(cs_gp_`var_reg'_low_no_mej)
	estat calendar, estore(cs_cal_`var_reg'_low_no_mej)
	estat event, window(-4 6)
	csdid_plot
	* Export graph
	graph export "Slides/Figures/CS/ES_`var_reg'_low_no_mejora.pdf", replace
	graph export "Paper/Figures/CS/ES_`var_reg'_low_no_mejora.pdf", replace	
}

* Export to slides
estout cs_gp_p_trees_low_no_mej cs_gp_p_grass_low_no_mej cs_gp_p_crops_low_no_mej cs_gp_p_water_low_no_mej cs_gp_p_f_veg_low_no_mej cs_gp_p_built_low_no_mej cs_gp_p_bare_low_no_mej cs_gp_p_scrub_low_no_mej ///
using "Slides/Tables/CS/CS_estimation_gp_low_no_mejora.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

* Export to paper
estout cs_gp_p_trees_low_no_mej cs_gp_p_grass_low_no_mej cs_gp_p_crops_low_no_mej cs_gp_p_water_low_no_mej cs_gp_p_f_veg_low_no_mej cs_gp_p_built_low_no_mej cs_gp_p_bare_low_no_mej cs_gp_p_scrub_low_no_mej ///
using "Paper/Tables/CS/CS_estimation_gp_low_no_mejora.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace

// Municipal data
preserve
	duplicates drop codmpio, force
	sum length_within_main_roads length_within_other_roads fer_org fer_quim no_mejora disaster less5 bt5_10 bt10_50 bt50_100 bt100_500 bt500_1000 more1000 p_more500 
restore

/*
keep codmpio year treat_2SD treat_group_2SD treat_dummy_2SD p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub coddepto

preserve
	collapse(mean) p_trees, by(codmpio)
	sum p_trees
restore

bysort codmpio: egen avg_p_trees = mean(p_trees)
keep if avg_p_trees > 0.2
// --------------------------------------------- // 
//						TWFE					 //
// --------------------------------------------- //

preserve

foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	qui eststo `var_reg'_2SD: reghdfe `var_reg' treat_dummy_2SD, abs(codmpio year) cluster(codmpio)
	qui estadd scalar m_c= _b[_cons]
	qui unique codmpio if treat_dummy_2SD==1
	qui estadd local munitreated = r(unique)
	qui unique codmpio
	qui estadd local totmunis = r(unique)
}
	
* ---------------- Exporting final tables ---------------- *
* -- Slides -- *
estout p_trees_2SD  p_grass_2SD  p_crops_2SD p_water_2SD p_f_veg_2SD p_built_2SD p_bare_2SD p_scrub_2SD ///
using "Slides/Tables/group_TWFE_landuse_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace
* -- Paper --*
estout p_trees_2SD  p_grass_2SD  p_crops_2SD p_water_2SD p_f_veg_2SD p_built_2SD p_bare_2SD p_scrub_2SD ///
using "Paper/Tables/TWFE/group_TWFE_landuse_clu.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace

restore

/// --------------------------------------------------- ///
///					Callaway Sant' Anna					///
/// ---------------------------------------------------	///
set scheme s1mono
foreach var_reg in p_trees p_crops p_grass p_water p_f_veg p_built p_bare p_scrub{
	csdid `var_reg', ivar(codmpio) time(year) gvar(treat_group_2SD) agg(group) notyet clu(codmpio)
	estat event, estore(cs_`var_reg'_2SD)
	estat simple, estore(cs_sim_`var_reg'_2SD)
	estat group, estore(cs_gp_`var_reg'_2SD)
	estat calendar, estore(cs_cal_`var_reg'_2SD)
	estat event, window(-4 6)
	csdid_plot
	
	graph export "Slides/Figures/CS/group_ES_`var_reg'_`SD'.pdf", replace
	graph export "Paper/Figures/CS/group_ES_`var_reg'_`SD'.pdf", replace	
}

// ------------------------------------------------------- //
// 				Export table: main results				   //
// ------------------------------------------------------- //
// only ATT
local SDs "2SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_sim_p_trees_`SD' cs_sim_p_grass_`SD' cs_sim_p_crops_`SD' cs_sim_p_water_`SD' cs_sim_p_f_veg_`SD' cs_sim_p_built_`SD' cs_sim_p_bare_`SD' /// 
	using "Slides/Tables/CS/group_CS_estimation_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_sim_p_trees_`SD' cs_sim_p_grass_`SD' cs_sim_p_crops_`SD' cs_sim_p_water_`SD' cs_sim_p_f_veg_`SD' cs_sim_p_built_`SD' cs_sim_p_bare_`SD' cs_sim_p_scrub_`SD' /// 
	using "Paper/Tables/CS/group_CS_estimation_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// With lags
local SDs "2SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_p_trees_`SD' cs_p_grass_`SD' cs_p_crops_`SD' cs_p_water_`SD' cs_p_f_veg_`SD' cs_p_built_`SD' cs_p_bare_`SD' /// 
	using "Slides/Tables/CS/group_CS_estimationES_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout  cs_p_trees_`SD' cs_p_grass_`SD' cs_p_crops_`SD' cs_p_water_`SD' cs_p_f_veg_`SD' cs_p_built_`SD' cs_p_bare_`SD' ///
	using "Paper/Tables/CS/group_CS_estimationES_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Group analysis
local SDs "2SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_gp_p_trees_`SD' cs_gp_p_grass_`SD' cs_gp_p_crops_`SD' cs_gp_p_water_`SD' cs_gp_p_f_veg_`SD' cs_gp_p_built_`SD' cs_gp_p_bare_`SD' cs_gp_p_scrub_`SD' /// 
	using "Slides/Tables/CS/group_CS_estimation_gp_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_gp_p_trees_`SD' cs_gp_p_grass_`SD' cs_gp_p_crops_`SD' cs_gp_p_water_`SD' cs_gp_p_f_veg_`SD' cs_gp_p_built_`SD' cs_gp_p_bare_`SD' cs_gp_p_scrub_`SD' ///
	using "Paper/Tables/CS/group_CS_estimation_gp_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Calendar analysis
local SDs "2SD"
foreach SD of local SDs{
	* Export to slides
	estout cs_cal_p_trees_`SD' cs_cal_p_grass_`SD' cs_cal_p_crops_`SD' cs_cal_p_water_`SD' cs_cal_p_f_veg_`SD' cs_cal_p_built_`SD' cs_cal_p_bare_`SD' cs_cal_p_scrub_`SD' ///
	using "Slides/Tables/CS/group_CS_estimation_cal_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	* Export to paper
	estout cs_cal_p_trees_`SD' cs_cal_p_grass_`SD' cs_cal_p_crops_`SD' cs_cal_p_water_`SD' cs_cal_p_f_veg_`SD' cs_cal_p_built_`SD' cs_cal_p_bare_`SD' cs_cal_p_scrub_`SD' ///
	using "Paper/Tables/CS/group_CS_estimation_cal_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

/*
/// --------------------------------------------------- ///
///				Regressions - Chaisemartin				///
/// ---------------------------------------------------	///

/* Install packages
ssc install gtools, replace
ssc install did_multiplegt_dyn, replace
*/

* did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, effects(7) design(0.5,"console") normalized effects_equal placebo(2)
est clear
foreach var_reg in p_max_trees p_max_grass p_max_crops {
	local SDs "1SD 1_5SD 2SD 2_5SD"
	foreach SD of local SDs{
		* Making regressions
		 /// 
		did_multiplegt_dyn `var_reg' codmpio year t_dummy_pre2000_`SD', effects(7) placebo(2) ///
		effects_equal cluster(coddepto) ci_level(95) graph_off
		estadd scalar p_joint = e(p_equality_effects)
		estadd scalar p_placebo = e(p_jointplacebo)
		eststo model_`var_reg'_`SD'
		* Saving plots 
		event_plot e(estimates)#e(variances), default_look  ///
			graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
			xlabel(-2(1)7)) stub_lag(Effect_#) stub_lead(Placebo_#) together ciplottype(rcap)	
		graph export "Slides/Figures/multiplegt_dyn_`var_reg'_`SD'.png", replace
		graph export "Paper/Figures/multiplegt_dyn_`var_reg'_`SD'.png", replace
		
	}
}
/// ------------------------ Complete Table ------------------------------ ///
* --- Slides --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Slides/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
* --- Paper --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Paper/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace

/// ------------------------ Average Effects ------------------------------ ///
* --- Slides --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Slides/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
keep(Av_tot_eff) prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
* --- Paper --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Paper/Tables/multiplegt_dyn_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
keep(Av_tot_eff) prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
est clear

did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, effects(7) effects_equal placebo(2) cluster(coddepto) ci_level(95) graph_off

event_plot e(estimates)#e(variances), default_look  ///
	graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	xlabel(-2(1)7)) stub_lag(Effect_#) stub_lead(Placebo_#) together ciplottype(rcap)
graph export "Slides/Figures/pre_pos_`var_reg'.png", replace
graph export "Paper/Figures/pre_pos_`var_reg'.png", replace


did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, placebo(1) cluster(coddepto) ci_level(95) graphoptions(yline(0))
did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_2SD, effects(7) cluster(coddepto) ci_level(95) graphoptions(yline(0))


did_multiplegt_dyn p_max_crops codmpio year t_dummy_pre2000_1SD, placebo(1) cluster(coddepto) ci_level(95)
did_multiplegt_dyn p_max_crops codmpio year t_dummy_pre2000_1SD, effects(7) cluster(coddepto) ci_level(95)

did_multiplegt_dyn p_max_grass codmpio year t_dummy_pre2000_1SD, placebo(1) cluster(coddepto) ci_level(95)
did_multiplegt_dyn p_max_grass codmpio year t_dummy_pre2000_1SD, effects(7) cluster(coddepto) ci_level(95)

eststo clear
*/



