*Econometric Analysis - Female DM Project 

*IAS 70 Dataset 
*Importing and inspecting dataset 
clear
import delimited ias_panel_wide_70.csv
summarize 
describe, fullnames 

*Generating difference variables 
generate diff_prop_vill_p_sch = prop_vill_p_sch_11 - prop_vill_p_sch_01 
generate diff_prop_vill_m_sch = prop_vill_m_sch_11 - prop_vill_m_sch_01
generate diff_prop_vill_s_sch = prop_vill_s_sch_11 - prop_vill_s_sch_01 
generate diff_prop_ph_cntr = prop_vill_ph_cntr_11 - prop_vill_ph_cntr_01 
generate diff_prop_all_hosp = prop_vill_all_hosp_11 - prop_vill_all_hosp_01 
generate diff_prop_disp = prop_vill_disp_11 - prop_vill_disp_01 
generate diff_prop_mcw_cntr = prop_vill_mcw_cntr_11 - prop_vill_mcw_cntr_01 
generate prop_rural_area_01_new = area_rural_01/area_total_01

*CLeaning dataset 
drop prop_vill_p_sch_01 prop_vill_m_sch_01 prop_vill_s_sch_01 prop_vill_ph_cntr_01 prop_vill_all_hosp_01 prop_vill_disp_01 prop_vill_mcw_cntr_01 prop_vill_p_sch_11 prop_vill_m_sch_11 prop_vill_s_sch_11 prop_vill_ph_cntr_11 prop_vill_all_hosp_11 prop_vill_disp_11 prop_vill_mcw_cntr_11 prop_rural_area_01 prop_rural_area_11 

*Inspecting cleaned dataset 
summarize 
describe, fullnames 

*Checking distributions of controls 
kdensity avg_vill_t_p_01

generate log_avg_vill_t_p_01 = log(avg_vill_t_p_01)
kdensity log_avg_vill_t_p_01

kdensity no_villages_inhabited_rural_01
generate log_no_villages_inhabited_01 = log(no_villages_inhabited_rural_01)
kdensity log_no_villages_inhabited_01

*Running regressions 

regress diff_prop_vill_p_sch prop_service_female 
estat hettest
regress diff_prop_vill_m_sch prop_service_female
regress diff_prop_vill_s_sch prop_service_female

regress diff_prop_vill_s_sch prop_service_female, robust 
regress diff_prop_ph_cntr prop_service_female
regress diff_prop_all_hosp prop_service_female
regress diff_prop_disp prop_service_female
regress diff_prop_mc prop_service_female

regress diff_prop_vill_p_sch prop_service_female, vce(cluster pc01_district_id_unique)
regress diff_prop_vill_p_sch prop_service_female, robust 
regress diff_prop_vill_p_sch prop_service_female, beta 

*Running a RESET 
regress diff_prop_vill_p_sch prop_service_female 
predict y 
generate y_sq = y^2 
generate y_cube = y^3 
regress diff_prop_vill_p_sch prop_service_female y_sq y_cube 

*Adding controls 
regress diff_prop_vill_p_sch prop_service_female log_avg_vill_t_p_01 prop_vill_sc_pop_01 prop_vill_st_pop_01 prop_rural_area_01_new 

*Regressions for embeddedness 
regress diff_prop_vill_p_sch prop_service_female prop_service_embedded 

*IAS 80 Dataset 
*Importing and inspecting dataset 
clear
import delimited ias_panel_wide_80.csv
summarize 
describe, fullnames 

*Generating difference variables 
generate diff_prop_vill_p_sch = prop_vill_p_sch_11 - prop_vill_p_sch_01 
generate diff_prop_vill_m_sch = prop_vill_m_sch_11 - prop_vill_m_sch_01
generate diff_prop_vill_s_sch = prop_vill_s_sch_11 - prop_vill_s_sch_01 
generate diff_prop_ph_cntr = prop_vill_ph_cntr_11 - prop_vill_ph_cntr_01 
generate diff_prop_all_hosp = prop_vill_all_hosp_11 - prop_vill_all_hosp_01 
generate diff_prop_disp = prop_vill_disp_11 - prop_vill_disp_01 
generate diff_prop_mcw_cntr = prop_vill_mcw_cntr_11 - prop_vill_mcw_cntr_01 
generate prop_rural_area_01_new = area_rural_01/area_total_01

*CLeaning dataset 
drop prop_vill_p_sch_01 prop_vill_m_sch_01 prop_vill_s_sch_01 prop_vill_ph_cntr_01 prop_vill_all_hosp_01 prop_vill_disp_01 prop_vill_mcw_cntr_01 prop_vill_p_sch_11 prop_vill_m_sch_11 prop_vill_s_sch_11 prop_vill_ph_cntr_11 prop_vill_all_hosp_11 prop_vill_disp_11 prop_vill_mcw_cntr_11 prop_rural_area_01 prop_rural_area_11 

*Inspecting cleaned dataset 
summarize 
describe, fullnames 

*Running regressions 

regress diff_prop_vill_p_sch prop_service_female 
estat hettest
regress diff_prop_vill_m_sch prop_service_female
regress diff_prop_vill_s_sch prop_service_female

regress diff_prop_vill_s_sch prop_service_female, robust 
regress diff_prop_ph_cntr prop_service_female
regress diff_prop_all_hosp prop_service_female
regress diff_prop_disp prop_service_female
regress diff_prop_mc prop_service_female

regress diff_prop_vill_p_sch prop_service_female, vce(cluster pc01_district_id_unique)
regress diff_prop_vill_p_sch prop_service_female, robust 
regress diff_prop_vill_p_sch prop_service_female, beta 

*IAS 90 Dataset 
*Importing and inspecting dataset 
clear
import delimited ias_panel_wide_90.csv
summarize 
describe, fullnames 

*Generating difference variables 
generate diff_prop_vill_p_sch = prop_vill_p_sch_11 - prop_vill_p_sch_01 
generate diff_prop_vill_m_sch = prop_vill_m_sch_11 - prop_vill_m_sch_01
generate diff_prop_vill_s_sch = prop_vill_s_sch_11 - prop_vill_s_sch_01 
generate diff_prop_ph_cntr = prop_vill_ph_cntr_11 - prop_vill_ph_cntr_01 
generate diff_prop_all_hosp = prop_vill_all_hosp_11 - prop_vill_all_hosp_01 
generate diff_prop_disp = prop_vill_disp_11 - prop_vill_disp_01 
generate diff_prop_mcw_cntr = prop_vill_mcw_cntr_11 - prop_vill_mcw_cntr_01 
generate prop_rural_area_01_new = area_rural_01/area_total_01

*CLeaning dataset 
drop prop_vill_p_sch_01 prop_vill_m_sch_01 prop_vill_s_sch_01 prop_vill_ph_cntr_01 prop_vill_all_hosp_01 prop_vill_disp_01 prop_vill_mcw_cntr_01 prop_vill_p_sch_11 prop_vill_m_sch_11 prop_vill_s_sch_11 prop_vill_ph_cntr_11 prop_vill_all_hosp_11 prop_vill_disp_11 prop_vill_mcw_cntr_11 prop_rural_area_01 prop_rural_area_11 

*Inspecting cleaned dataset 
summarize 
describe, fullnames 

*Running regressions 

regress diff_prop_vill_p_sch prop_service_female 
estat hettest
regress diff_prop_vill_m_sch prop_service_female
regress diff_prop_vill_s_sch prop_service_female

regress diff_prop_vill_s_sch prop_service_female, robust 
regress diff_prop_ph_cntr prop_service_female
regress diff_prop_all_hosp prop_service_female
regress diff_prop_disp prop_service_female
regress diff_prop_mc prop_service_female

regress diff_prop_vill_p_sch prop_service_female, vce(cluster pc01_district_id_unique)
regress diff_prop_vill_p_sch prop_service_female, robust 
regress diff_prop_vill_p_sch prop_service_female, beta 

*Running regressions for maternity healthcare centres 
regress diff_prop_mcw_cntr prop_service_female
regress diff_prop_mcw_cntr prop_service_female, robust 
regress diff_prop_mcw_cntr prop_service_female, vce(cluster pc01_district_id_unique)
regress diff_prop_mcw_cntr prop_service_female, beta 

*Adding controls 
generate log_avg_vill_t_p_01 = log(avg_vill_t_p_01)
regress diff_prop_mcw_cntr prop_service_female prop_service_embedded
regress diff_prop_mcw_cntr prop_service_female prop_service_embedded log_avg_vill_t_p_01 
regress diff_prop_mcw_cntr prop_service_female prop_service_embedded log_avg_vill_t_p_01 prop_vill_sc_pop_01
regress diff_prop_mcw_cntr prop_service_female prop_service_embedded log_avg_vill_t_p_01 prop_vill_sc_pop_01 prop_vill_st_pop_01 
regress diff_prop_mcw_cntr prop_service_female prop_service_embedded log_avg_vill_t_p_01 prop_vill_sc_pop_01 prop_vill_st_pop_01 prop_rural_area_01_new

