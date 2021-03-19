import petab
petab.create_parameter_df(sbml_model    = petab.get_sbml_model("petab/enzymeKinetics/model_enzymeKinetics.xml"),
                         condition_df   = petab.get_condition_df("petab/enzymeKinetics/experimentalCondition_enzymeKinetics.tsv"),
                         observable_df  = petab.get_observable_df("petab/enzymeKinetics/observables_enzymeKinetics.tsv"),
                         measurement_df = petab.get_measurement_df("petab/enzymeKinetics/measurementData_enzymeKinetics.tsv"))
# 
# import petab
# petab.create_parameter_df(sbml_model    = petab.get_sbml_model("petab/Boehm_JProteomeRes2014/model_Boehm_JProteomeRes2014.xml"),
#                          condition_df   = petab.get_condition_df("petab/Boehm_JProteomeRes2014/experimentalCondition_Boehm_JProteomeRes2014.tsv"),
#                          observable_df  = petab.get_observable_df("petab/Boehm_JProteomeRes2014/observables_Boehm_JProteomeRes2014.tsv"),
#                          measurement_df = petab.get_measurement_df("petab/Boehm_JProteomeRes2014/measurementData_Boehm_JProteomeRes2014.tsv"))

