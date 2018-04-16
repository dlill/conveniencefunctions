# ---- Define reactions
reactions <- structure(list(smatrix = structure(c(-1, NA, 1, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, 1, -1, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, 1, -1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     -1, 1, -1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, -1, NA, NA,
                                     -1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, -1, NA, NA, -1, 1,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, 1, -1, 1, -1, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, -1, -1, NA, NA, 1, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, 1, 1, -1, -1, -1, 1, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, 1, 1, NA, -1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, -1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, -1,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1, 1, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, 1, -1, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, 1, 1, -1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, 1, 1, -1), .Dim = c(28L, 17L), .Dimnames = list(c("1",
                                                                                               "11", "12", "13", "14", "15", "16", "17", "18", "19", "110",
                                                                                               "111", "112", "113", "114", "115", "116", "117", "118", "119",
                                                                                               "120", "121", "122", "123", "124", "125", "126", "127"), c("cRAF_0_0",
                                                                                                                                                          "cRAF_1_0", "cRAF_1_1", "MEK_0_0", "MEK_1_0", "MEK_0_1", "MEK_1_1",
                                                                                                                                                          "ERK", "pERK", "ppERK", "AKT", "pAKT", "PAK", "pPAK", "switch",
                                                                                                                                                          "RAS", "PI3K"))), states = c("cRAF_0_0", "cRAF_1_0", "cRAF_1_1",
                                                                                                                                                                                       "MEK_0_0", "MEK_1_0", "MEK_0_1", "MEK_1_1", "ERK", "pERK", "ppERK",
                                                                                                                                                                                       "AKT", "pAKT", "PAK", "pPAK", "switch", "RAS", "PI3K"), rates = c("phospho_cRAF*(RAS+RAS_0)*pPAK*cRAF_0_0",
                                                                                                                                                                                                                                                         "deact_cRAF*cRAF_1_0", "dephospho_cRAF*cRAF_1_1", "phospho_MEK_0_0*MEK_0_0*cRAF_1_0",
                                                                                                                                                                                                                                                         "dephospho_MEK_1_0*MEK_1_0", "stim_PAK_MEK_0_0*pPAK*MEK_0_0",
                                                                                                                                                                                                                                                         "dephospho_MEK_0_1*MEK_0_1", "stim_PAK_MEK_1_0*pPAK*MEK_1_0",
                                                                                                                                                                                                                                                         "dephospho_MEK*MEK_1_1", "phospho_MEK_0_1*MEK_0_1*cRAF_1_0",
                                                                                                                                                                                                                                                         "dephospho_MEK_1_1*MEK_1_1", "phospho_ERK_MEK_1_0*ERK*MEK_1_0",
                                                                                                                                                                                                                                                         "(phospho_ERK_MEK_1_0+enhanced_ERK_MEK_1_1)*ERK*MEK_1_1", "phospho_pERK_MEK_1_0*pERK*MEK_1_0",
                                                                                                                                                                                                                                                         "(phospho_pERK_MEK_1_0+enhanced_pERK_MEK_1_1)*pERK*MEK_1_1",
                                                                                                                                                                                                                                                         "dephospho_pERK*pERK", "dephospho_ppERK*ppERK", "phospho_AKT*AKT*(PI3K+PI3K_0)",
                                                                                                                                                                                                                                                         "dephospho_pAKT*pAKT", "phospho_PAK*(PI3K+PI3K_0)*PAK", "deactpPAK*pPAK",
                                                                                                                                                                                                                                                         "0", "actRAS_drug*(1*exp(-(time-0)^2/(2*5^2))/(5*2.506628))",
                                                                                                                                                                                                                                                         "switch*actRAS_egf*(1*exp(-(time-60)^2/(2*5^2))/(5*2.506628))",
                                                                                                                                                                                                                                                         "deactRAS*RAS", "actPI3K_drug*(1*exp(-(time-0)^2/(2*5^2))/(5*2.506628))",
                                                                                                                                                                                                                                                         "switch*actPI3K_egf*(1*exp(-(time-60)^2/(2*5^2))/(5*2.506628))",
                                                                                                                                                                                                                                                         "deactPI3K*PI3K"), volumes = NULL, description = c("Stimulated cRAF_0_0 to cRAF_1_0",
                                                                                                                                                                                                                                                                                                            "Deactivate cRAF_1_0 to cRAF_1_1", "cRAF_1_1 to cRAF_0_0", "MEK_0_0 to MEK_1_0",
                                                                                                                                                                                                                                                                                                            "MEK_1_0 to MEK_0_0", "Stimulated MEK_0_0 to MEK_0_1 ", "MEK_0_1 to MEK_0_0",
                                                                                                                                                                                                                                                                                                            "Stimulated MEK_1_0 to MEK_1_1", "MEK_1_1 to MEK_1_0", "MEK_0_1 to MEK_1_1",
                                                                                                                                                                                                                                                                                                            "MEK_1_1 to MEK_0_1", "ERK to pERK by MEK_1_0", "ERK to pERK by MEK_1_1",
                                                                                                                                                                                                                                                                                                            "pERK to ppERK by MEK_1_0", "pERK to ppERK by MEK_1_1", "pERK to ERK",
                                                                                                                                                                                                                                                                                                            "ppERK to pERK", "AKT to pAKT", "pAKT to AKT", "PAK phosphorylation",
                                                                                                                                                                                                                                                                                                            "Deactivate pPAK", "EGF_induction (No SS)", "Activate RAS by drug (No SS)",
                                                                                                                                                                                                                                                                                                            "Activate RAS by EGF (No SS)", "Deactivate RAS (No SS)", "Activate PI3K by drug (No SS)",
                                                                                                                                                                                                                                                                                                            "Activate PI3K by EGF (No SS)", "Deactivate PI3K (No SS)")), .Names = c("smatrix",
                                                                                                                                                                                                                                                                                                                                                                                    "states", "rates", "volumes", "description"), class = c("eqnlist",
                                                                                                                                                                                                                                                                                                                                                                                                                                            "list"))

# output for mathematica, keep only the ones with "MEK" in their name
reactions %>% as.eqnvec() %>% str_subset_name("MEK") %>% print_mathematica.eqnvec()

reactions %>% as.eqnvec() %>% str_subset_name("MEK") %>% names %>% print_mathematica.character() %>% .[[1]]

# solve in Mathematica
# mysolution = Solve[f==0, x];
# Export["myfile.txt", mysolution//Simplify]
# gives "solutions"

solutions <- structure(c("(cRAF10*MEK00*(dephosphoMEK01*dephosphoMEK11*phosphoMEK00 + dephosphoMEK*(dephosphoMEK01*phosphoMEK00 + cRAF10*phosphoMEK00*phosphoMEK01 + phosphoMEK01*pPAK*stimPAKMEK00)))/(dephosphoMEK*dephosphoMEK10*(dephosphoMEK01 + cRAF10*phosphoMEK01) + dephosphoMEK01*dephosphoMEK11*(dephosphoMEK10 + pPAK*stimPAKMEK10))",
            "(MEK00*pPAK*(dephosphoMEK10*(dephosphoMEK + dephosphoMEK11)*stimPAKMEK00 + dephosphoMEK11*(cRAF10*phosphoMEK00 + pPAK*stimPAKMEK00)*stimPAKMEK10))/(dephosphoMEK*dephosphoMEK10*(dephosphoMEK01 + cRAF10*phosphoMEK01) + dephosphoMEK01*dephosphoMEK11*(dephosphoMEK10 + pPAK*stimPAKMEK10))",
            "(cRAF10*MEK00*pPAK*(dephosphoMEK10*phosphoMEK01*stimPAKMEK00 + (dephosphoMEK01*phosphoMEK00 + cRAF10*phosphoMEK00*phosphoMEK01 + phosphoMEK01*pPAK*stimPAKMEK00)*stimPAKMEK10))/(dephosphoMEK*dephosphoMEK10*(dephosphoMEK01 + cRAF10*phosphoMEK01) + dephosphoMEK01*dephosphoMEK11*(dephosphoMEK10 + pPAK*stimPAKMEK10))"
), .Names = c("MEK10", "MEK01", "MEK11"))

# resubstitute original characters
replacements <- reactions %>% as.eqnvec() %>% str_subset_name("MEK") %>% getSymbols() %>% print_mathematica.character() %>% .[[2]]


print(solutions %>% str_replace_all(replacements))
