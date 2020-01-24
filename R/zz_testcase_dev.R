#' These are dev test cases that will eventually be turned into real tests

########
# # utils test cases
# .args1 <- list("json" = T, "threads" = 4)
# .args2 <- list("json" = T, "threads" = 4, "naw" = "naw")
# .args3 <- list("json" = T, "threads" = 4, "debug" = "naw")
# .args4 <- list("json" = T, "threads" = 4, "debug" = F)
# check_nonmem_args(.args1) # "--json --threads=4"
# check_nonmem_args(.args2) # naw is not a valid argument for the `.args`
# check_nonmem_args(.args3) # `naw` passed for arg `debug` -- Must inherit from class 'logical'
# check_nonmem_args(.args4) # "--json --threads=4"

########
# # submit-nonmem-model test cases
# submit_nonmem_model("/data/240/001.mod", .type="local")
# submit_nonmem_model("/data/240/001.mod", .args = list("overwrite" = TRUE), .type="local")
# submit_nonmem_model("/data/240/001.mod", .args = .args1, .type="local")
# submit_nonmem_model("/data/240/[001:004].mod", .args = list("overwrite" = TRUE))
# submit_nonmem_model("/data/240/[001:004].mod", .args = list("overwrite" = TRUE), .type="local")

########
# # read-model-results test cases
# res <- model_summary("/data/240/001/001.mod"); names(res)
