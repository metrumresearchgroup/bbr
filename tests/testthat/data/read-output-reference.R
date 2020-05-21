# reference character vectors for checking the contents of output files

OUTPUT_DIR <- "model-examples/1"

# fake model object

MOD1 <- read_model(OUTPUT_DIR, .directory = NULL)

# .lst file
LST_TEST_FILE <- "model-examples/1/1.lst"

LST_FULL_VEC <- readr::read_lines(LST_TEST_FILE)

LST_REF_DEFAULT <- c("Wed May 20 16:45:27 EDT 2020", "$PROBLEM PK model 1 cmt base",
                     "", "...", " ", " Elapsed finaloutput time in seconds:     0.11",
                     " #CPUT: Total CPU Time in Seconds,        2.543", "Stop Time:",
                     "Wed May 20 16:45:38 EDT 2020")

LST_REF_0_5 <- c("...", " ", " Elapsed finaloutput time in seconds:     0.11",
                 " #CPUT: Total CPU Time in Seconds,        2.543", "Stop Time:",
                 "Wed May 20 16:45:38 EDT 2020")

LST_REF_5_0 <- c("Wed May 20 16:45:27 EDT 2020", "$PROBLEM PK model 1 cmt base",
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../../data/acop.csv IGNORE=@", "...")

LST_REF_1_5 <- c("Wed May 20 16:45:27 EDT 2020", "...", " ", " Elapsed finaloutput time in seconds:     0.11",
                 " #CPUT: Total CPU Time in Seconds,        2.543", "Stop Time:",
                 "Wed May 20 16:45:38 EDT 2020")

LST_REF_5_1 <- c("Wed May 20 16:45:27 EDT 2020", "$PROBLEM PK model 1 cmt base",
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../../data/acop.csv IGNORE=@",
                 "...", "Wed May 20 16:45:38 EDT 2020")


OUTPUT_FILE <- file.path(OUTPUT_DIR, "OUTPUT")

# directory ls stuff
OUTPUT_DIR_LS <- fs::dir_ls(OUTPUT_DIR)
CTL_FILTER <- ".ctl"
CTL_FILTER_RES <- as.character(grep(CTL_FILTER, OUTPUT_DIR_LS, value = TRUE))

# table output
EXT_REF_FLOOR_0 <- "data/acop_ext_ref_floor0_200520.rds"
EXT_REF_FLOOR_NULL <- "data/acop_ext_ref_floorNULL_200520.rds"

GRD_REF_FLOOR_0 <- "data/acop_grd_ref_floor0_200520.rds"
GRD_REF_FLOOR_10 <- "data/acop_grd_ref_floor10_200520.rds"
GRD_REF_FLOOR_NULL <- "data/acop_grd_ref_floorNULL_200520.rds"

