# reference character vectors for checking the contents of output files

OUTPUT_DIR <- "model-examples/1"

# fake result object
RES1 <- import_result(OUTPUT_DIR)

# .lst file
LST_TEST_FILE <- "model-examples/1/1.lst"

LST_FULL_VEC <- readr::read_lines(LST_TEST_FILE)

LST_REF_DEFAULT <- c("Fri Jan 31 16:45:54 EST 2020", "$PROBLEM PK model 1 cmt base",
                      "", "...", " ", " Elapsed finaloutput time in seconds:     0.00",
                      " #CPUT: Total CPU Time in Seconds,        2.042", "Stop Time:",
                      "Fri Jan 31 16:46:00 EST 2020")

LST_REF_0_5 <- c("...", " ", " Elapsed finaloutput time in seconds:     0.00",
                 " #CPUT: Total CPU Time in Seconds,        2.042", "Stop Time:",
                 "Fri Jan 31 16:46:00 EST 2020")

LST_REF_5_0 <- c("Fri Jan 31 16:45:54 EST 2020", "$PROBLEM PK model 1 cmt base",
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../data/acop.csv IGNORE=@", "...")

LST_REF_1_5 <- c("Fri Jan 31 16:45:54 EST 2020", "...", " ", " Elapsed finaloutput time in seconds:     0.00",
                 " #CPUT: Total CPU Time in Seconds,        2.042", "Stop Time:",
                 "Fri Jan 31 16:46:00 EST 2020")

LST_REF_5_1 <- c("Fri Jan 31 16:45:54 EST 2020", "$PROBLEM PK model 1 cmt base",
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../data/acop.csv IGNORE=@",
                 "...", "Fri Jan 31 16:46:00 EST 2020")


OUTPUT_FILE <- file.path(OUTPUT_DIR, "OUTPUT")

# directory ls stuff
OUTPUT_DIR_LS <- fs::dir_ls(OUTPUT_DIR)
CTL_FILTER <- ".ctl"
CTL_FILTER_RES <- as.character(grep(CTL_FILTER, OUTPUT_DIR_LS, value = TRUE))

# table output
EXT_REF_FLOOR_0 <- "data/acop_ext_ref_floor0_200211.rds"
EXT_REF_FLOOR_NULL <- "data/acop_ext_ref_floorNULL_200211.rds"

GRD_REF_FLOOR_0 <- "data/acop_grd_ref_floor0_200228.rds"
GRD_REF_FLOOR_10 <- "data/acop_grd_ref_floor10_200228.rds"
GRD_REF_FLOOR_NULL <- "data/acop_grd_ref_floorNULL_200228.rds"

