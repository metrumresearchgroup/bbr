
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
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../data/acop.csv IGNORE=@")

LST_REF_1_5 <- c("Fri Jan 31 16:45:54 EST 2020", "...", " ", " Elapsed finaloutput time in seconds:     0.00",
                 " #CPUT: Total CPU Time in Seconds,        2.042", "Stop Time:",
                 "Fri Jan 31 16:46:00 EST 2020")

LST_REF_5_1 <- c("Fri Jan 31 16:45:54 EST 2020", "$PROBLEM PK model 1 cmt base",
                 "", "$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN", "$DATA ../data/acop.csv IGNORE=@",
                 "...", "Fri Jan 31 16:46:00 EST 2020")

