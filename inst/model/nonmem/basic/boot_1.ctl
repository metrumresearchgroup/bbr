$PROB Bootstrap model for 1.ctl
; Note: This is _not_ a NONMEM control stream file
; Do not delete or modify this file, as it is needed to summarize results

$DESIGN bbr bootstrap
 $MODEL
   CONTEXT: Bootstrap run 01 for model 1
   MODEL_FILE: 01.ctl
   YAML: 01.yaml
   DATA: data/boot_1_01.csv

 $MODEL
   CONTEXT: Bootstrap run 02 for model 1
   MODEL_FILE: 02.ctl
   YAML: 02.yaml
   DATA: data/boot_1_02.csv

 $MODEL
   CONTEXT: Bootstrap run 03 for model 1
   MODEL_FILE: 03.ctl
   YAML: 03.yaml
   DATA: data/boot_1_03.csv

 $MODEL
   CONTEXT: Bootstrap run 04 for model 1
   MODEL_FILE: 04.ctl
   YAML: 04.yaml
   DATA: data/boot_1_04.csv

 $MODEL
   CONTEXT: Bootstrap run 05 for model 1
   MODEL_FILE: 05.ctl
   YAML: 05.yaml
   DATA: data/boot_1_05.csv

 $MODEL
   CONTEXT: Bootstrap run 06 for model 1
   MODEL_FILE: 06.ctl
   YAML: 06.yaml
   DATA: data/boot_1_06.csv

 $MODEL
   CONTEXT: Bootstrap run 07 for model 1
   MODEL_FILE: 07.ctl
   YAML: 07.yaml
   DATA: data/boot_1_07.csv

 $MODEL
   CONTEXT: Bootstrap run 08 for model 1
   MODEL_FILE: 08.ctl
   YAML: 08.yaml
   DATA: data/boot_1_08.csv

 $MODEL
   CONTEXT: Bootstrap run 09 for model 1
   MODEL_FILE: 09.ctl
   YAML: 09.yaml
   DATA: data/boot_1_09.csv

 $MODEL
   CONTEXT: Bootstrap run 10 for model 1
   MODEL_FILE: 10.ctl
   YAML: 10.yaml
   DATA: data/boot_1_10.csv
