

IO = list()

IO$public_output_data = paste0("../Data/",par$dataset, "/")
IO$out_Rdata = paste0(IO$public_output_data, "Rdata/")
IO$out_csv = paste0(IO$public_output_data, "CSV/")

if(!dir.exists(IO$public_output_data)){dir.create(IO$public_output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

source("../../PL-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R")


IO$input_data = paste0(IO$r_Data,"input_data/")
IO$tmp_data = paste0(IO$r_Data, "tmp_data/")
IO$output_data = paste0(IO$r_Data,"output_data/")
if(!dir.exists(IO$tmp_data)){dir.create(IO$tmp_data)}
if(!dir.exists(IO$output_data)){dir.create(IO$output_data)}




IO$panels = "../Figures Tables Media/Figures/panels/"
IO$tables = "../Figures Tables Media/Tables/"


