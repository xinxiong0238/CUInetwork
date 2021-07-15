## code to prepare `my_dataset` dataset goes here
setwd("~/Xin/CUInetwork")
edge_matrix_full = readRDS("~/zipped_file_xin/CUInetwork/data/edge_matrix_cut_0.1.RDS")
dict.combine = readRDS("~/zipped_file_xin/CUInetwork/data/dict_combine.RDS")
input_table = readRDS("~/zipped_file_xin/CUInetwork/data/input_table.RDS")
color.df = readRDS("~/zipped_file_xin/CUInetwork/data/color_df.RDS")

data = list(`edge_matrix_full` = edge_matrix_full,
            `dict.combine` = dict.combine,
            `input_table` = input_table,
            `color.df` = color.df)

saveRDS(data, file = "~/zipped_file_xin/golem_app/CUInetwork_data.RDS")

usethis::use_data(color.df, overwrite = TRUE)
