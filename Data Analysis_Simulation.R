library(readxl)
library(ggplot2)
library(ggpubr)
########################   导入数据   #################################
MAE_laplace_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/MAE_Laplace_5_20.csv")[,-1]
miu_laplace_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/miu_laplace_5_20.csv")[,-1]
sigma_laplace_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/sigma_square_laplace_5_20.csv")[,-1]
m_laplace_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/m_square_laplace_5_20.csv")[,-1]
MAE_laplace_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/MAE_Laplace_5_50.csv")[,-1]
miu_laplace_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/miu_laplace_5_50.csv")[,-1]
sigma_laplace_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/sigma_square_laplace_5_50.csv")[,-1]
m_laplace_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/m_square_laplace_5_50.csv")[,-1]
MAE_laplace_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/MAE_Laplace_10_20.csv")[,-1]*300/200
miu_laplace_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/miu_laplace_10_20.csv")[,-1]
sigma_laplace_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/sigma_square_laplace_10_20.csv")[,-1]
m_laplace_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/m_square_laplace_10_20.csv")[,-1]
MAE_laplace_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/MAE_Laplace_10_50.csv")[,-1]*300/200
miu_laplace_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/miu_laplace_10_50.csv")[,-1]
sigma_laplace_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/sigma_square_laplace_10_50.csv")[,-1]
m_laplace_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/m_square_laplace_10_50.csv")[,-1]
MAE_laplace_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/MAE_Laplace_20_20.csv")[,-1]
miu_laplace_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/miu_laplace_20_20.csv")[,-1]
sigma_laplace_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/sigma_square_laplace_20_20.csv")[,-1]
m_laplace_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/m_square_laplace_20_20.csv")[,-1]
MAE_laplace_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/MAE_Laplace_20_50.csv")[,-1]*300/200
miu_laplace_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/miu_laplace_20_50.csv")[,-1]
sigma_laplace_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/sigma_square_laplace_20_50.csv")[,-1]
m_laplace_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/m_square_laplace_20_50.csv")[,-1]

MAE_logistic_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/MAE_logistic_5_20.csv")[,-1]
miu_logistic_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/miu_logistic_5_20.csv")[,-1]
sigma_logistic_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/sigma_square_logistic_5_20.csv")[,-1]
m_logistic_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/m_square_logistic_5_20.csv")[,-1]
MAE_logistic_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/MAE_logistic_5_50.csv")[,-1]
miu_logistic_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/miu_logistic_5_50.csv")[,-1]
sigma_logistic_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/sigma_square_logistic_5_50.csv")[,-1]
m_logistic_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/m_square_logistic_5_50.csv")[,-1]
MAE_logistic_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/MAE_logistic_10_20.csv")[,-1]*300/200
miu_logistic_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/miu_logistic_10_20.csv")[,-1]
sigma_logistic_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/sigma_square_logistic_10_20.csv")[,-1]
m_logistic_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/m_square_logistic_10_20.csv")[,-1]
MAE_logistic_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/MAE_logistic_10_50.csv")[,-1]*300/200
miu_logistic_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/miu_logistic_10_50.csv")[,-1]
sigma_logistic_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/sigma_square_logistic_10_50.csv")[,-1]
m_logistic_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/m_square_logistic_10_50.csv")[,-1]
MAE_logistic_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/MAE_logistic_20_20.csv")[,-1]
miu_logistic_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/miu_logistic_20_20.csv")[,-1]
sigma_logistic_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/sigma_square_logistic_20_20.csv")[,-1]
m_logistic_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/m_square_logistic_20_20.csv")[,-1]
MAE_logistic_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/MAE_logistic_20_50.csv")[,-1]*300/200
miu_logistic_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/miu_logistic_20_50.csv")[,-1]
sigma_logistic_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/sigma_square_logistic_20_50.csv")[,-1]
m_logistic_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/m_square_logistic_20_50.csv")[,-1]

MAE_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/MAE_Pearson_5_20.csv")[,-1]
miu_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/miu_Pearson_5_20.csv")[,-1]
sigma_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/sigma_square_Pearson_5_20.csv")[,-1]
m_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/m_square_Pearson_5_20.csv")[,-1]
MAE_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/MAE_Pearson_5_50.csv")[,-1]
miu_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/miu_Pearson_5_50.csv")[,-1]
sigma_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/sigma_square_Pearson_5_50.csv")[,-1]
m_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/m_square_Pearson_5_50.csv")[,-1]
MAE_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/MAE_Pearson_10_20.csv")[,-1]*300/200
miu_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/miu_Pearson_10_20.csv")[,-1]
sigma_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/sigma_square_Pearson_10_20.csv")[,-1]
m_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/m_square_Pearson_10_20.csv")[,-1]
MAE_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/MAE_Pearson_10_50.csv")[,-1]*300/200
miu_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/miu_Pearson_10_50.csv")[,-1]
sigma_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/sigma_square_Pearson_10_50.csv")[,-1]
m_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/m_square_Pearson_10_50.csv")[,-1]
MAE_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/MAE_Pearson_20_20.csv")[,-1]
miu_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/miu_Pearson_20_20.csv")[,-1]
sigma_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/sigma_square_Pearson_20_20.csv")[,-1]
m_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/m_square_Pearson_20_20.csv")[,-1]
MAE_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/MAE_Pearson_20_50.csv")[,-1]*300/200
miu_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/miu_Pearson_20_50.csv")[,-1]
sigma_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/sigma_square_Pearson_20_50.csv")[,-1]
m_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/m_square_Pearson_20_50.csv")[,-1]
mean(sigma_Pearson_10_20)

MAE_t_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/MAE_t_5_20.csv")[,-1]
miu_t_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/miu_t_5_20.csv")[,-1]
sigma_t_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/sigma_square_t_5_20.csv")[,-1]
m_t_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/m_square_t_5_20.csv")[,-1]
MAE_t_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/MAE_t_5_50.csv")[,-1]
miu_t_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/miu_t_5_50.csv")[,-1]
sigma_t_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/sigma_square_t_5_50.csv")[,-1]
m_t_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/m_square_t_5_50.csv")[,-1]
MAE_t_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/MAE_t_10_20.csv")[,-1]*300/200
miu_t_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/miu_t_10_20.csv")[,-1]
sigma_t_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/sigma_square_t_10_20.csv")[,-1]
m_t_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/m_square_t_10_20.csv")[,-1]
MAE_t_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/MAE_t_10_50.csv")[,-1]*300/200
miu_t_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/miu_t_10_50.csv")[,-1]
sigma_t_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/sigma_square_t_10_50.csv")[,-1]
m_t_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/m_square_t_10_50.csv")[,-1]
MAE_t_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/MAE_t_20_20.csv")[,-1]
miu_t_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/miu_t_20_20.csv")[,-1]
sigma_t_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/sigma_square_t_20_20.csv")[,-1]
m_t_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/m_square_t_20_20.csv")[,-1]
MAE_t_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/MAE_t_20_50.csv")[,-1]*300/200
miu_t_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/miu_t_20_50.csv")[,-1]
sigma_t_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/sigma_square_t_20_50.csv")[,-1]
m_t_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/m_square_t_20_50.csv")[,-1]

MAE_norm_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/MAE_norm_5_20.csv")[,-1]
miu_norm_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/miu_norm_5_20.csv")[,-1]
sigma_norm_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/sigma_square_norm_5_20.csv")[,-1]
m_norm_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/m_square_norm_5_20.csv")[,-1]
MAE_norm_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/MAE_norm_5_50.csv")[,-1]
miu_norm_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/miu_norm_5_50.csv")[,-1]
sigma_norm_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/sigma_square_norm_5_50.csv")[,-1]
m_norm_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/m_square_norm_5_50.csv")[,-1]
MAE_norm_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/MAE_norm_10_20.csv")[,-1]*300/200
miu_norm_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/miu_norm_10_20.csv")[,-1]
sigma_norm_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/sigma_square_norm_10_20.csv")[,-1]
m_norm_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/m_square_norm_10_20.csv")[,-1]
MAE_norm_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/MAE_norm_10_50.csv")[,-1]*300/200
miu_norm_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/miu_norm_10_50.csv")[,-1]
sigma_norm_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/sigma_square_norm_10_50.csv")[,-1]
m_norm_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/m_square_norm_10_50.csv")[,-1]
MAE_norm_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/MAE_norm_20_20.csv")[,-1]
miu_norm_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/miu_norm_20_20.csv")[,-1]
sigma_norm_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/sigma_square_norm_20_20.csv")[,-1]
m_norm_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/m_square_norm_20_20.csv")[,-1]
MAE_norm_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/MAE_norm_20_50.csv")[,-1]*300/200
miu_norm_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/miu_norm_20_50.csv")[,-1]
sigma_norm_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/sigma_square_norm_20_50.csv")[,-1]
m_norm_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/m_square_norm_20_50.csv")[,-1]

v_t_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/v_t_5_20.csv")[,-1]
v_t_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/v_t_5_50.csv")[,-1]
v_t_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/v_t_10_20.csv")[,-1]
v_t_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/v_t_10_50.csv")[,-1]
v_t_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/v_t_20_20.csv")[,-1]
v_t_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/v_t_20_50.csv")[,-1]

v_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/v_Pearson_5_20.csv")[,-1]
v_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/v_Pearson_5_50.csv")[,-1]
v_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/v_Pearson_10_20.csv")[,-1]
v_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/v_Pearson_10_50.csv")[,-1]
v_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/v_Pearson_20_20.csv")[,-1]
v_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/v_Pearson_20_50.csv")[,-1]

delta_Pearson_5_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_20/delta_Pearson_5_20.csv")[,-1]
delta_Pearson_5_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 5_50/delta_Pearson_5_50.csv")[,-1]
delta_Pearson_10_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_20/delta_Pearson_10_20.csv")[,-1]
delta_Pearson_10_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 10_50/delta_Pearson_10_50.csv")[,-1]
delta_Pearson_20_20 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_20/delta_Pearson_20_20.csv")[,-1]
delta_Pearson_20_50 = read.csv("C:/Users/admin/Desktop/SMN代码/Outlier/outlier 20_50/delta_Pearson_20_50.csv")[,-1]

MAE_laplace_10_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 10_80/MAE_Laplace_10_80.csv")[,-1]
MAE_t_10_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 10_80/MAE_t_10_80.csv")[,-1]
MAE_Pearson_10_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 10_80/MAE_Pearson_10_80.csv")[,-1]
MAE_logistic_10_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 10_80/MAE_Logistic_10_80.csv")[,-1]
MAE_norm_10_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 10_80/MAE_norm_10_80.csv")[,-1]
MAE_laplace_20_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 20_80/MAE_Laplace_20_80.csv")[,-1]
MAE_t_20_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 20_80/MAE_t_20_80.csv")[,-1]
MAE_Pearson_20_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 20_80/MAE_Pearson_20_80.csv")[,-1]
MAE_logistic_20_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 20_80/MAE_Logistic_20_80.csv")[,-1]
MAE_norm_20_80 = read.csv("C:/Users/lenovo/Desktop/工业统计/Simuatioin 2 Outlier/outlier 20_80/MAE_norm_20_80.csv")[,-1]


##################   分析Simulation 2 MAE   ##########################
MAE_10_80 = data.frame(MAE_t = MAE_t_10_80,MAE_laplace=MAE_laplace_10_80,MAE_logistic = MAE_logistic_10_80,MAE_Pearson=MAE_Pearson_10_80,MAE_norm = MAE_norm_10_80)
t_sort = rep(-1,2000)
t_sort[which(MAE_t_10_20>=2.6)] = MAE_t_10_20[which(MAE_t_10_20>=2.6)]
MAE_10_20$t_sort = t_sort
laplace_sort = rep(-1,2000)
laplace_sort[which(MAE_laplace_10_20>=2.7)] = MAE_laplace_10_20[which(MAE_laplace_10_20>=2.7)]
MAE_10_20$laplace_sort = laplace_sort
logistic_sort = rep(-1,2000)
logistic_sort[which(MAE_logistic_10_20>=2.6)] = MAE_logistic_10_20[which(MAE_logistic_10_20>=2.6)]
MAE_10_20$logistic_sort = logistic_sort
Pearson_sort = rep(-1,2000)
Pearson_sort[which(MAE_Pearson_10_20>=2.65)] = MAE_Pearson_10_20[which(MAE_Pearson_10_20>=2.65)]
MAE_10_20$Pearson_sort = Pearson_sort
norm_sort = rep(-1,2000)
norm_sort[which(MAE_norm_10_20>=2.8)] = MAE_norm_10_20[which(MAE_norm_10_20>=2.8)]
norm_sort[which(MAE_norm_10_20<=1.7)] = MAE_norm_10_20[which(MAE_norm_10_20<=1.7)]
MAE_10_20$norm_sort = norm_sort

MAE_10_50 = data.frame(MAE_t = MAE_t_10_50,MAE_laplace=MAE_laplace_10_50,MAE_logistic = MAE_logistic_10_50,MAE_Pearson=MAE_Pearson_10_50,MAE_norm = MAE_norm_10_50)
t_sort = rep(-1,2000)
t_sort[which(MAE_t_10_50>=2.45)] = MAE_t_10_50[which(MAE_t_10_50>=2.45)]
MAE_10_50$t_sort = t_sort
laplace_sort = rep(-1,2000)
laplace_sort[which(MAE_laplace_10_50>=2.9)] = MAE_laplace_10_50[which(MAE_laplace_10_50>=2.9)]
MAE_10_50$laplace_sort = laplace_sort
logistic_sort = rep(-1,2000)
logistic_sort[which(MAE_logistic_10_50>=2.9)] = MAE_logistic_10_50[which(MAE_logistic_10_50>=2.9)]
MAE_10_50$logistic_sort = logistic_sort
Pearson_sort = rep(-1,2000)
Pearson_sort[which(MAE_Pearson_10_50>=2.45)] = MAE_Pearson_10_50[which(MAE_Pearson_10_50>=2.45)]
MAE_10_50$Pearson_sort = Pearson_sort
norm_sort = rep(-1,2000)
norm_sort[which(MAE_norm_10_50>=4.4)] = MAE_norm_10_50[which(MAE_norm_10_50>=4.4)]
norm_sort[which(MAE_norm_10_50<=2.6)] = MAE_norm_10_50[which(MAE_norm_10_50<=2.6)]
MAE_10_50$norm_sort = norm_sort

MAE_20_80 = data.frame(MAE_t = MAE_t_20_80,MAE_laplace=MAE_laplace_20_80,MAE_logistic = MAE_logistic_20_80,MAE_Pearson=MAE_Pearson_20_80,MAE_norm = MAE_norm_20_80)
t_sort = rep(-1,2000)
t_sort[which(MAE_t_20_20>=3)] = MAE_t_20_20[which(MAE_t_20_20>=3)]
MAE_20_20$t_sort = t_sort
laplace_sort = rep(-1,2000)
laplace_sort[which(MAE_laplace_20_20>=3)] = MAE_laplace_20_20[which(MAE_laplace_20_20>=3)]
MAE_20_20$laplace_sort = laplace_sort
logistic_sort = rep(-1,2000)
logistic_sort[which(MAE_logistic_20_20>=3)] = MAE_logistic_20_20[which(MAE_logistic_20_20>=3)]
logistic_sort[which(MAE_logistic_20_20<=1.7)] = MAE_logistic_20_20[which(MAE_logistic_20_20<=1.7)]
MAE_20_20$logistic_sort = logistic_sort
Pearson_sort = rep(-1,2000)
Pearson_sort[which(MAE_Pearson_20_20>=3.1)] = MAE_Pearson_20_20[which(MAE_Pearson_20_20>=3.1)]
MAE_20_20$Pearson_sort = Pearson_sort
norm_sort = rep(-1,2000)
norm_sort[which(MAE_norm_20_20>=3.4)] = MAE_norm_20_20[which(MAE_norm_20_20>=3.4)]
norm_sort[which(MAE_norm_20_20<=2)] = MAE_norm_20_20[which(MAE_norm_20_20<=2)]
MAE_20_20$norm_sort = norm_sort

MAE_20_50 = data.frame(MAE_t = MAE_t_20_50,MAE_laplace=MAE_laplace_20_50,MAE_logistic = MAE_logistic_20_50,MAE_Pearson=MAE_Pearson_20_50,MAE_norm = MAE_norm_20_50)
t_sort = rep(-1,2000)
t_sort[which(MAE_t_20_50>=2.6)] = MAE_t_20_50[which(MAE_t_20_50>=2.6)]
MAE_20_50$t_sort = t_sort
laplace_sort = rep(-1,2000)
laplace_sort[which(MAE_laplace_20_50>=3.5)] = MAE_laplace_20_50[which(MAE_laplace_20_50>=3.5)]
MAE_20_50$laplace_sort = laplace_sort
logistic_sort = rep(-1,2000)
logistic_sort[which(MAE_logistic_20_50>=3.8)] = MAE_logistic_20_50[which(MAE_logistic_20_50>=3.8)]
MAE_20_50$logistic_sort = logistic_sort
Pearson_sort = rep(-1,2000)
Pearson_sort[which(MAE_Pearson_20_50>=2.6)] = MAE_Pearson_20_50[which(MAE_Pearson_20_50>=2.6)]
MAE_20_50$Pearson_sort = Pearson_sort
norm_sort = rep(-1,2000)
norm_sort[which(MAE_norm_20_50>=6.1)] = MAE_norm_20_50[which(MAE_norm_20_50>=6.1)]
norm_sort[which(MAE_norm_20_50<=3.9)] = MAE_norm_20_50[which(MAE_norm_20_50<=3.9)]
MAE_20_50$norm_sort = norm_sort



############################   画估计MAE图部分   ##########################
## 为什么aes中的颜色与直接设置不一样：https://www.it1352.com/794121.html

mycolor = c("black",rgb(213, 93, 81, maxColorValue = 255),rgb(242, 197, 73, maxColorValue = 255),rgb(112, 159, 86, maxColorValue = 255),rgb(54, 129, 193, maxColorValue = 255))

two = ggplot(data = MAE_10_80)+
  geom_boxplot(aes(x=1,y=MAE_10_80[,5]),fill = mycolor[1],outlier.shape = 16)+
  geom_segment(aes(x=0.8,xend = 1.2,y = 5.75,yend = 5.75))+
  geom_segment(aes(x=0.8,xend = 1.2,y = 3.4,yend = 3.4))+
  #geom_jitter(aes(x=1,y=MAE_10_20[,10]),width = 0.4,color = mycolor[1])+
  #geom_point(aes(x=1,y=c(mean(MAE_10_20[,5]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=2,y=MAE_10_80[,1]),fill = mycolor[2],outlier.shape = 16)+
  geom_segment(aes(x=1.8,xend = 2.2,y = 2.45,yend = 2.45))+
  geom_segment(aes(x=1.8,xend = 2.2,y = 1.5,yend = 1.5))+
  #geom_jitter(aes(x=2,y=MAE_10_20[,6]),width = 0.4,color = mycolor[2])+
  #geom_point(aes(x=2,y=c(mean(MAE_10_20[,1]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=3,y=MAE_10_80[,2]),fill = mycolor[3],outlier.shape = 16)+
  geom_segment(aes(x=2.8,xend = 3.2,y = 3.1,yend = 3.1))+
  geom_segment(aes(x=2.8,xend = 3.2,y = 1.58,yend = 1.58))+
  #geom_jitter(aes(x=3,y=MAE_10_20[,7]),width = 0.4,color = mycolor[3])+
  #geom_point(aes(x=3,y=c(mean(MAE_10_20[,2]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=4,y=MAE_10_80[,3]),fill = mycolor[4],outlier.shape = 16)+
  geom_segment(aes(x=3.8,xend = 4.2,y = 3.14,yend = 3.14))+
  geom_segment(aes(x=3.8,xend = 4.2,y = 1.65,yend = 1.65))+
  #geom_jitter(aes(x=4,y=MAE_10_20[,8]),width = 0.4,color = mycolor[4])+
  #geom_point(aes(x=4,y=c(mean(MAE_10_20[,3]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=5,y=MAE_10_80[,4]),fill = mycolor[5],outlier.shape = 16)+
  geom_segment(aes(x=4.8,xend = 5.2,y = 2.45,yend = 2.45))+
  geom_segment(aes(x=4.8,xend = 5.2,y = 1.47,yend = 1.47))+
  #geom_jitter(aes(x=5,y=MAE_10_20[,9]),width = 0.4,color = mycolor[5])+
  #geom_point(aes(x=5,y=c(mean(MAE_10_20[,4]),rep(-1,1999))),color = 'white',size = 1)+
  #geom_hline(yintercept = c(mean(MAE_10_20[,5])),linetype = 2)+
  theme(plot.margin = margin(1, 0.5, 0.5, 0.1, unit = "cm"),panel.background = element_blank(),axis.line = element_line(),text=element_text(size=16,family = 'serif')) +
  labs(y=NULL,x=NULL)+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)+
  annotate(geom = 'segment', y = Inf, yend = -Inf, color = 'black', x = Inf, xend = Inf, size = 0.5)+
  scale_x_continuous(breaks = 3,labels = "S=10,D=80")+
  scale_y_continuous(limits = c(1,10))
  
  

one = ggplot(data = MAE_10_50)+
  geom_boxplot(aes(x=1,y=MAE_10_50[,5]),fill = mycolor[1],outlier.shape = 16)+
  geom_segment(aes(x=0.8,xend = 1.2,y = 4.4,yend = 4.4))+
  geom_segment(aes(x=0.8,xend = 1.2,y = 2.6,yend = 2.6))+
  #geom_jitter(aes(x=1,y=MAE_10_50[,10]),width = 0.4,color = mycolor[1])+
  #geom_point(aes(x=1,y=c(mean(MAE_10_50[,5]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=2,y=MAE_10_50[,1]),fill = mycolor[2],outlier.shape = 16)+
  geom_segment(aes(x=1.8,xend = 2.2,y = 2.42,yend = 2.42))+
  geom_segment(aes(x=1.8,xend = 2.2,y = 1.47,yend = 1.47))+
  #geom_jitter(aes(x=2,y=MAE_10_50[,6]),width = 0.4,color = mycolor[2])+
  #geom_point(aes(x=2,y=c(mean(MAE_10_50[,1]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=3,y=MAE_10_50[,2]),fill = mycolor[3],outlier.shape = 16)+
  geom_segment(aes(x=2.8,xend = 3.2,y = 2.9,yend = 2.9))+
  geom_segment(aes(x=2.8,xend = 3.2,y = 1.55,yend = 1.55))+
  #geom_jitter(aes(x=3,y=MAE_10_50[,7]),width = 0.4,color = mycolor[3])+
  #geom_point(aes(x=3,y=c(mean(MAE_10_50[,2]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=4,y=MAE_10_50[,3]),fill = mycolor[4],outlier.shape = 16)+
  geom_segment(aes(x=3.8,xend = 4.2,y = 2.87,yend = 2.87))+
  geom_segment(aes(x=3.8,xend = 4.2,y = 1.6,yend = 1.6))+
  #geom_jitter(aes(x=4,y=MAE_10_50[,8]),width = 0.4,color = mycolor[4])+
  #geom_point(aes(x=4,y=c(mean(MAE_10_50[,3]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=5,y=MAE_10_50[,4]),fill = mycolor[5],outlier.shape = 16)+
  geom_segment(aes(x=4.8,xend = 5.2,y = 2.42,yend = 2.42))+
  geom_segment(aes(x=4.8,xend = 5.2,y = 1.46,yend = 1.46))+
  #geom_jitter(aes(x=5,y=MAE_10_50[,9]),width = 0.4,color = mycolor[5])+
  #geom_point(aes(x=5,y=c(mean(MAE_10_50[,4]),rep(-1,1999))),color = 'white',size = 1)+
  #geom_hline(yintercept = c(mean(MAE_10_50[,5])),linetype = 2)+
  theme(plot.margin = margin(1, 0.5, 0.5, 0.1, unit = "cm"),panel.background = element_blank(),axis.line = element_line(),text=element_text(size=16,family = 'serif')) +
  labs(y=NULL,x=NULL)+
  annotate(geom = 'segment', y = Inf, yend = -Inf, color = 'black', x = Inf, xend = Inf, size = 0.5)+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)+
  scale_x_continuous(breaks = 3,labels = "S=10,D=50")+
  scale_y_continuous(limits = c(1,10))



four = ggplot(data = MAE_20_80)+
  geom_boxplot(aes(x=1,y=MAE_20_80[,5]),fill = mycolor[1],outlier.shape = 16)+
  geom_segment(aes(x=0.8,xend = 1.2,y = 9.2,yend = 9.2))+
  geom_segment(aes(x=0.8,xend = 1.2,y = 5.9,yend = 5.9))+
  #geom_jitter(aes(x=1,y=MAE_20_20[,10]),width = 0.4,color = mycolor[1])+
  #geom_point(aes(x=1,y=c(mean(MAE_20_20[,5]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=2,y=MAE_20_80[,1]),fill = mycolor[2],outlier.shape = 16)+
  geom_segment(aes(x=1.8,xend = 2.2,y = 2.6,yend = 2.6))+
  geom_segment(aes(x=1.8,xend = 2.2,y = 1.55,yend = 1.55))+
  #geom_jitter(aes(x=2,y=MAE_20_20[,6]),width = 0.4,color = mycolor[2])+
  #geom_point(aes(x=2,y=c(mean(MAE_20_20[,1]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=3,y=MAE_20_80[,2]),fill = mycolor[3],outlier.shape = 16)+
  geom_segment(aes(x=2.8,xend = 3.2,y = 3.67,yend = 3.67))+
  geom_segment(aes(x=2.8,xend = 3.2,y = 1.8,yend = 1.8))+
  #geom_jitter(aes(x=3,y=MAE_20_20[,7]),width = 0.4,color = mycolor[3])+
  #geom_point(aes(x=3,y=c(mean(MAE_20_20[,2]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=4,y=MAE_20_80[,3]),fill = mycolor[4],outlier.shape = 16)+
  geom_segment(aes(x=3.8,xend = 4.2,y = 4.7,yend = 4.7))+
  geom_segment(aes(x=3.8,xend = 4.2,y = 2.3,yend = 2.3))+
  #geom_jitter(aes(x=4,y=MAE_20_20[,8]),width = 0.4,color = mycolor[4])+
  #geom_point(aes(x=4,y=c(mean(MAE_20_20[,3]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=5,y=MAE_20_80[,4]),fill = mycolor[5],outlier.shape = 16)+
  geom_segment(aes(x=4.8,xend = 5.2,y = 2.65,yend = 2.65))+
  geom_segment(aes(x=4.8,xend = 5.2,y = 1.52,yend = 1.52))+
  #geom_jitter(aes(x=5,y=MAE_20_20[,9]),width = 0.4,color = mycolor[5])+
  #geom_point(aes(x=5,y=c(mean(MAE_20_20[,4]),rep(-1,1999))),color = 'white',size = 1)+
  #geom_hline(yintercept = c(mean(MAE_20_20[,5])),linetype = 2)+
  theme(plot.margin = margin(1, 0.5, 0.5, 0.1, unit = "cm"),panel.background = element_blank(),axis.line = element_line(),text=element_text(size=16,family = 'serif')) +
  annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = Inf, xend = Inf, size = 0.5)+
  labs(y=NULL,x=NULL)+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)+
  scale_x_continuous(breaks = 3,labels = "S=20,D=80")+
  scale_y_continuous(limits = c(1,10))





three = ggplot(data = MAE_20_50)+
  geom_boxplot(aes(x=1,y=MAE_20_50[,5],fill = mycolor[1]),outlier.shape = 16)+
  geom_segment(aes(x=0.8,xend = 1.2,y = 6.1,yend = 6.1))+
  geom_segment(aes(x=0.8,xend = 1.2,y = 3.9,yend = 3.9))+
  #geom_jitter(aes(x=1,y=MAE_20_50[,10]),width = 0.4,color = mycolor[1])+
  #geom_point(aes(x=1,y=c(mean(MAE_20_50[,5]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=2,y=MAE_20_50[,1],fill = mycolor[2]),outlier.shape = 16)+
  geom_segment(aes(x=1.8,xend = 2.2,y = 2.6,yend = 2.6))+
  geom_segment(aes(x=1.8,xend = 2.2,y = 1.5,yend = 1.5))+
  #geom_jitter(aes(x=2,y=MAE_20_50[,6]),width = 0.4,color = mycolor[2])+
  #geom_point(aes(x=2,y=c(mean(MAE_20_50[,1]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=3,y=MAE_20_50[,2],fill = mycolor[3]),outlier.shape = 16)+
  geom_segment(aes(x=2.8,xend = 3.2,y = 3.47,yend = 3.47))+
  geom_segment(aes(x=2.8,xend = 3.2,y = 1.75,yend = 1.75))+
  #geom_jitter(aes(x=3,y=MAE_20_50[,7]),width = 0.4,color = mycolor[3])+
  #geom_point(aes(x=3,y=c(mean(MAE_20_50[,2]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=4,y=MAE_20_50[,3],fill = mycolor[4]),outlier.shape = 16)+
  geom_segment(aes(x=3.8,xend = 4.2,y = 3.8,yend = 3.8))+
  geom_segment(aes(x=3.8,xend = 4.2,y = 1.99,yend = 1.99))+
  #geom_jitter(aes(x=4,y=MAE_20_50[,8]),width = 0.4,color = mycolor[5])+
  #geom_point(aes(x=4,y=c(mean(MAE_20_50[,3]),rep(-1,1999))),color = 'white',size = 1)+
  geom_boxplot(aes(x=5,y=MAE_20_50[,4],fill = mycolor[5]),outlier.shape = 16)+
  geom_segment(aes(x=4.8,xend = 5.2,y = 2.57,yend = 2.57))+
  geom_segment(aes(x=4.8,xend = 5.2,y = 1.5,yend = 1.5))+
  #geom_jitter(aes(x=5,y=MAE_20_50[,9]),width = 0.4,color = mycolor[4])+
  #geom_point(aes(x=5,y=c(mean(MAE_20_50[,4]),rep(-1,1999))),color = 'white',size = 1)+
  #geom_hline(yintercept = c(mean(MAE_20_50[,5])),linetype = 2)+
  theme(plot.margin = margin(1, 0.5, 0.5, 0.1, unit = "cm"),panel.background = element_blank(),axis.line.x = element_line(),axis.line.y = element_line(),legend.key=element_rect(fill = "white"),legend.background = element_rect(color = rgb(0,0,0,50,maxColorValue = 255)),text=element_text(size=16,family = 'serif')) +
  labs(y=NULL,x=NULL)+
  scale_x_continuous(breaks = 3,labels = "S=20,D=50")+
  scale_y_continuous(limits = c(1,10))+
  annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = Inf, xend = Inf, size = 0.5)+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)+
  #scale_color_manual(name = "Data",values = c(mycolor),labels = c('MAE_norm','MAE_t','MAE_laplace','MAE_logistic','MAE_pearson'))+
  scale_fill_manual(name = "Data",values = c(mycolor),breaks = c(mycolor),labels = c('Gaussian',"Student's-t",'Laplace','Logistic','Pearson'))+
  guides(fill=guide_legend(nrow=2),color=guide_legend(nrow=2)) +
  theme(legend.title=element_blank(),legend.position = c(0.5, 0.85),legend.direction = 'horizontal',legend.margin = margin(0, 0, 0, 0, unit = "cm"),legend.key.width = unit(0.3,"cm"),legend.key.height = unit(0.5,"cm"),legend.box.margin = margin(0, 0, 0, 0, unit = "cm"),legend.key.size=unit(2,'cm'))


ggarrange(one,two,three,four,widths = c(2,2),ncol=2,nrow = 2)



#####################   画参数MAE部分  (文中没用) ################################
miu_t = data.frame(miu_t_5_20,miu_t_5_50,miu_t_10_20,miu_t_10_50,miu_t_20_20,miu_t_20_50)
miu_laplace = data.frame(miu_laplace_5_20,miu_laplace_5_50,miu_laplace_10_20,miu_laplace_10_50,miu_laplace_20_20,miu_laplace_20_50)
miu_logistic = data.frame(miu_logistic_5_20,miu_logistic_5_50,miu_logistic_10_20,miu_logistic_10_50,miu_logistic_20_20,miu_logistic_20_50)
miu_Pearson = data.frame(miu_Pearson_5_20,miu_Pearson_5_50,miu_Pearson_10_20,miu_Pearson_10_50,miu_Pearson_20_20,miu_Pearson_20_50)
miu_norm = data.frame(miu_norm_5_20,miu_norm_5_50,miu_norm_10_20,miu_norm_10_50,miu_norm_20_20,miu_norm_20_50)

sigma_t = data.frame(sigma_t_5_20,sigma_t_5_50,sigma_t_10_20,sigma_t_10_50,sigma_t_20_20,sigma_t_20_50)
sigma_laplace = data.frame(sigma_laplace_5_20,sigma_laplace_5_50,sigma_laplace_10_20,sigma_laplace_10_50,sigma_laplace_20_20,sigma_laplace_20_50)
sigma_logistic = data.frame(sigma_logistic_5_20,sigma_logistic_5_50,sigma_logistic_10_20,sigma_logistic_10_50,sigma_logistic_20_20,sigma_logistic_20_50)
sigma_Pearson = data.frame(sigma_Pearson_5_20,sigma_Pearson_5_50,sigma_Pearson_10_20,sigma_Pearson_10_50,sigma_Pearson_20_20,sigma_Pearson_20_50)
sigma_norm = data.frame(sigma_norm_5_20,sigma_norm_5_50,sigma_norm_10_20,sigma_norm_10_50,sigma_norm_20_20,sigma_norm_20_50)

m_t = data.frame(m_t_5_20,m_t_5_50,m_t_10_20,m_t_10_50,m_t_20_20,m_t_20_50)
m_laplace = data.frame(m_laplace_5_20,m_laplace_5_50,m_laplace_10_20,m_laplace_10_50,m_laplace_20_20,m_laplace_20_50)
m_logistic = data.frame(m_logistic_5_20,m_logistic_5_50,m_logistic_10_20,m_logistic_10_50,m_logistic_20_20,m_logistic_20_50)
m_Pearson = data.frame(m_Pearson_5_20,m_Pearson_5_50,m_Pearson_10_20,m_Pearson_10_50,m_Pearson_20_20,m_Pearson_20_50)
m_norm = data.frame(m_norm_5_20,m_norm_5_50,m_norm_10_20,m_norm_10_50,m_norm_20_20,m_norm_20_50)


par(mar=c(4,3,4,2),mgp=c(1.8,0.6,0))
plot(seq(1,4),abs(c(mean((miu_t[,3]-3)),mean((miu_t[,4]-3)),mean((miu_t[,5]-3)),mean((miu_t[,6]-3)))),type = 'l',ylab = "MAE of μ",ylim=c(0,0.03),xaxt="n",xlab=NA,col=mycolor[2],lwd=2)
axis(1,labels=c("z=10,ξ=20","z=10,ξ=50","z=20,ξ=20","z=20,ξ=50"),at=seq(1,4),las=1)
points(seq(1,4),abs(c(mean((miu_t[,3]-3)),mean((miu_t[,4]-3)),mean((miu_t[,5]-3)),mean((miu_t[,6]-3)))),cex=1.2,col=mycolor[2],pch=0,lwd=2)
lines(seq(1,4),abs(c(mean((miu_laplace[,3]-3)),mean((miu_laplace[,4]-3)),mean((miu_laplace[,5]-3)),mean((miu_laplace[,6]-3)))),col=mycolor[3],lwd=2)
points(seq(1,4),abs(c(mean((miu_laplace[,3]-3)),mean((miu_laplace[,4]-3)),mean((miu_laplace[,5]-3)),mean((miu_laplace[,6]-3)))),col=mycolor[3],cex=1.2,pch=2,lwd=2)
lines(seq(1,4),abs(c(mean((miu_logistic[,3]-3)),mean((miu_logistic[,4]-3)),mean((miu_logistic[,5]-3)),mean((miu_logistic[,6]-3)))),col=mycolor[4],lwd=2)
points(seq(1,4),abs(c(mean((miu_logistic[,3]-3)),mean((miu_logistic[,4]-3)),mean((miu_logistic[,5]-3)),mean((miu_logistic[,6]-3)))),col=mycolor[4],cex=1.2,pch=5,lwd=2)
lines(seq(1,4),abs(c(mean((miu_Pearson[,3]-3)),mean((miu_Pearson[,4]-3)),mean((miu_Pearson[,5]-3)),mean((miu_Pearson[,6]-3)))),col=mycolor[5],lwd=2)
points(seq(1,4),abs(c(mean((miu_Pearson[,3]-3)),mean((miu_Pearson[,4]-3)),mean((miu_Pearson[,5]-3)),mean((miu_Pearson[,6]-3)))),col=mycolor[5],cex=1.2,pch=6,lwd=2)
lines(seq(1,4),abs(c(mean((miu_norm[,3]-3)),mean((miu_norm[,4]-3)),mean((miu_norm[,5]-3)),mean((miu_norm[,6]-3)))),col=mycolor[1],lwd=2)
points(seq(1,4),abs(c(mean((miu_norm[,3]-3)),mean((miu_norm[,4]-3)),mean((miu_norm[,5]-3)),mean((miu_norm[,6]-3)))),col=mycolor[1],cex=1.2,pch=1,lwd=2)
legend(x=1,y=0.03,legend=c("Gaussian","Student-t","Laplace","Logistic","Pearson Type VII"),pch=c(1,0,2,5,6),lwd=2,col=mycolor,box.lwd = 2,box.col = "grey")



plot(seq(1,6),abs(c(mean(sigma_t[,1])-25,mean(sigma_t[,2])-25,mean(sigma_t[,3])-25,mean(sigma_t[,4])-25,mean(sigma_t[,5])-25,mean(sigma_t[,6])-25)),type = 'l',ylim=c(0,200))
lines(seq(1,6),abs(c(mean(sigma_laplace[,1])-25,mean(sigma_laplace[,2])-25,mean(sigma_laplace[,3])-25,mean(sigma_laplace[,4])-25,mean(sigma_laplace[,5])-25,mean(sigma_laplace[,6])-25)))
lines(seq(1,6),abs(c(mean(sigma_logistic[,1])-25,mean(sigma_logistic[,2])-25,mean(sigma_logistic[,3])-25,mean(sigma_logistic[,4])-25,mean(sigma_logistic[,5])-25,mean(sigma_logistic[,6])-25)))
lines(seq(1,6),abs(c(mean(sigma_Pearson[,1])-25,mean(sigma_Pearson[,2])-25,mean(sigma_Pearson[,3])-25,mean(sigma_Pearson[,4])-25,mean(sigma_Pearson[,5])-25,mean(sigma_Pearson[,6])-25)))
lines(seq(1,6),abs(c(mean(sigma_norm[,1])-25,mean(sigma_norm[,2])-25,mean(sigma_norm[,3])-25,mean(sigma_norm[,4])-25,mean(sigma_norm[,5])-25,mean(sigma_norm[,6])-25)),col = 'red')


plot(seq(1,4),abs(c(mean(sqrt(sigma_t[,3]))-5,mean(sqrt(sigma_t[,4]))-5,mean(sqrt(sigma_t[,5]))-5,mean(sqrt(sigma_t[,6]))-5)),type = 'l',ylim=c(0,15),ylab='MAE of σ',xaxt="n",xlab=NA,col=mycolor[2],lwd=2)
axis(1,labels=c("z=10,ξ=20","z=10,ξ=50","z=20,ξ=20","z=20,ξ=50"),at=seq(1,4),las=1)
points(seq(1,4),abs(c(mean(sqrt(sigma_t[,3]))-5,mean(sqrt(sigma_t[,4]))-5,mean(sqrt(sigma_t[,5]))-5,mean(sqrt(sigma_t[,6]))-5)),col=mycolor[2],lwd=2,cex=1.2,pch=0)
lines(seq(1,4),abs(c(mean(sqrt(sigma_laplace[,3]))-5,mean(sqrt(sigma_laplace[,4]))-5,mean(sqrt(sigma_laplace[,5]))-5,mean(sqrt(sigma_laplace[,6]))-5)),lwd=2,col = mycolor[3])
points(seq(1,4),abs(c(mean(sqrt(sigma_laplace[,3]))-5,mean(sqrt(sigma_laplace[,4]))-5,mean(sqrt(sigma_laplace[,5]))-5,mean(sqrt(sigma_laplace[,6]))-5)),cex=1.2,lwd=2,col = mycolor[3],pch=2)
lines(seq(1,4),abs(c(mean(sqrt(sigma_logistic[,3]))-5,mean(sqrt(sigma_logistic[,4]))-5,mean(sqrt(sigma_logistic[,5]))-5,mean(sqrt(sigma_logistic[,6]))-5)),col = mycolor[4],lwd = 2)
points(seq(1,4),abs(c(mean(sqrt(sigma_logistic[,3]))-5,mean(sqrt(sigma_logistic[,4]))-5,mean(sqrt(sigma_logistic[,5]))-5,mean(sqrt(sigma_logistic[,6]))-5)),col = mycolor[4],lwd=2,cex=1.2,pch=5)
lines(seq(1,4),abs(c(mean(sqrt(sigma_Pearson[,3]))-5,mean(sqrt(sigma_Pearson[,4]))-5,mean(sqrt(sigma_Pearson[,5]))-5,mean(sqrt(sigma_Pearson[,6]))-5)),lwd=2,col = mycolor[5])
points(seq(1,4),abs(c(mean(sqrt(sigma_Pearson[,3]))-5,mean(sqrt(sigma_Pearson[,4]))-5,mean(sqrt(sigma_Pearson[,5]))-5,mean(sqrt(sigma_Pearson[,6]))-5)),col = mycolor[5],cex=1.2,lwd=2,pch=6)
lines(seq(1,4),abs(c(mean(sqrt(sigma_norm[,3]))-5,mean(sqrt(sigma_norm[,4]))-5,mean(sqrt(sigma_norm[,5]))-5,mean(sqrt(sigma_norm[,6]))-5)),col = mycolor[1],lwd=2)
points(seq(1,4),abs(c(mean(sqrt(sigma_norm[,3]))-5,mean(sqrt(sigma_norm[,4]))-5,mean(sqrt(sigma_norm[,5]))-5,mean(sqrt(sigma_norm[,6]))-5)),col = mycolor[1],lwd=2,cex=1.2,pch=1)
legend(x=1,y=15,legend=c("Gaussian","Student-t","Laplace","Logistic","Pearson Type VII"),pch=c(1,0,2,5,6),lwd=2,col=mycolor,box.lwd = 2,box.col = "grey")



plot(seq(1,6),abs(c(mean(m_t[,1])-4,mean(m_t[,2])-4,mean(m_t[,3])-4,mean(m_t[,4])-4,mean(m_t[,5])-4,mean(m_t[,6])-4)),type = 'l',ylim=c(0,4))
lines(seq(1,6),abs(c(mean(m_laplace[,1])-4,mean(m_laplace[,2])-4,mean(m_laplace[,3])-4,mean(m_laplace[,4])-4,mean(m_laplace[,5])-4,mean(m_laplace[,6])-4)))
lines(seq(1,6),abs(c(mean(m_logistic[,1])-4,mean(m_logistic[,2])-4,mean(m_logistic[,3])-4,mean(m_logistic[,4])-4,mean(m_logistic[,5])-4,mean(m_logistic[,6])-4)))
lines(seq(1,6),abs(c(mean(m_Pearson[,1])-4,mean(m_Pearson[,2])-4,mean(m_Pearson[,3])-4,mean(m_Pearson[,4])-4,mean(m_Pearson[,5])-4,mean(m_Pearson[,6])-4)))
lines(seq(1,6),abs(c(mean(m_norm[,1])-4,mean(m_norm[,2])-4,mean(m_norm[,3])-4,mean(m_norm[,4])-4,mean(m_norm[,5])-4,mean(m_norm[,6])-4)),col = 'red')



##############################   分析v和delta  (文中没用) ##############################
v_t = data.frame(v_t_5_20,v_t_5_50,v_t_10_20,v_t_10_50,v_t_20_20,v_t_20_50)
v_Pearson = data.frame(v_Pearson_5_20,v_Pearson_5_50,v_Pearson_10_20,v_Pearson_10_50,v_Pearson_20_20,v_Pearson_20_50)
delta_Pearson = data.frame(delta_Pearson_5_20,delta_Pearson_5_50,delta_Pearson_10_20,delta_Pearson_10_50,delta_Pearson_20_20,delta_Pearson_20_50)

par(mar=c(4,3,4,2),mgp=c(1.8,0.6,0))
plot(seq(1,4),abs(c(mean(v_t[,3]),mean(v_t[,4]),mean(v_t[,5]),mean(v_t[,6]))),type = 'l',ylab = NA,ylim=c(0,25),xaxt="n",xlab=NA,col=mycolor[2],lwd=2)
axis(1,labels=c("z=10,ξ=20","z=10,ξ=50","z=20,ξ=20","z=20,ξ=50"),at=seq(1,4),las=1)
points(seq(1,4),abs(c(mean(v_t[,3]),mean(v_t[,4]),mean(v_t[,5]),mean(v_t[,6]))),cex=1.2,col=mycolor[2],pch=0,lwd=2)
lines(seq(1,4),abs(c(mean(v_Pearson[,3]),mean(v_Pearson[,4]),mean(v_Pearson[,5]),mean(v_Pearson[,6]))),lwd=2,col = mycolor[5])
points(seq(1,4),abs(c(mean(v_Pearson[,3]),mean(v_Pearson[,4]),mean(v_Pearson[,5]),mean(v_Pearson[,6]))),col = mycolor[5],cex=1.2,lwd=2,pch=6)
lines(seq(1,4),abs(c(mean(delta_Pearson[,3]),mean(delta_Pearson[,4]),mean(delta_Pearson[,5]),mean(delta_Pearson[,6]))),lwd=2,col = mycolor[5])
points(seq(1,4),abs(c(mean(delta_Pearson[,3]),mean(delta_Pearson[,4]),mean(delta_Pearson[,5]),mean(delta_Pearson[,6]))),col = mycolor[5],cex=1.2,lwd=2,pch=25,bg = mycolor[5])
legend(x=3,y=25,legend=c("v of Student-t","v of Pearson","δ of Pearson"),pch=c(0,6,25),pt.bg = mycolor[5],lwd=2,col=c(mycolor[2],mycolor[5],mycolor[5]),box.lwd = 2,box.col = "grey")




