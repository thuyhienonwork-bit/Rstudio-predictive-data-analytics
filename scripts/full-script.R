## ==============================================
# R Script: Employee Survey Analytics Pipeline
# ==============================================
# Script Purpose:
#   This script executes an end-to-end analytics workflow
#   on the Employee Survey dataset in RStudio.
#
#   It covers data cleaning, missing value handling,
#   imputation (KNN & MICE), outlier detection,
#   descriptive visualization, and statistical modeling
#   for Job Satisfaction and Overtime (HaveOT).
#
#   Run this script to generate a cleaned dataset,
#   analytical summaries, visualizations, and model outputs.
# ==============================================

install.packages("readxl")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("ggplot2")
install.packages("stringr")
install.packages ("MissMech")
install.packages ("naniar")

library(readxl)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(stringr)
library (MissMech)
library (naniar)

base_path <- "/Users/nguyenthuyhien/Downloads/IBA_A2_datasets/"
data <- read_excel(paste0(base_path, "Employee Survey.xlsx"))
View(data)

##Datachecking
str(data)

## Unify the Missing Values
placeholders <- c("NA", "na", "Na", "n/a", "N/A", "")
data <- data %>%
  mutate(across(everything(), ~ifelse(. %in% placeholders, NA, .)))

### Data Pre-processing

library(dplyr)
library(forcats)

likert_vars <- c("WorkLifeBalance", "WorkEnv", "Workload", "Stress", "JobSatisfaction")

data <- data %>% select(-EmpID)
data <- data %>% rename(HaveOT = haveOT)

data <- data %>%
  mutate(
   
    # Nominal categorical
    Gender        = as.factor(Gender),
    MaritalStatus = as.factor(MaritalStatus),
    Dept          = as.factor(Dept),
    EmpType       = as.factor(EmpType),
    CommuteMode   = as.factor(CommuteMode),
    haveOT        = as.factor(HaveOT),
    
    # Ordinal: JobLevel
    JobLevel = factor(
      JobLevel,
      levels = c("Intern/Fresher", "Junior", "Mid", "Senior", "Manager"),
      ordered = TRUE
    ),
    
    # Ordinal: EduLevel
    EduLevel = factor(
      EduLevel,
      levels = c("Bachelor", "Master", "PhD"),
      ordered = TRUE
    ),
    
    # Ordinal: các thang Likert 1–5
    across(
      .cols = all_of(likert_vars),
      .fns  = ~ factor(., levels = 1:5, ordered = TRUE)
    )
  )

str(data)

## Error Detection
# Check for duplicate records based on identification number
duplicate_count <- sum(duplicated(data))
cat("Number of duplicate records:", duplicate_count, "\n")

## Missing Value Detection
# Calculate missing values per column
missing_values <- colSums(is.na(data))
print(missing_values)

missing_percentage <- round(missing_values / nrow(data) * 100, 2)
print(missing_percentage)

# Create a summary dataframe
missing_summary <- data.frame(
  Missing_Count = missing_values,
  Missing_Percentage = missing_percentage
)

# Sort by missing percentage
missing_summary <- missing_summary[order(-missing_summary$Missing_Percentage), ]

# Display variables with missing values
cat("Variables with missing values:", "\n")
print(missing_summary[missing_summary$Missing_Count > 0, ])









# CHECK MCAR / MAR / (POSSIBLE) MNAR FOR data


# Cài gói nếu chưa có:
# install.packages("tidyverse")
# install.packages("naniar")
# install.packages("purrr")

library(tidyverse)
library(naniar)
library(purrr)

#----------------------------------------------------
# 0. Chuẩn hóa tên biến để tránh lỗi (bỏ khoảng trắng, ký tự lạ)
#    Sau dòng này, tất cả tên cột sẽ thành tên "hợp lệ" của R
#----------------------------------------------------
data <- data %>% 
  rename_with(make.names)


# STEP 1 — Biến nào có missing?


na_vars <- names(data)[colSums(is.na(data)) > 0]

cat("Variables with missing values:\n")
print(na_vars)

if (length(na_vars) == 0) {
  stop("Không có biến nào bị missing trong data.")
}

cat("\nMissing summary per variable:\n")
print(naniar::miss_var_summary(data))

# STEP 2 — Tạo biến chỉ báo missing cho từng biến có NA
data_miss <- data %>%
  mutate(
    across(
      all_of(na_vars),
      ~ ifelse(is.na(.), 1, 0),
      .names = "miss_{.col}"
    )
  )

# STEP 3 — Xác định predictor vars
predictor_vars <- names(data)[!names(data) %in% na_vars]


# STEP 4 — Logistic model: miss_Var ~ predictors

run_missing_logit <- function(var) {
  miss_var <- paste0("miss_", var)
  
  # nếu biến miss_ đó không tồn tại thì bỏ qua
  if (!(miss_var %in% names(data_miss))) return(NULL)
  
  # dùng reformulate để tránh lỗi tên biến
  form <- reformulate(termlabels = predictor_vars,
                      response   = miss_var)
  
  model <- tryCatch(
    glm(form, data = data_miss, family = binomial),
    error = function(e) {
      message("Lỗi glm cho biến ", var, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(model)) return(NULL)
  
  summary(model)
}

logit_results <- lapply(na_vars, run_missing_logit)
names(logit_results) <- na_vars


# STEP 5 — Phân loại gần MCAR vs MAR
detect_mechanism <- function(model_summary) {
  if (is.null(model_summary)) {
    return("Model error (cannot classify)")
  }
  
  coefs <- coef(model_summary)
  if (nrow(coefs) <= 1) {
    return("Model issue (only intercept)")
  }
  
  pvals <- coefs[-1, 4]  # bỏ intercept
  
  if (all(pvals > 0.05, na.rm = TRUE)) {
    return("≈ MCAR (no predictors significantly explain missingness)")
  } else {
    return("≈ MAR (missingness associated with observed variables)")
  }
}

mechanism_results <- map_chr(logit_results, detect_mechanism)

# STEP 6 — Tín hiệu MNAR (gián tiếp)
# So sánh Age & Experience giữa nhóm missing vs non-missing


check_mnar <- function(var) {
  miss_col <- paste0("miss_", var)
  
  if (!(miss_col %in% names(data_miss))) {
    return(c(p_age = NA, p_experience = NA))
  }
  if (!("Age" %in% names(data_miss)) | !("Experience" %in% names(data_miss))) {
    return(c(p_age = NA, p_experience = NA))
  }
  
  p_age <- tryCatch(
    t.test(data_miss$Age ~ data_miss[[miss_col]])$p.value,
    error = function(e) NA
  )
  
  p_exp <- tryCatch(
    t.test(data_miss$Experience ~ data_miss[[miss_col]])$p.value,
    error = function(e) NA
  )
  
  c(p_age = p_age, p_experience = p_exp)
}

mnar_signals <- lapply(na_vars, check_mnar)
names(mnar_signals) <- na_vars


# STEP 7 — Bảng tổng hợp kết quả


final_summary <- tibble(
  Variable         = na_vars,
  MissingMechanism = mechanism_results,
  MNAR_Signals     = mnar_signals
)

cat("\n===== FINAL SUMMARY (per-variable missingness mechanism) =====\n")
print(final_summary)


# END OF SCRIPT











########
# IMPUTATION PIPELINE (NO EmpID):
#  - KNN for nominal categorical
#  - MICE + PMM for numeric + ordinal + binary
########

library(tidyverse)
library(mice)
library(VIM)


# 1. Chuẩn hóa tên biến

data <- data %>% rename_with(make.names)


# 2. Tách nhóm biến


# Numeric
numeric_vars <- names(dplyr::select_if(data, is.numeric))

# Ordinal (ordered factor: Likert, JobLevel, EduLevel)
ordinal_vars <- names(Filter(is.ordered, data))

# Tất cả factor
factor_vars  <- names(Filter(is.factor, data))

# Nominal = factor nhưng không ordered
nominal_vars <- setdiff(factor_vars, ordinal_vars)

cat("\nNUMERIC VARS:\n");  print(numeric_vars)
cat("\nORDINAL VARS:\n");  print(ordinal_vars)
cat("\nNOMINAL VARS:\n");  print(nominal_vars)


# 3. KNN imputation cho NOMINAL


if (length(nominal_vars) > 0) {
  knn_out <- kNN(
    data[nominal_vars],
    k = 5,
    imp_var = FALSE
  )
} else {
  knn_out <- data[nominal_vars]
}

data_after_knn <- data
data_after_knn[nominal_vars] <- knn_out[nominal_vars]

cat("\nKNN imputation DONE for nominal variables.\n")


# 4. Chuẩn bị data cho MICE: numeric + ordinal + binary
#    (ordinal tạm convert thành numeric 1..K để dùng PMM)


data_mice <- data_after_knn %>%
  select(all_of(c(numeric_vars, ordinal_vars)))

# Lưu levels gốc của ordinal để convert lại sau
ordinal_levels <- lapply(data_mice[ordinal_vars], levels)

# Convert ordinal -> numeric 1..K
data_mice[ordinal_vars] <- lapply(data_mice[ordinal_vars], function(x) as.numeric(x))

# Xác định lại danh sách biến numeric sau khi convert
mice_vars <- names(data_mice)

# Method vector: tất cả dùng PMM
meth <- make.method(data_mice)
meth[mice_vars] <- "pmm"

# Predictor matrix: dùng tất cả biến khác làm predictor
pred <- make.predictorMatrix(data_mice)
pred[,] <- 1
diag(pred) <- 0


# 5. Chạy MICE


set.seed(123)
imp <- mice(
  data_mice,
  m = 5,
  method = meth,
  predictorMatrix = pred,
  maxit = 10
)

cat("\nMICE (PMM) imputation DONE.\n")

mice_completed <- complete(imp, 1)


# 6. Convert lại các biến ordinal thành ordered factor


for (v in ordinal_vars) {
  # lấy levels gốc
  levs <- ordinal_levels[[v]]
  # giá trị hiện tại là 1..K, map lại levels
  mice_completed[[v]] <- factor(
    levs[round(mice_completed[[v]])],
    levels = levs,
    ordered = TRUE
  )
}


# 7. Gộp tất cả lại thành final_data


final_data <- data_after_knn
final_data[numeric_vars] <- mice_completed[numeric_vars]
final_data[ordinal_vars] <- mice_completed[ordinal_vars]

cat("\nFINAL DATA READY (KNN for nominal + MICE PMM for others):\n")
str(final_data)

########
# END OF PIPELINE
########

view(final_data)

install.packages("writexl")
library(writexl)
write_xlsx(final_data,
           "/Users/nguyenthuyhien/Downloads/final_data_imputed.xlsx")

#######
# OUTLIER DETECTION USING IQR — APPLY TO ALL NUMERIC COLUMNS
#######

# data dùng ở đây là final_data
df <- final_data

# 1. Identify numeric columns
numeric_cols <- names(dplyr::select_if(df, is.numeric))
cat("Numeric variables:\n")
print(numeric_cols)

# 2. Function to detect outliers using IQR
detect_outlier_iqr <- function(x) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  # return TRUE if outlier
  return(x < lower | x > upper)
}

# 3. Apply to entire dataset
outlier_flags <- df %>%
  mutate(across(all_of(numeric_cols),
                ~ detect_outlier_iqr(.),
                .names = "outlier_{.col}"))

# 4. Summary table: count + percentage
outlier_summary <- tibble(
  Variable = numeric_cols,
  Outlier_Count = sapply(numeric_cols,
                         function(v) sum(outlier_flags[[paste0("outlier_", v)]], na.rm = TRUE)),
  Outlier_Percentage = sapply(numeric_cols,
                              function(v) {
                                sum(outlier_flags[[paste0("outlier_", v)]], na.rm = TRUE) / 
                                  nrow(df) * 100
                              })
)

cat("\n===== OUTLIER SUMMARY (IQR METHOD) =====\n")
print(outlier_summary)

# 5. (Optional) Long format list of actual outlier rows
outlier_long <- list()

for (v in numeric_cols) {
  flag_col <- paste0("outlier_", v)
  rows <- df[outlier_flags[[flag_col]] == TRUE, ]
  
  if (nrow(rows) > 0) {
    rows$Variable <- v
    rows$Outlier_Value <- rows[[v]]
    outlier_long[[v]] <- rows
  }
}

# Convert to one dataframe if needed:
outlier_details <- bind_rows(outlier_long, .id = "VarName")

#######
# DONE
#######








library(e1071)

# lấy toàn bộ numeric trong final_data
numeric_cols <- names(dplyr::select_if(final_data, is.numeric))

# tính skewness
skew_values <- sapply(final_data[numeric_cols], skewness)

# tạo bảng summary
skew_summary <- data.frame(
  Variable = numeric_cols,
  Skewness = skew_values,
  Recommendation = case_when(
    abs(skew_values) < 0.5 ~ "No transform needed (approximately symmetric)",
    abs(skew_values) < 1   ~ "Mild skew — optional transform",
    TRUE                   ~ "High skew — consider log/sqrt transform"
  )
)

print(skew_summary)


# Lấy các biến numeric
numeric_cols <- names(dplyr::select_if(final_data, is.numeric))

# Shapiro-Wilk test cho từng biến
normality_results <- lapply(numeric_cols, function(col) {
  test <- shapiro.test(final_data[[col]])
  data.frame(
    Variable = col,
    W = test$statistic,
    p_value = test$p.value
  )
})

normality_results <- do.call(rbind, normality_results)
print(normality_results)




par(mfrow = c(3, 3))  # layout 3x3, tùy số lượng biến

for (col in numeric_cols) {
  qqnorm(final_data[[col]], main = paste("Q-Q Plot:", col))
  qqline(final_data[[col]], col = "red")
}
par(mfrow = c(1,1))




par(mfrow = c(3, 3))

for (col in numeric_cols) {
  boxplot(final_data[[col]],
          main = paste("Boxplot:", col),
          col = "lightblue")
}

par(mfrow = c(1,1))






# 0. Load packages
library(dplyr)
library(e1071)

# 1. Select variables to transform using sqrt
        vars_to_transform <- c("Experience", "NumCompanies", "NumReports")
        
        # Check variables exist
        vars_to_transform <- vars_to_transform[vars_to_transform %in% names(final_data)]
        
        cat("Variables to transform using sqrt():\n")
        print(vars_to_transform)


# 2. Create transformed dataset
      final_data_transformed <- final_data
      
      for (v in vars_to_transform) {
        new_name <- paste0(v, "_sqrt")
        final_data_transformed[[new_name]] <- sqrt(final_data_transformed[[v]])
      }


# 3. Compute skewness before and after transformation
      # Skewness BEFORE
      skew_before <- sapply(final_data[vars_to_transform], skewness)
      
      # Skewness AFTER
      skew_after <- sapply(final_data_transformed[paste0(vars_to_transform, "_sqrt")], skewness)
      
      skew_compare <- data.frame(
        Variable = vars_to_transform,
        Skew_Before = skew_before,
        Skew_After = skew_after
      )
      
      cat("\n===== SKEWNESS COMPARISON (BEFORE vs AFTER) =====\n")
      print(skew_compare)

# 4. Boxplots: Before vs After
      par(mfrow = c(length(vars_to_transform), 2))
      
      for (v in vars_to_transform) {
        boxplot(final_data[[v]],
                main = paste("Before (Boxplot):", v),
                col = "lightblue")
        
        boxplot(final_data_transformed[[paste0(v, "_sqrt")]],
                main = paste("After sqrt (Boxplot):", v),
                col = "lightgreen")
      }
      
      par(mfrow = c(1,1))


# 5. Q-Q Plots Before vs After
par(mfrow = c(length(vars_to_transform), 2))

for (v in vars_to_transform) {
  ## Before
  qqnorm(final_data[[v]], main = paste("Before (Q-Q):", v))
  qqline(final_data[[v]], col = "red")
  
  ## After
  qqnorm(final_data_transformed[[paste0(v, "_sqrt")]],
         main = paste("After sqrt (Q-Q):", v))
  qqline(final_data_transformed[[paste0(v, "_sqrt")]], col = "red")
}

par(mfrow = c(1,1))

# 6. Output transformed dataset
cat("\nTransformation complete. New dataset: final_data_transformed\n")

        ### Correlation matrix
              library(dplyr)
              library(corrplot)
              
              df_num_ord <- final_data %>%
                mutate(across(where(is.ordered), as.numeric))
              
              num_vars <- df_num_ord[, sapply(df_num_ord, is.numeric), drop = FALSE]
              
              cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
              
              # Custom diverging palette 
              custom_col <- colorRampPalette(
                c("#D7191C", "#FDAE61", "#F7F7F7", "#ABD9E9", "#2C7BB6")
              )(200)
              
              # Create margin (bottom, left, top, right)
              par(mar = c(6, 4, 4, 2))  # tăng top để chứa title
              
              corrplot(
                cor_matrix,
                method = "color",
                type = "upper",
                addCoef.col = "black",
                tl.col = "black",
                tl.cex = 0.7,
                number.cex = 0.6,
                col = custom_col
              )




# ANOVA: Categorical variables predicting JobSatisfaction

# 0. Đảm bảo JobSatisfaction là numeric 1..K
# (nếu đã là numeric rồi thì dòng này không thay đổi gì)
final_data$JobSatisfaction_num <- as.numeric(final_data$JobSatisfaction)

# 1. Lấy danh sách biến categorical (factor + ordered), trừ JobSatisfaction
cat_vars <- names(final_data)[sapply(final_data, function(x) is.factor(x) | is.ordered(x))]
cat_vars <- setdiff(cat_vars, c("JobSatisfaction"))  # không dùng JobSatisfaction làm predictor

cat("Categorical predictors for ANOVA:\n")
print(cat_vars)

# 2. Chạy ANOVA cho từng biến, lưu kết quả vào list
anova_list <- lapply(cat_vars, function(v) {
  form <- as.formula(paste("JobSatisfaction_num ~", v))
  fit  <- aov(form, data = final_data)
  
  # Lấy dòng đầu tiên trong summary (hiệu ứng của biến v)
  s <- summary(fit)[[1]][1, ]
  
  data.frame(
    Variable = v,
    Df       = s[["Df"]],
    F_value  = s[["F value"]],
    p_value  = s[["Pr(>F)"]],
    row.names = NULL
  )
})

# 3. Ghép lại thành một bảng
anova_results <- do.call(rbind, anova_list)

# 4. Sắp xếp theo p-value
anova_results <- anova_results[order(anova_results$p_value), ]

cat("\n=========== ANOVA RESULTS: Categorical vs JobSatisfaction ===========\n")
print(anova_results)


str(final_data)



library(dplyr)

df <- final_data

df$JobSatisfaction <- as.factor(df$JobSatisfaction)
df$JobSatisfaction_num <- as.numeric(df$JobSatisfaction_num)

numeric_vars <- c(
  "Age","Experience","PhysicalActivityHours","SleepHours",
  "CommuteDistance","NumCompanies","TeamSize",
  "NumReports","TrainingHoursPerYear"
)

categorical_vars <- c(
  "Gender","MaritalStatus","Dept","EmpType","CommuteMode","HaveOT",
  "JobLevel","WorkLifeBalance","WorkEnv","Workload","Stress","EduLevel"
)

fmt3 <- function(x) sprintf("%.3f", x)
sig_flag <- function(p) ifelse(p < 0.05, "Yes", "No")

# ---------- ANOVA ----------
res_numeric <- lapply(numeric_vars, function(v) {
  fml <- as.formula(paste("JobSatisfaction_num ~", v))
  fit <- aov(fml, data = df)
  sm <- summary(fit)[[1]]
  
  data.frame(
    Variable = v,
    Test_Used = "ANOVA",
    Stat_Value = sm[1, "F value"],
    p_value = sm[1, "Pr(>F)"]
  )
}) |> bind_rows()

# ---------- Chi-square ----------
res_categorical <- lapply(categorical_vars, function(v) {
  tab <- table(df[[v]], df$JobSatisfaction)
  cs <- suppressWarnings(chisq.test(tab))
  
  data.frame(
    Variable = v,
    Test_Used = "Chi-square",
    Stat_Value = unname(cs$statistic),
    p_value = unname(cs$p.value)
  )
}) |> bind_rows()

final_table <- bind_rows(res_numeric, res_categorical) %>%
  mutate(
    `X² / F Value` = fmt3(Stat_Value),
    `p-value` = fmt3(p_value),
    Significant = sig_flag(p_value)
  ) %>%
  select(Variable, Test_Used, `X² / F Value`, `p-value`, Significant)

print(final_table)


### Descriptive Analytics

        # Packages
        library(dplyr)
        library(ggplot2)
        library(scales)
        
        df <- final_data %>%
          mutate(
            # Outcome
            JobSatisfaction = as.factor(JobSatisfaction),
            JobSatisfaction_num = as.numeric(JobSatisfaction_num),
            
            # Ensure key predictors are treated as factors (ordinal scales)
            WorkLifeBalance = as.factor(WorkLifeBalance),
            WorkEnv = as.factor(WorkEnv),
            Workload = as.factor(Workload),
            Stress = as.factor(Stress),
            
            EmpType = as.factor(EmpType),
            HaveOT = as.factor(HaveOT)
          )
        
        # Optional: consistent ordering for 1-5 scales
        order_1_5 <- function(x) factor(x, levels = c("1","2","3","4","5"), ordered = TRUE)
        
        df <- df %>%
          mutate(
            JobSatisfaction = order_1_5(JobSatisfaction),
            WorkLifeBalance = order_1_5(WorkLifeBalance),
            WorkEnv = order_1_5(WorkEnv),
            Workload = order_1_5(Workload),
            Stress = order_1_5(Stress)
          )
        #### Scenario 3: 
        p1 <- ggplot(df, aes(x = WorkLifeBalance, fill = JobSatisfaction)) +
          geom_bar(position = "fill") +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          labs(
            title = "Figure 3: Job Satisfaction distribution by Work-Life Balance",
            x = "Work-Life Balance (1=Low, 5=High)",
            y = "Percentage of employees",
            fill = "Job Satisfaction"
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(
              face = "bold",      # in đậm title
              hjust = 0.5         # căn giữa
            ),
            axis.title.x = element_text(
              margin = margin(t = 10)  # đẩy chữ trục X xuống
            ),
            axis.title.y = element_text(
              margin = margin(r = 10)  # đẩy chữ trục Y sang trái
            )
          )
        
        p1
        
        
        
        ### Scenario 4:
        p2_facet <- ggplot(
          df2,
          aes(
            x = Workload,
            y = JobSatisfaction_num,
            group = 1,
            color = Stress_group
          )
        ) +
          stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
          stat_summary(fun = mean, geom = "point", size = 2.6) +
          facet_wrap(~ Stress_group, nrow = 1) +
          scale_color_manual(
            values = c(
              "Low Stress" = "#2C7BB6",
              "Medium Stress" = "#FDAE61",
              "High Stress" = "#D7191C"
            )
          ) +
          scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
          labs(
            title = "Figure 4: Workload effects on Job_Satisfaction across Stress Level",
            x = "Workload (1 = Low, 5 = High)",
            y = "Mean Job Satisfaction"
          ) +
          theme_gray(base_size = 12) +
          theme(
            legend.position = "none",
            strip.background = element_rect(fill = "grey85", color = NA),
            strip.text = element_text(face = "bold"),
            plot.title = element_text(
              face = "bold",
              size = 14,                 
              hjust = 0.5,
              margin = margin(b = 12)    
            ),
            plot.subtitle = element_text(
              size = 10,
              hjust = 0.5
            ),
            axis.title.x = element_text(
              margin = margin(t = 10)
            ),
            axis.title.y = element_text(
              margin = margin(r = 10)
            ),
            panel.grid.minor = element_blank()
          )
        
        p2_facet
        
        
        
        
        
        ### Scenario 3:
        p3 <- ggplot(
          df,
          aes(
            x = WorkEnv,
            y = JobSatisfaction_num,
            fill = WorkEnv
          )
        ) +
          geom_boxplot(width = 0.6, outlier.alpha = 0.4) +
          scale_fill_manual(
            values = c(
              "1" = "#D7191C",
              "2" = "#FDAE61",
              "3" = "#FFFFBF",
              "4" = "#ABD9E9",
              "5" = "#2C7BB6"
            )
          ) +
          scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
          labs(
            title = "Figure 1: Job Satisfaction by Work Environment",
            x = "Work Environment (1 = Poor, 5 = Excellent)",
            y = "Job Satisfaction"
          ) +
          theme_gray(base_size = 12) +
          theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5),  # 
            axis.title = element_text(face = "bold"),
            panel.grid.minor = element_blank()
          )
        
        p3
        
        ### Scenario 2:
        p4 <- ggplot(
          df4,
          aes(x = HaveOT, y = JobSatisfaction_num, fill = EmpType)
        ) +
          geom_col(stat = "summary", fun = mean, width = 0.6) +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
          facet_wrap(~ EmpType, nrow = 1) +
          scale_fill_manual(
            values = c(
              "Contract"  = "#D7191C",
              "Part-Time" = "#FDAE61",
              "Full-Time" = "#2C7BB6"
            )
          ) +
          scale_y_continuous(breaks = 1:5) +
          coord_cartesian(ylim = c(1, 5)) +
          labs(
            title = "Figure 2: Mean Job Satisfaction by Overtime Status across Employment Types",
            x = "Overtime Status (FALSE / TRUE)",
            y = "Mean Job Satisfaction",
          ) +
          theme_gray(base_size = 12) +
          theme(
            legend.position = "none",
            strip.text = element_text(face = "bold"),
            plot.title = element_text(
              face = "bold",
              hjust = 0.5,
              margin = margin(b = 14)   
            ),
            axis.title.x = element_text(
              face = "bold",
              margin = margin(t = 12)   
            ),
            axis.title.y = element_text(
              face = "bold",
              margin = margin(r = 12)   
            ),
            panel.grid.minor = element_blank()
          )
        
        p4
        
        
        
    
### Predictive Analytics
                      
       #### Scenario 2: Job Satisfaction 
        ## ORDINAL LOGISTIC REGRESSION – polr()
        ## Explotary 
              install.packages("MASS")
              library(MASS)
              df <- final_data   # đổi tên nếu dataset của bạn khác
              # Ensure Job Satisfaction is ordinal
              df$JobSatisfaction <- ordered(
                df$JobSatisfaction,
                levels = c("1", "2", "3", "4", "5")
              )
              # Ensure categorical variables are factors
              df$HaveOT       <- factor(df$HaveOT)
              df$EmpType      <- factor(df$EmpType)
              df$Dept         <- factor(df$Dept)
              df$MaritalStatus<- factor(df$MaritalStatus)
              df$CommuteMode  <- factor(df$CommuteMode)
              df$Gender       <- factor(df$Gender)
              df$EduLevel     <- factor(df$EduLevel)
              full_model <- polr(
                JobSatisfaction ~
                  # Core job-related variables
                  WorkLifeBalance + WorkEnv + Workload + Stress +
                  HaveOT + EmpType +
                  
                  # Control variables
                  Age + Experience + JobLevel + Dept + MaritalStatus +
                  SleepHours + TrainingHoursPerYear + CommuteDistance +
                  TeamSize + NumReports + NumCompanies +
                  CommuteMode + Gender + EduLevel + PhysicalActivityHours,
                
                data = df,
                Hess = TRUE
              )
              
              summary(full_model)
        ### LRT test
              #Likelihood Ratio Tests (LRT) for each predictor
              lrt_table <- drop1(full_model, test = "Chisq")
              #View LRT results
              lrt_table
       
       ### Fit FINAL model using LRT-selected predictors
              final_model_lrt <- polr(
                JobSatisfaction ~
                  ## Core job-related predictors
                  WorkLifeBalance + WorkEnv + Workload + Stress +
                  HaveOT + 
                  ## Key control variables 
                  Age + MaritalStatus + SleepHours + NumCompanies + EduLevel,
                data = df,
                Hess = TRUE
              )
              summary(final_model_lrt)
        
              
      ### Confusion Matrix
              
              # Required packages
              library(MASS)
              library(ggplot2)
              library(dplyr)
              
              # 1. Ensure correct data types
                      df <- final_data
                      
                      df$JobSatisfaction <- ordered(
                        df$JobSatisfaction,
                        levels = c("1", "2", "3", "4", "5")
                      )
                      
                      df$HaveOT        <- factor(df$HaveOT)
                      df$MaritalStatus <- factor(df$MaritalStatus)
                      df$EduLevel      <- factor(df$EduLevel)

              # 2. Predict ordinal class
                        pred_class <- predict(
                          final_model_lrt,
                          newdata = df,
                          type = "class"
                        )
                        
                        pred_class <- ordered(pred_class, levels = levels(df$JobSatisfaction))

              # 3. Build confusion matrix
                        conf_mat <- table(
                          Actual    = df$JobSatisfaction,
                          Predicted = pred_class
                        )
                        
                        conf_df <- as.data.frame(conf_mat)
                        colnames(conf_df) <- c("Actual", "Predicted", "Count")
                        
              # Row-normalized proportions (by Actual)
                        conf_df <- conf_df %>%
                          group_by(Actual) %>%
                          mutate(Proportion = Count / sum(Count)) %>%
                          ungroup()
              
              # Plot confusion matrix (Row-normalized)
                        p_rates <- ggplot(conf_df, aes(
                          x = Predicted,
                          y = Actual,
                          fill = Proportion
                        )) +
                          geom_tile(color = "white", linewidth = 0.8) +
                          geom_text(aes(label = sprintf("%.2f", Proportion)),
                                    size = 5, fontface = "bold") +
                          scale_fill_gradient(low = "#deebf7", high = "#08306b") +
                          labs(
                            title = "Figure: Confusion Matrix",
                            x = "Predicted Job Satisfaction",
                            y = "Actual Job Satisfaction",
                            fill = "Proportion"
                          ) +
                          theme_classic(base_size = 15) +
                          theme(
                            plot.title = element_text(
                              size = 14,
                              face = "bold",
                              hjust = 0.5
                            ),
                            plot.subtitle = element_text(
                              size = 11,
                              hjust = 0.5
                            ),
                            axis.title.x = element_text(
                              size = 12,
                              face = "bold",
                              margin = margin(t = 20)
                            ),
                            axis.title.y = element_text(
                              size = 12,
                              face = "bold",
                              margin = margin(r = 20) 
                            )
                          )
                        
                        print(p_rates)
                        
                        
                        

print(p_rates)

              
       
      #####Scenario Predicted Possibilities
            ##### Scenario Predicted Possibilities (WE meet = 3; J0–J5)
            
            library(dplyr)
            
            # -----------------------------
            # Helpers
            # -----------------------------
            mode_level <- function(x) names(sort(table(x), decreasing = TRUE))[1]
            
            # Convert a fixed numeric level (e.g., 3) into the same ordered-factor structure as df[[var]]
            ord_val <- function(var, value, df) {
              ordered(as.character(value), levels = levels(df[[var]]))
            }
            
            # Force predicted probs into a 1-row matrix with consistent, ordered columns
            to_prob_row <- function(prob_obj, levs) {
              if (is.null(dim(prob_obj))) {
                m <- matrix(prob_obj, nrow = 1)
                colnames(m) <- names(prob_obj)
              } else {
                m <- as.matrix(prob_obj)
              }
              m <- m[, levs, drop = FALSE]
              return(m)
            }
            
            # -----------------------------
            # Baseline controls: keep constant to isolate policy effects
            # -----------------------------
            # IMPORTANT: df is your modelling dataset used to fit final_model
            # final_model is your fitted ordinal logistic model (MASS::polr), already built earlier
            
            base_age      <- median(df$Age, na.rm = TRUE)
            base_marital  <- mode_level(df$MaritalStatus)
            base_edulevel <- mode_level(df$EduLevel)
            base_numcomp  <- median(df$NumCompanies, na.rm = TRUE)
            
            # -----------------------------
            # Scenario anchor values (WE meet = 3 baseline as requested)
            # -----------------------------
            # J0 baseline:
            # Work_Environment = 3
            # Work_Life_Balance = 3
            # Workload = 3
            # Stress = 3
            # HaveOT = 1 (TRUE)
            # SleepHours = 7
            
            scenario_J0 <- data.frame(
              WorkLifeBalance = ord_val("WorkLifeBalance", 3, df),
              WorkEnv         = ord_val("WorkEnv", 3, df),
              Workload        = ord_val("Workload", 3, df),
              Stress          = ord_val("Stress", 3, df),
              HaveOT          = factor("TRUE", levels = levels(df$HaveOT)),
              Age             = base_age,
              MaritalStatus   = factor(base_marital, levels = levels(df$MaritalStatus)),
              SleepHours      = 7,
              NumCompanies    = base_numcomp,
              EduLevel        = factor(base_edulevel, levels = levels(df$EduLevel))
            )
            
            # J1 Reduce OT: HaveOT 1 -> 0
            scenario_J1 <- scenario_J0 %>%
              mutate(HaveOT = factor("FALSE", levels = levels(df$HaveOT)))
            
            # J2 Reduce Workload: 3 -> 2
            scenario_J2 <- scenario_J0 %>%
              mutate(Workload = ord_val("Workload", 2, df))
            
            # J3 Reduce Stress: 3 -> 2
            scenario_J3 <- scenario_J0 %>%
              mutate(Stress = ord_val("Stress", 2, df))
            
            # J4 Increase SleepHours: 7 -> 8
            scenario_J4 <- scenario_J0 %>%
              mutate(SleepHours = 8)
            
            # J5 Increase WLB: 3 -> 4
            scenario_J5 <- scenario_J0 %>%
              mutate(WorkLifeBalance = ord_val("WorkLifeBalance", 4, df))
            
            # -----------------------------
            # Predict probabilities for each class
            # -----------------------------
            levs <- levels(df$JobSatisfaction)  # expected: c("1","2","3","4","5")
            
            pJ0 <- to_prob_row(predict(final_model, newdata = scenario_J0, type = "probs"), levs)
            pJ1 <- to_prob_row(predict(final_model, newdata = scenario_J1, type = "probs"), levs)
            pJ2 <- to_prob_row(predict(final_model, newdata = scenario_J2, type = "probs"), levs)
            pJ3 <- to_prob_row(predict(final_model, newdata = scenario_J3, type = "probs"), levs)
            pJ4 <- to_prob_row(predict(final_model, newdata = scenario_J4, type = "probs"), levs)
            pJ5 <- to_prob_row(predict(final_model, newdata = scenario_J5, type = "probs"), levs)
            
            # -----------------------------
            # Assemble results table
            # -----------------------------
            scenario_results <- data.frame(
              Scenario = c(
                "J0: Baseline (WE=3, WLB=3, WL=3, Stress=3, OT=TRUE, Sleep=7)",
                "J1: Reduce OT (OT=FALSE)",
                "J2: Reduce Workload (WL=2)",
                "J3: Reduce Stress (Stress=2)",
                "J4: Increase SleepHours (Sleep=8)",
                "J5: Increase WLB (WLB=4)"
              ),
              P1 = c(pJ0[1,"1"], pJ1[1,"1"], pJ2[1,"1"], pJ3[1,"1"], pJ4[1,"1"], pJ5[1,"1"]),
              P2 = c(pJ0[1,"2"], pJ1[1,"2"], pJ2[1,"2"], pJ3[1,"2"], pJ4[1,"2"], pJ5[1,"2"]),
              P3 = c(pJ0[1,"3"], pJ1[1,"3"], pJ2[1,"3"], pJ3[1,"3"], pJ4[1,"3"], pJ5[1,"3"]),
              P4 = c(pJ0[1,"4"], pJ1[1,"4"], pJ2[1,"4"], pJ3[1,"4"], pJ4[1,"4"], pJ5[1,"4"]),
              P5 = c(pJ0[1,"5"], pJ1[1,"5"], pJ2[1,"5"], pJ3[1,"5"], pJ4[1,"5"], pJ5[1,"5"])
            ) %>%
              mutate(
                P4_or_5 = P4 + P5
              ) %>%
              mutate(across(where(is.numeric), ~ round(.x, 4)))
            
            scenario_results

              
                      ##### Visualize Scenario Predicted Possibilities (WE = 3; JS 1–5)
                      
                      library(dplyr)
                      library(tidyr)
                      library(ggplot2)
                      library(scales)
                      
                      # Force dplyr verbs (avoid MASS masking)
                      select  <- dplyr::select
                      filter  <- dplyr::filter
                      mutate  <- dplyr::mutate
                      
                      # Shorten scenario names + prepare KPI
                      kpi_df <- scenario_results %>%
                        mutate(
                          Scenario_short = case_when(
                            Scenario == "Baseline (WE = 3)"              ~ "Baseline",
                            Scenario == "WE +1 (4)"                       ~ "WE +1",
                            Scenario == "WL -1"                           ~ "WL -1",
                            Scenario == "Stress -1"                       ~ "Stress -1",
                            Scenario == "Sleep +1"                        ~ "Sleep +1",
                            TRUE                                         ~ Scenario
                          ),
                          Scenario_short = factor(Scenario_short, levels = Scenario_short),
                          P4_or_5 = as.numeric(P4_or_5)
                        )
                      
                      ggplot(kpi_df, aes(x = Scenario_short, y = P4_or_5)) +
                        geom_col(
                          width = 0.75,
                          fill  = "#2171b5",      # main blue
                          color = "#08306b"       # darker blue border
                        ) +
                        scale_y_continuous(
                          labels = percent_format(accuracy = 1),
                          limits = c(0, 1)
                        ) +
                        labs(
                          title    = "Figure: Probability of High Job Satisfaction (JS = 4–5)",
                          x = NULL,
                          y = "P(JS ≥ 4)"
                        ) +
                        theme_minimal(base_size = 13) +
                        theme(
                          axis.text.x  = element_text(angle = 20, hjust = 1),
                          plot.title = element_text(face = "bold", color = "#08306b", hjust = 0.5),
                          plot.subtitle= element_text(color = "#2171b5"),
                          axis.title.y = element_text(face = "bold")
                        )
            
                            

              ### Job_Satisfaction interaction
                        core_vars <- c("WorkLifeBalance", "WorkEnv", "Workload", "Stress", "HaveOT", "SleepHours")
                        interaction_pairs <- combn(core_vars, 2, simplify = FALSE)
                        test_one_interaction <- function(pair) {
                          int_term <- paste(pair, collapse = ":")
                          fml <- as.formula(
                            paste(
                              "JobSatisfaction ~",
                              paste(
                                c(
                                  "WorkLifeBalance", "WorkEnv", "Workload", "Stress", "HaveOT",
                                  "Age", "MaritalStatus", "SleepHours", "NumCompanies", "EduLevel",
                                  int_term
                                ),
                                collapse = " + "
                              )
                            )
                          )
                          
                          out <- tryCatch({
                            
                            model_int <- polr(fml, data = df, Hess = TRUE)
                            
                            lrt <- anova(final_model, model_int, test = "Chisq")
                            if (nrow(lrt) < 2) {
                              return(data.frame(Interaction = int_term, Chisq = NA, df = NA, p_value = NA,
                                                AIC_base = AIC(final_model), AIC_int = AIC(model_int),
                                                Delta_AIC = NA, Status = "LRT_failed"))
                            }
                            
                            data.frame(
                              Interaction = int_term,
                              Chisq = lrt$Chisq[2],
                              df = lrt$Df[2],
                              p_value = lrt$`Pr(>Chisq)`[2],
                              AIC_base = AIC(final_model),
                              AIC_int  = AIC(model_int),
                              Delta_AIC = AIC(model_int) - AIC(final_model),
                              Status = "OK"
                            )
                            
                          }, error = function(e) {
                            data.frame(Interaction = int_term, Chisq = NA, df = NA, p_value = NA,
                                       AIC_base = AIC(final_model), AIC_int = NA, Delta_AIC = NA,
                                       Status = paste0("Fit_error: ", substr(e$message, 1, 60)))
                          })
                          
                          out
                        }
                        
                        interaction_results <- do.call(rbind, lapply(interaction_pairs, test_one_interaction))
                      
                        keep_interactions <- interaction_results %>%
                          filter(Status == "OK") %>%
                          filter(!is.na(p_value)) %>%
                          mutate(Keep = ifelse(p_value <= 0.05 & Delta_AIC < 0, "YES", "NO")) %>%
                          arrange(p_value)
                        
                        interaction_results
                        keep_interactions
              
              
              
              # Binary Logistic Regression for HaveOT
              # Dataset structure: tibble with HaveOT as Factor ("FALSE","TRUE")
              # Packages
              library(dplyr)
              library(broom)
              library(car)      # optional: VIF
              library(pROC)     # optional: ROC/AUC
              
              # Assume your data is named df
              # df <- your_tibble
              
              # 1) Basic preparation
              df2 <- df %>%
                mutate(
                  # Ensure outcome is a 2-level factor with desired reference
                  HaveOT = factor(HaveOT, levels = c("FALSE", "TRUE")),
                  
                  # Ensure categorical predictors are factors
                  Gender        = factor(Gender),
                  MaritalStatus = factor(MaritalStatus),
                  Dept          = factor(Dept),
                  EmpType       = factor(EmpType),
                  CommuteMode   = factor(CommuteMode),
                  
                  # Ensure ordinal variables are ordered factors (as per your structure)
                  JobLevel        = as.ordered(JobLevel),
                  WorkLifeBalance = as.ordered(WorkLifeBalance),
                  WorkEnv         = as.ordered(WorkEnv),
                  Workload        = as.ordered(Workload),
                  Stress          = as.ordered(Stress),
                  EduLevel        = as.ordered(EduLevel),
                  JobSatisfaction = as.ordered(JobSatisfaction)
                ) %>%
                # For a clean run: keep complete cases for variables used in the model
                # (If you already imputed, you can remove this line)
                tidyr::drop_na(
                  HaveOT, Gender, Age, MaritalStatus, JobLevel, Experience, Dept, EmpType,
                  WorkLifeBalance, WorkEnv, PhysicalActivityHours, Workload, Stress,
                  SleepHours, CommuteMode, CommuteDistance, NumCompanies, TeamSize,
                  NumReports, EduLevel, TrainingHoursPerYear, JobSatisfaction_num
                )
              
              # 2) Fit a full logistic regression model 
              m_full <- glm(
                HaveOT ~ Gender + Age + MaritalStatus + JobLevel + Experience + Dept + EmpType +
                  WorkLifeBalance + WorkEnv + PhysicalActivityHours + Workload + Stress +
                  SleepHours + CommuteMode + CommuteDistance + NumCompanies + TeamSize +
                  NumReports + EduLevel + TrainingHoursPerYear + JobSatisfaction_num,
                data   = df2,
                family = binomial(link = "logit")
              )
              
              # 3) Model summary (log-odds scale)
              summary(m_full)
              
              # 4) Tidy output + Odds Ratios (manager-friendly)
              coef_table <- broom::tidy(m_full, conf.int = TRUE) %>%
                mutate(
                  OR      = exp(estimate),
                  OR_low  = exp(conf.low),
                  OR_high = exp(conf.high)
                )
              
              coef_table
              
              
              # Likelihood Ratio Test for variable selection
              drop1(m_full, test = "LRT")
              
              



              ## 0) Packages
              library(dplyr)
              library(broom)
             
         ## 1) Chuẩn hoá dữ liệu
              final_data <- final_data %>%
                mutate(
                  HaveOT = as.factor(HaveOT)
                )
              
              # set reference cho HaveOT
              if (all(c("FALSE", "TRUE") %in% levels(final_data$HaveOT))) {
                final_data <- final_data %>%
                  mutate(HaveOT = relevel(HaveOT, ref = "FALSE"))
              }

              ## 2) Danh sách independent variables
              ivars <- setdiff(names(final_data), "HaveOT")
              
              ## 3) Fit FULL binary logistic regression
              full_formula <- as.formula(
                paste("HaveOT ~", paste(ivars, collapse = " + "))
              )
              
              full_logit <- glm(
                full_formula,
                data = final_data,
                family = binomial()
              )
              

              ## 4) Model summary (log-odds)
              summary(full_logit)

              ## 5) Odds Ratios + 95% CI (từng level)

              OR_table <- broom::tidy(
                full_logit,
                conf.int = TRUE,
                exponentiate = TRUE
              ) %>%
                filter(term != "(Intercept)") %>%
                select(term, estimate, conf.low, conf.high, p.value) %>%
                rename(
                  OR = estimate,
                  CI_low = conf.low,
                  CI_high = conf.high
                ) %>%
                arrange(p.value)
              
              print(OR_table, n = Inf)

              ## 6) Likelihood Ratio Test

              LRT_table <- drop1(
                full_logit,
                test = "Chisq"
              ) %>%
                as.data.frame() %>%
                tibble::rownames_to_column("Variable") %>%
                filter(Variable != "<none>") %>%
                arrange(`Pr(>Chi)`)
              
              print(LRT_table)
              
              ## =========================
              ## 7) Gộp thành bảng SUMMARY THEO BIẾN
              ## =========================
              final_variable_logit_summary <- LRT_table %>%
                select(
                  Variable,
                  Df,
                  LRT = LRT,
                  p_value = `Pr(>Chi)`
                ) %>%
                mutate(
                  sig = case_when(
                    p_value < 0.001 ~ "***",
                    p_value < 0.01  ~ "**",
                    p_value < 0.05  ~ "*",
                    TRUE ~ ""
                  )
                )
              
              options(na.print = "NA")
              View(final_variable_logit_summary)


              # Final Binary Logistic Regression Model
                                # HaveOT ~ Dept + JobSatisfaction_num + NumReports
                  
                                library(dplyr)
                                library(broom)
                                
                                # Ensure correct variable types
                                df_final <- final_data %>%
                                  mutate(
                                    HaveOT = factor(HaveOT, levels = c("FALSE", "TRUE")),
                                    Dept   = factor(Dept)
                                  )
                                
                                # Fit the final model
                                m_final <- glm(
                                  HaveOT ~ Dept + JobSatisfaction_num + NumReports,
                                  data   = df_final,
                                  family = binomial(link = "logit")
                                )
                                
                                # Model summary (log-odds scale)
                                summary(m_final)
                                
                                # =========================
                                # Odds Ratios with 95% CI
                                
                                coef_table_final <- tidy(m_final, conf.int = TRUE) %>%
                                  mutate(
                                    OR      = exp(estimate),
                                    OR_low  = exp(conf.low),
                                    OR_high = exp(conf.high)
                                  )
                                
                                library(dplyr)
                                
                                # Have_OT Iteraction checking
                                # 1. Prepare data
                                df_final <- final_data %>%
                                  mutate(
                                    HaveOT = factor(HaveOT, levels = c("FALSE", "TRUE")),
                                    Dept   = factor(Dept)
                                  )

                                # 2. Baseline model 
                                m_base <- glm(
                                  HaveOT ~ Dept + JobSatisfaction_num + NumReports,
                                  data   = df_final,
                                  family = binomial
                                )
                                
                                # 3. All pairwise interactions
                                interaction_terms <- list(
                                  "Dept:JobSatisfaction_num",
                                  "Dept:NumReports",
                                  "JobSatisfaction_num:NumReports"
                                )
                                
                                interaction_models <- lapply(interaction_terms, function(term) {
                                  glm(
                                    as.formula(
                                      paste("HaveOT ~ Dept + JobSatisfaction_num + NumReports +", term)
                                    ),
                                    data   = df_final,
                                    family = binomial
                                  )
                                })
                                
                                names(interaction_models) <- interaction_terms
                                -
                                # 4. Likelihood Ratio Tests
                                lrt_summary <- data.frame(
                                  Interaction = interaction_terms,
                                  Chisq = sapply(interaction_models, function(m) {
                                    anova(m_base, m, test = "Chisq")$Deviance[2]
                                  }),
                                  df = sapply(interaction_models, function(m) {
                                    anova(m_base, m, test = "Chisq")$Df[2]
                                  }),
                                  p_value = sapply(interaction_models, function(m) {
                                    anova(m_base, m, test = "Chisq")$`Pr(>Chi)`[2]
                                  })
                                )
                                
                                lrt_summary
                                
                                
              
              ## Confusion matrix
              # Confusion Matrix VISUALISATION (ggplot2)
              # Model: HaveOT ~ Dept + JobSatisfaction_num + NumReports
              
              library(dplyr)
              library(broom)
              library(tidyr)
              library(ggplot2)

              # 0) INPUT DATA
              dat <- final_data

              # 1) Enforce types + drop NA for required vars
              dat <- dat %>%
                mutate(
                  HaveOT = factor(HaveOT, levels = c("FALSE", "TRUE")),
                  Dept = factor(Dept),
                  JobSatisfaction_num = as.numeric(JobSatisfaction_num),
                  NumReports = as.numeric(NumReports)
                ) %>%
                drop_na(HaveOT, Dept, JobSatisfaction_num, NumReports)
              

              # 2) Fit model
              m_final <- glm(
                HaveOT ~ Dept + JobSatisfaction_num + NumReports,
                data = dat,
                family = binomial(link = "logit")
              )

              # 3) Predict probabilities + classify at threshold
              threshold <- 0.5
              
              dat <- dat %>%
                mutate(
                  pred_prob = predict(m_final, type = "response"),
                  pred_class = factor(ifelse(pred_prob >= threshold, "TRUE", "FALSE"),
                                      levels = c("FALSE", "TRUE"))
                )
              

              # 4) Confusion matrix table (same counts as your print)
              cm <- table(Predicted = dat$pred_class, Actual = dat$HaveOT)
              
              # Turn into a tidy data frame for ggplot
              cm_df <- as.data.frame(cm) %>%
                rename(Count = Freq) %>%
                mutate(
                  # Labels to match your plot: columns = Predicted (No OT / OT), rows = Actual (TRUE/FALSE)
                  PredictedLabel = factor(Predicted, levels = c("FALSE", "TRUE"),
                                          labels = c("No OT", "OT")),
                  ActualLabel = factor(Actual, levels = c("TRUE", "FALSE"))  # TRUE on top like your image
                )
              

              # 5) Plot confusion matrix (heatmap + numbers)
              ggplot(cm_df, aes(x = PredictedLabel, y = ActualLabel, fill = Count)) +
                geom_tile(color = "white", linewidth = 0.6) +
                geom_text(aes(label = Count), size = 6, fontface = "bold") +
                scale_fill_gradient(
                  low  = "#deebf7",
                  high = "#08306b"
                ) +
                labs(
                  title = "Confusion Matrix for Overtime Prediction",
                  x = "Predicted",
                  y = "Actual",
                  fill = "Count"
                ) +
                theme_minimal(base_size = 13) +
                theme(
                  plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                  
                  # Axis titles
                  axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 6)),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 6)),
                  
                  # Axis tick labels (FIXED HERE)
                  axis.text.x  = element_text(size = 13, face = "bold"),
                  axis.text.y  = element_text(size = 13, face = "bold"),
                  
                  # Legend
                  legend.title = element_text(size = 12, face = "bold"),
                  legend.text  = element_text(size = 11),
                  
                  panel.grid = element_blank()
                )
              
              
              # (Optional) Print metrics as well
              TN <- cm["FALSE", "FALSE"]
              FP <- cm["TRUE",  "FALSE"]
              FN <- cm["FALSE", "TRUE"]
              TP <- cm["TRUE",  "TRUE"]
              
              accuracy  <- (TP + TN) / sum(cm)
              precision <- TP / (TP + FP)
              recall    <- TP / (TP + FN)
              specificity <- TN / (TN + FP)
              f1 <- 2 * precision * recall / (precision + recall)
              
              cat("\n=== Confusion Matrix (Threshold = 0.50) ===\n")
              print(cm)
              cat(sprintf("\nAccuracy: %.4f\nPrecision: %.4f\nRecall (Sensitivity): %.4f\nSpecificity: %.4f\nF1: %.4f\n",
                          accuracy, precision, recall, specificity, f1))

              
              ### Chart for Have_OT
              final_data$predicted_prob_OT <- predict(
                model_reduced,
                type = "response"
              )
              
              summary(final_data$predicted_prob_OT)
              library(ggplot2)
              
              ggplot(final_data, aes(
                x = JobSatisfaction_num,
                y = predicted_prob_OT
              )) +
                geom_jitter(alpha = 0.2, height = 0) +
                geom_smooth(method = "loess", se = TRUE) +
                labs(
                  title = "Predicted Probability of Overtime by Job Satisfaction",
                  x = "Job Satisfaction (numeric)",
                  y = "Predicted Probability of Overtime"
                ) +
                theme_classic()
              ggplot(final_data, aes(
                x = JobSatisfaction_num,
                y = predicted_prob_OT,
                color = Dept
              )) +
                geom_smooth(se = FALSE) +
                labs(
                  title = "Predicted Probability of Overtime by Department",
                  x = "Job Satisfaction",
                  y = "Predicted Probability of Overtime"
                ) +
                theme_classic()
              
