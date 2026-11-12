# Cline Rules for Base R Package Development

## 1. 项目定位

本项目是一个 R 包开发项目。开发目标是构建结构清晰、稳定可靠、易维护、符合 CRAN 要求的 R 包。

开发过程中应优先使用 **base R** 完成功能实现，除非确有必要，不应引入额外依赖包。

本规则适用于：

- R 函数开发
- S3 泛型和方法开发
- 数据清洗与转换函数开发
- 单元测试
- roxygen2 文档
- 示例代码
- CRAN 检查问题修复
- 包结构优化

---

## 2. 总体开发原则

### 2.1 优先使用 base R

所有函数实现应优先使用 base R，包括但不限于：

- `lapply()`
- `sapply()`
- `vapply()`
- `Map()`
- `Reduce()`
- `split()`
- `merge()`
- `aggregate()`
- `by()`
- `tapply()`
- `match()`
- `%in%`
- `order()`
- `sort()`
- `unique()`
- `duplicated()`
- `is.na()`
- `complete.cases()`
- `subset()`
- `transform()`
- `within()`
- `with()`
- `do.call()`
- `as.data.frame()`
- `data.frame()`
- `read.csv()`
- `write.csv()`

除非用户明确要求，否则不要使用：

- `dplyr`
- `tidyr`
- `purrr`
- `stringr`
- `readr`
- `data.table`
- `tibble`
- `janitor`
- `lubridate`

如果确实需要外部依赖，必须说明原因，并优先选择轻量、稳定、CRAN 上长期维护的包。

---

## 3. R 包结构规范

项目应遵循标准 R 包结构：

```text
pkg/
├── DESCRIPTION
├── NAMESPACE
├── R/
├── man/
├── tests/
│   └── testthat/
├── inst/
├── data/
├── data-raw/
├── vignettes/
├── README.md
└── .Rbuildignore
```

### 3.1 R 代码文件

所有正式函数应放在 `R/` 目录下。

建议按功能拆分文件，例如：

```text
R/
├── utils.R
├── validate.R
├── clean.R
├── classify.R
├── count.R
├── rate.R
├── plot.R
├── s3-generics.R
└── s3-methods.R
```

不要把所有函数都写在一个超长文件中。

---

## 4. 函数开发规范

### 4.1 函数命名

函数名应使用小写字母和下划线，例如：

```r
clean_registry()
classify_icd10()
count_cases()
create_rate()
validate_code()
```

内部函数可以使用点号或下划线，但推荐统一使用下划线：

```r
check_required_cols()
standardize_names()
internal_match_code()
```

不推荐使用含义不清的名称：

```r
fun1()
tmp_fun()
doit()
abc()
```

---

### 4.2 函数参数设计

函数参数应清晰、稳定、易理解。

推荐：

```r
clean_data <- function(x,
                       vars = NULL,
                       na_drop = TRUE,
                       verbose = FALSE) {
  ...
}
```

避免：

```r
clean_data <- function(data, a, b, c, flag) {
  ...
}
```

参数命名应表达含义：

- `x`：主要输入对象
- `data`：数据框
- `vars`：变量名
- `by`：分组变量
- `na_drop`：是否移除缺失值
- `verbose`：是否输出过程信息
- `...`：传递给方法的额外参数

---

### 4.3 输入检查

每个导出函数都应进行必要的输入检查。

示例：

```r
check_data_frame <- function(x, arg = "x") {
  if (!is.data.frame(x)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }
  invisible(TRUE)
}
```

检查字符参数：

```r
check_character <- function(x, arg = "x", allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }

  if (!is.character(x)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }

  invisible(TRUE)
}
```

检查逻辑参数：

```r
check_flag <- function(x, arg = "x") {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}
```

---

## 5. 错误处理规范

错误信息应清晰、具体、可定位。

推荐：

```r
if (!"age" %in% names(x)) {
  stop("Column `age` is required but was not found.", call. = FALSE)
}
```

不推荐：

```r
stop("Error")
stop("Wrong input")
stop("Invalid")
```

错误信息应说明：

1. 哪个参数有问题；
2. 期望是什么；
3. 实际问题是什么；
4. 用户该如何修复。

示例：

```r
if (!all(vars %in% names(x))) {
  missing_vars <- vars[!vars %in% names(x)]

  stop(
    "The following columns are missing from `x`: ",
    paste(missing_vars, collapse = ", "),
    call. = FALSE
  )
}
```

---

## 6. base R 数据处理规范

### 6.1 选择列

推荐：

```r
x[vars]
x[, vars, drop = FALSE]
```

不要使用：

```r
dplyr::select(x, vars)
```

---

### 6.2 筛选行

推荐：

```r
x[x$sex == "Male", , drop = FALSE]
x[!is.na(x$age), , drop = FALSE]
```

不要使用：

```r
dplyr::filter(x, sex == "Male")
```

---

### 6.3 新增变量

推荐：

```r
x$rate <- x$cases / x$population * 100000
```

或：

```r
x <- within(x, {
  rate <- cases / population * 100000
})
```

不要使用：

```r
dplyr::mutate(x, rate = cases / population * 100000)
```

---

### 6.4 分组汇总

推荐：

```r
aggregate(cases ~ year + sex, data = x, FUN = sum, na.rm = TRUE)
```

或：

```r
res <- split(x, list(x$year, x$sex), drop = TRUE)
res <- lapply(res, function(z) {
  data.frame(
    cases = sum(z$cases, na.rm = TRUE),
    population = sum(z$population, na.rm = TRUE)
  )
})
res <- do.call(rbind, res)
```

不要使用：

```r
dplyr::group_by(x, year, sex)
dplyr::summarise(...)
```

---

### 6.5 合并数据

推荐：

```r
merge(x, y, by = "id", all.x = TRUE)
```

不要使用：

```r
dplyr::left_join(x, y, by = "id")
```

---

### 6.6 字符串处理

推荐使用 base R：

```r
grepl(pattern, x)
grep(pattern, x, value = TRUE)
gsub(pattern, replacement, x)
sub(pattern, replacement, x)
regexpr(pattern, x)
gregexpr(pattern, x)
regmatches(x, m)
trimws(x)
paste(x, collapse = ", ")
sprintf("%04d", x)
```

不要使用：

```r
stringr::str_detect()
stringr::str_extract()
stringr::str_replace()
stringr::str_trim()
```

---

## 7. 向量化原则

优先编写向量化函数。

推荐：

```r
classify_sex <- function(x) {
  res <- rep(NA_character_, length(x))

  res[x == 1] <- "Male"
  res[x == 2] <- "Female"

  res
}
```

不推荐：

```r
classify_sex <- function(x) {
  for (i in seq_along(x)) {
    ...
  }
}
```

如果循环更清晰，可以使用 `for`，但应保证代码简单、可读、稳定。

---

## 8. 缺失值处理规范

所有函数应明确处理 `NA`。

示例：

```r
clean_code <- function(x, na_drop = FALSE) {
  x <- as.character(x)
  x <- trimws(x)

  if (na_drop) {
    x <- x[!is.na(x)]
  }

  x
}
```

不要让 `NA` 在逻辑判断中产生错误：

```r
if (x == "A") {
  ...
}
```

应改为：

```r
if (!is.na(x) && x == "A") {
  ...
}
```

向量判断推荐：

```r
idx <- !is.na(x) & x == "A"
```

---

## 9. S3 方法开发规范

### 9.1 S3 泛型

S3 泛型应简洁：

```r
clean_registry <- function(x, ...) {
  UseMethod("clean_registry")
}
```

### 9.2 默认方法

应提供 default 方法：

```r
clean_registry.default <- function(x, ...) {
  stop(
    "No `clean_registry()` method for objects of class: ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}
```

### 9.3 data.frame 方法

```r
clean_registry.data.frame <- function(x, ...) {
  check_data_frame(x)

  x
}
```

### 9.4 NAMESPACE 导出

roxygen2 中应写：

```r
#' @export
clean_registry <- function(x, ...) {
  UseMethod("clean_registry")
}

#' @export
clean_registry.data.frame <- function(x, ...) {
  ...
}
```

---

## 10. roxygen2 文档规范

所有导出函数必须有完整 roxygen2 文档。

模板：

```r
#' Clean registry data
#'
#' Clean and standardize cancer registry data using base R.
#'
#' @param x A data frame.
#' @param vars A character vector of variable names to clean.
#' @param na_drop Logical. Whether to remove missing values.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with cleaned variables.
#'
#' @details
#' This function uses base R only and does not depend on tidyverse packages.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   code = c(" C50.9 ", "C34.1", NA)
#' )
#'
#' clean_registry(df)
#'
#' @export
clean_registry <- function(x, vars = NULL, na_drop = FALSE, ...) {
  UseMethod("clean_registry")
}
```

文档应包括：

- 函数目的
- 参数含义
- 返回值
- 细节说明
- 示例
- 是否导出

示例必须能通过 `R CMD check`。

不要在 examples 中使用需要外部文件、网络连接或复杂环境的代码。

---

## 11. 单元测试规范

使用 `testthat` 进行测试。

测试文件命名：

```text
tests/testthat/test-clean-registry.R
tests/testthat/test-classify-icd10.R
tests/testthat/test-count-cases.R
```

测试应覆盖：

- 正常输入
- 缺失值
- 空向量
- 错误输入
- 边界值
- 类型错误
- S3 方法分发
- 输出类型
- 输出长度
- 输出列名

示例：

```r
test_that("classify_sex works with valid input", {
  expect_equal(
    classify_sex(c(1, 2, 9, NA)),
    c("Male", "Female", NA, NA)
  )
})

test_that("classify_sex keeps input length", {
  x <- c(1, 2, NA, 9)
  expect_length(classify_sex(x), length(x))
})

test_that("classify_sex requires numeric input", {
  expect_error(classify_sex(c("1", "2")))
})
```

---

## 12. DESCRIPTION 文件规范

`DESCRIPTION` 应尽量减少依赖。

推荐：

```text
Package: mypackage
Title: Tools for Cancer Registry Data Processing
Version: 0.0.0.9000
Authors@R: 
    person("Qiong", "Chen", email = "your@email.com", role = c("aut", "cre"))
Description: Provides tools for cleaning, validating, classifying, and summarising cancer registry data using base R.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Depends:
    R (>= 4.1.0)
Suggests:
    testthat (>= 3.0.0),
    knitr,
    rmarkdown
Config/testthat/edition: 3
```

避免不必要的：

```text
Imports:
    dplyr,
    tidyr,
    stringr,
    purrr,
    data.table
```

除非用户明确要求，不要主动加入这些依赖。

---

## 13. NAMESPACE 规范

不要手动编辑 `NAMESPACE`，应通过 roxygen2 自动生成。

使用：

```r
devtools::document()
```

或：

```r
roxygen2::roxygenise()
```

导出函数时使用：

```r
#' @export
```

如果需要注册 S3 方法，应确保 roxygen2 正确生成 NAMESPACE。

---

## 14. 内部函数规范

内部函数不需要导出。

内部函数命名建议：

```r
check_*
validate_*
standardize_*
format_*
parse_*
internal_*
```

示例：

```r
check_required_columns <- function(x, cols) {
  missing_cols <- cols[!cols %in% names(x)]

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
```

内部函数如果被多个函数复用，应放在：

```text
R/utils.R
R/checks.R
R/helpers.R
```

---

## 15. 兼容 CRAN 的代码要求

开发时应遵守 CRAN 要求。

### 15.1 不使用绝对路径

不要写：

```r
read.csv("/Users/name/project/data/file.csv")
```

应使用：

```r
system.file("extdata", "file.csv", package = "mypackage")
```

或在 examples 中使用临时文件：

```r
tmp <- tempfile(fileext = ".csv")
write.csv(x, tmp, row.names = FALSE)
read.csv(tmp)
```

---

### 15.2 不在函数中直接修改用户环境

不要使用：

```r
assign("x", value, envir = .GlobalEnv)
```

不要使用：

```r
setwd()
```

不要在包函数中随意改变全局选项：

```r
options(stringsAsFactors = FALSE)
```

如必须修改，应恢复：

```r
old <- options(...)
on.exit(options(old), add = TRUE)
```

---

### 15.3 不自动安装包

不要写：

```r
install.packages("xxx")
```

包函数不应自动安装依赖。

---

### 15.4 不依赖交互输入

不要在包函数中使用：

```r
readline()
menu()
file.choose()
```

除非这是明确设计为交互式函数。

---

### 15.5 examples 应可运行

不要写需要真实本地文件的示例：

```r
read_registry("D:/data/cancer.xlsx")
```

应写最小可复现示例。

---

## 16. 性能优化原则

在 base R 中应尽量避免低效写法。

### 16.1 避免循环中不断增长对象

不推荐：

```r
res <- c()

for (i in seq_along(x)) {
  res <- c(res, f(x[i]))
}
```

推荐：

```r
res <- vector("list", length(x))

for (i in seq_along(x)) {
  res[[i]] <- f(x[i])
}

res <- unlist(res, use.names = FALSE)
```

或：

```r
res <- vapply(x, f, character(1))
```

---

### 16.2 优先使用 vapply

推荐：

```r
res <- vapply(x, function(z) {
  paste(z, collapse = ", ")
}, character(1))
```

不推荐：

```r
res <- sapply(x, function(z) {
  paste(z, collapse = ", ")
})
```

`vapply()` 更安全，因为它显式指定返回类型。

---

### 16.3 大数据处理

如果数据量较大，优先考虑：

- 减少重复拷贝
- 提前选择必要列
- 使用 `match()` 替代反复 `%in%`
- 使用因子或整数索引
- 使用 `split()` 后批量处理
- 避免嵌套循环

---

## 17. 可读性规范

代码应清晰胜过炫技。

推荐：

```r
idx_valid <- !is.na(code) & grepl("^C[0-9]{2}\\.[0-9]$", code)
res <- rep(NA_character_, length(code))
res[idx_valid] <- code[idx_valid]
```

不推荐：

```r
res <- ifelse(!is.na(code) & grepl("^C[0-9]{2}\\.[0-9]$", code), code, NA)
```

对于复杂逻辑，应拆分为多个中间变量。

---

## 18. 管道符使用规范

由于本项目以 base R 为主，默认不使用 `%>%` 或 `|>`。

如用户明确希望使用 base pipe，可以使用：

```r
x |>
  transform(rate = cases / population * 100000)
```

但在包内部核心函数中，优先使用普通赋值，保证兼容性和可读性：

```r
x$rate <- x$cases / x$population * 100000
```

---

## 19. 数据对象规范

包内置数据应放在 `data/` 或 `inst/extdata/`。

### 19.1 用户可直接调用的数据

放在：

```text
data/
```

生成 `.rda` 文件。

文档示例：

```r
#' ICD-10 cancer code labels
#'
#' A data frame containing ICD-10 cancer code labels.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{code}{ICD-10 code.}
#'   \item{label}{Code label.}
#' }
"icd10_labels"
```

### 19.2 示例文件

放在：

```text
inst/extdata/
```

读取方式：

```r
system.file("extdata", "example.csv", package = "mypackage")
```

---

## 20. 测试数据规范

测试中应尽量构造小型数据。

推荐：

```r
df <- data.frame(
  id = 1:4,
  sex = c(1, 2, 1, NA),
  age = c(45, 60, NA, 70),
  icd10 = c("C50.9", "C34.1", "D05.1", NA),
  stringsAsFactors = FALSE
)
```

不推荐依赖大型外部数据文件。

---

## 21. 注释规范

注释应解释“为什么”，而不是重复“做什么”。

推荐：

```r
# Keep original length so that results can be assigned back to input data.
res <- rep(NA_character_, length(x))
```

不推荐：

```r
# Create result
res <- rep(NA_character_, length(x))
```

---

## 22. 版本控制规范

每次修改应尽量聚焦一个主题。

推荐提交信息：

```text
Add ICD-10 validation helper
Fix NA handling in classify_sex
Add tests for count_cases
Refactor rate calculation using base R
```

不推荐：

```text
update
fix
changes
new
```

---

## 23. Cline 工作方式要求

当 Cline 修改代码时，应遵守以下要求：

1. 先理解现有代码结构；
2. 不随意重写整个项目；
3. 优先做最小必要修改；
4. 保持现有函数风格一致；
5. 不主动引入新依赖；
6. 不删除已有测试，除非测试确实错误；
7. 修改函数后同步更新文档；
8. 修改导出函数后检查 NAMESPACE；
9. 修改功能后补充或更新测试；
10. 如果发现潜在 bug，应说明原因并给出修复建议。

---

## 24. 开发任务执行流程

每次开发一个新函数时，按照以下步骤：

### 第一步：明确函数目标

回答：

- 输入是什么？
- 输出是什么？
- 是否保持输入长度？
- 是否允许 `NA`？
- 是否需要 S3 方法？
- 是否需要导出？
- 是否需要单元测试？

### 第二步：编写函数骨架

```r
function_name <- function(x, ...) {
  ...
}
```

### 第三步：添加输入检查

```r
if (!is.data.frame(x)) {
  stop("`x` must be a data frame.", call. = FALSE)
}
```

### 第四步：实现核心逻辑

优先使用 base R。

### 第五步：处理边界情况

包括：

- `NULL`
- `NA`
- 空向量
- 零行数据框
- 重复值
- 非法编码
- 类型不匹配

### 第六步：添加 roxygen2 文档

包括：

- title
- description
- params
- return
- details
- examples
- export

### 第七步：添加测试

至少测试：

- 正常情况
- 异常情况
- 缺失值
- 边界值

### 第八步：运行检查

建议运行：

```r
devtools::document()
devtools::test()
devtools::check()
```

如果不使用 devtools，可运行：

```r
roxygen2::roxygenise()
testthat::test_local()
```

---

## 25. 常用 base R 模板

### 25.1 检查必需列

```r
check_required_columns <- function(x, cols) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  if (!is.character(cols)) {
    stop("`cols` must be a character vector.", call. = FALSE)
  }

  missing_cols <- cols[!cols %in% names(x)]

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
```

---

### 25.2 标准化字符向量

```r
standardize_character <- function(x,
                                  trim = TRUE,
                                  empty_as_na = TRUE,
                                  upper = FALSE) {
  x <- as.character(x)

  if (trim) {
    x <- trimws(x)
  }

  if (empty_as_na) {
    x[x == ""] <- NA_character_
  }

  if (upper) {
    x <- toupper(x)
  }

  x
}
```

---

### 25.3 匹配字典标签

```r
add_labels <- function(x, dict, value_col = "code", label_col = "label") {
  if (!is.data.frame(dict)) {
    stop("`dict` must be a data frame.", call. = FALSE)
  }

  check_required_columns(dict, c(value_col, label_col))

  pos <- match(x, dict[[value_col]])
  dict[[label_col]][pos]
}
```

---

### 25.4 安全求和

```r
safe_sum <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = na.rm)
}
```

---

### 25.5 压缩连续数字范围

```r
compress_range <- function(x, sep = ", ", conn = "-") {
  x <- sort(unique(x))
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  d <- c(TRUE, diff(x) != 1)
  group <- cumsum(d)
  parts <- split(x, group)

  out <- vapply(parts, function(z) {
    if (length(z) == 1L) {
      as.character(z)
    } else {
      paste0(min(z), conn, max(z))
    }
  }, character(1))

  paste(out, collapse = sep)
}
```

---

## 26. 不推荐写法

除非用户明确要求，否则避免以下写法。

### 26.1 不必要的 tidyverse

```r
x |>
  dplyr::filter(!is.na(code)) |>
  dplyr::mutate(code = stringr::str_trim(code))
```

应改为：

```r
x <- x[!is.na(x$code), , drop = FALSE]
x$code <- trimws(x$code)
```

---

### 26.2 不稳定的 sapply

```r
sapply(x, f)
```

应优先使用：

```r
vapply(x, f, character(1))
```

---

### 26.3 隐式部分匹配

避免：

```r
x$co
```

应使用完整列名：

```r
x$code
```

或：

```r
x[["code"]]
```

---

### 26.4 依赖全局变量

避免：

```r
clean_data <- function(x) {
  x$code <- dict[x$code]
  x
}
```

应改为：

```r
clean_data <- function(x, dict) {
  x$code <- dict[x$code]
  x
}
```

如果是包内数据，应明确引用。

---

## 27. 输出对象规范

函数输出应稳定。

如果函数返回数据框：

- 不应无故改变行顺序；
- 不应无故删除列；
- 不应改变用户未指定的变量；
- 应保留必要属性；
- 应明确新增列名称。

示例：

```r
x$rate <- x$cases / x$population * 100000
x
```

如果函数返回向量：

- 长度应尽量与输入一致；
- `NA` 应在原位置保留；
- 类型应稳定。

示例：

```r
res <- rep(NA_character_, length(x))
res[valid] <- value[valid]
res
```

---

## 28. 包开发检查清单

每次完成一个功能后，应检查：

- [ ] 是否使用 base R 实现？
- [ ] 是否避免了不必要依赖？
- [ ] 是否有输入检查？
- [ ] 是否正确处理 `NA`？
- [ ] 是否有清晰错误信息？
- [ ] 是否保持输出类型稳定？
- [ ] 是否添加 roxygen2 文档？
- [ ] 是否添加 examples？
- [ ] examples 是否可以运行？
- [ ] 是否添加单元测试？
- [ ] 是否通过 `devtools::test()`？
- [ ] 是否通过 `devtools::check()`？
- [ ] 是否没有 CRAN NOTE/WARNING/ERROR？
- [ ] 是否没有使用绝对路径？
- [ ] 是否没有修改用户全局环境？
- [ ] 是否没有自动安装包？

---

## 29. Cline 回复要求

当 Cline 回答问题或修改代码时，应尽量做到：

1. 先指出问题核心；
2. 给出 base R 解决方案；
3. 必要时解释为什么这样写；
4. 保留原函数设计意图；
5. 优先给出可直接复制运行的代码；
6. 如果涉及 R 包，应同时考虑文档、测试和 CRAN 检查；
7. 如果发现代码中存在潜在风险，应指出；
8. 如果用户提供了现有函数，应尽量在原函数基础上修改，而不是完全重写；
9. 如果必须重写，应说明重写原因；
10. 不要主动把代码改成 tidyverse 风格。

---

## 30. 推荐开发命令

常用开发命令如下：

```r
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

构建包：

```r
devtools::build()
```

安装本地包：

```r
devtools::install()
```

检查文档：

```r
tools::checkRd("man/function_name.Rd")
```

检查拼写：

```r
spelling::spell_check_package()
```

---

## 31. 最终要求

本项目的 R 包开发应坚持以下原则：

- 使用 base R 为主；
- 减少外部依赖；
- 函数职责单一；
- 输入检查明确；
- 错误信息友好；
- 输出结构稳定；
- 文档完整；
- 测试充分；
- 符合 CRAN 要求；
- 代码可长期维护。

除非用户明确提出，否则不要引入 tidyverse、data.table 或其他重型依赖。
