library(RSQLite)
mypath<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(mypath)

# 读取RSQLite中的数据 -----------------------------------------------------------


# 连接到特定位置的数据库文件
con <- dbConnect(SQLite(), dbname = "credentials.sqlite")

# 获取表格列表
table_list <- dbListTables(con)
print(table_list)

# 执行查询
query <- "SELECT * FROM credentials"
result <- dbGetQuery(con, query)
print(result)

# Insert the new rows into the database table
dbWriteTable(con, "your_table_name", new_rows, append = TRUE)
# 关闭连接
dbDisconnect(con)


# 随机生成卡密储存至Rdata中------------------------------------------------------------------

# 创建包含大小写英文字母的字符向量
letters <- c(letters, LETTERS)

# 创建一个空的数据框
random_strings_df <- data.frame(strings = character(), stringsAsFactors = FALSE)

# 生成100个随机字符串并放入数据框
for (i in 1:300) {
  random_letters <- sample(letters, 20)
  random_string <- paste(random_letters, collapse = "")
  random_strings_df <- rbind(random_strings_df, data.frame(strings = random_string, stringsAsFactors = FALSE))
}
# random_strings_df$strings <- paste0("day",random_strings_df$strings)
random_strings_df$day <- 3
random_strings_df[200:300,2] <- 7
keydatabase <- random_strings_df
colnames(keydatabase)[1] <- "key"
save(keydatabase, file = "./data/key.RData")