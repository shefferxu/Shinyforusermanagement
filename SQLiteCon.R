library(RSQLite)
mypath<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(mypath)

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