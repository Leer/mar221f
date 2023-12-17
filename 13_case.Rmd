# Cases

## Запись занятия

<iframe width="560" height="315" src="https://www.youtube.com/embed/4jhX-aPotMM?si=ae88sxpZe0vcznoL" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>


<!-- ## тестовое задание на A/B-тест -->

<!-- Процесс решения: -->

<!-- 1. импортируйте данные в R -->
<!-- 2. посмотрите на данные и решите, какую метрику будем оценивать на знач.различий -->
<!-- 3. посмотрите на данные и подумайте, как должна выглядеть таблица, на которой будем проверять гипотезы -->
<!-- 4. проверяем гипотезу о различии (влиянии новой системы рекомендации) -->

<!-- ```{r} -->
<!-- # импорт данных -->
<!-- library(data.table) -->
<!-- clients <- fread('./ab_test_data/clients.csv') -->
<!-- sales <- fread('./ab_test_data/sales.csv') -->
<!-- products <- fread('./ab_test_data/products.csv') -->
<!-- shows <- fread('./ab_test_data/shows.csv') -->
<!-- ``` -->

<!-- ```{r} -->
<!-- # к показам прикрепляем product_name -->
<!-- shows <- merge(shows, products, by.x = 'product_id', by.y = 'id', all.x = TRUE) -->

<!-- # фильтруем показы наших продуктов -->
<!-- shows <- shows[product_name %in% c('insurance', 'deposit_4')] -->
<!-- ``` -->


<!-- ```{r} -->
<!-- # к платежам прикрепляем product_name -->
<!-- sales <- merge(sales, products, by.x = 'product_id', by.y = 'id', all.x = TRUE) -->

<!-- # фильтруем покупки наших продуктов -->
<!-- sales <- sales[product_name %in% c('insurance', 'deposit_4')] -->

<!-- ``` -->


<!-- ```{r} -->
<!-- # к клиентам прикрепляем, сколько раз они видели рекламу наших целевыы продуктов -->
<!-- clients <- merge( -->
<!--   clients, -->
<!--   dcast(shows, client_id ~ paste0('shows_', product_name), fun.aggregate = length), -->
<!--   by = 'client_id', all.x = TRUE -->
<!-- ) -->

<!-- # к клиентам прикрепляем, сколько раз они покупали наши целевые продукты -->
<!-- clients <- merge( -->
<!--   clients, -->
<!--   dcast(sales, client_id ~ paste0('sales_', product_name), fun.aggregate = length), -->
<!--   by = 'client_id', all.x = TRUE -->
<!-- ) -->
<!-- ``` -->


<!-- ```{r} -->
<!-- # считаем статистики по показам и покупкам страховки -->
<!-- # сначала фильтруем клиентов, которые видели рекламу страховки -->
<!-- ab_stat_ins <- clients[!is.na(shows_insurance) & shows_insurance > 0] -->

<!-- # корректируем, если купил страховку больше одного раза просто в купил / не купил -->
<!-- ab_stat_ins[, sales_insurance := ifelse(sales_insurance == 0 | is.na(sales_insurance), 0, 1)] -->

<!-- # считаем количество пользователей и показов в каждой группе -->
<!-- ab_stat_ins_stat <- ab_stat_ins[, list(n_users = uniqueN(client_id)),  -->
<!--                                 by = list(group_type, sales_insurance)] -->

<!-- # считаем, сколько вообще пользователей было в тестовой / контрольной группах -->
<!-- ab_stat_ins_stat[, group_users := sum(n_users), by = group_type] -->

<!-- # смотрим табличку -->
<!-- ab_stat_ins_stat -->

<!-- # считаем статистики значимость различий -->
<!-- prop.test(c(2840, 2937), c(18159, 18234)) -->
<!-- ``` -->


<!-- ```{r} -->
<!-- # аналогично с покупками товара deposit_4 -->
<!-- ab_stat_deposit4 <- clients[!is.na(shows_deposit_4) & shows_deposit_4 > 0] -->
<!-- ab_stat_deposit4[, sales_deposit_4 := ifelse(sales_deposit_4 == 0 | is.na(sales_deposit_4),  -->
<!--                                              0, 1)] -->
<!-- ab_stat_deposit4_stat <- ab_stat_deposit4[, list(n_users = uniqueN(client_id)),  -->
<!--                                           by = list(group_type, sales_deposit_4)] -->
<!-- ab_stat_deposit4_stat[, group_users := sum(n_users), by = group_type] -->
<!-- ab_stat_deposit4_stat -->
<!-- prop.test(c(2813, 2880), c(18148, 18001)) -->
<!-- ``` -->
