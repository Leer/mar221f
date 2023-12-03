# SQL advanced {-}



## Запись занятия {-}

<iframe width="560" height="315" src="https://www.youtube.com/embed/s7zZc-F3ENQ?si=Y3fpPBF71MqjmPJB" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Части SQL {-}

### Data Definition Language {-}

Набор команд для работы с объектами базы данных. С помощью этих команд можно создать, удалить или изменить какой-нибудь объект: таблицу, схему, функцию и т. д. Как правило для этих команд требуются дополнительные права пользователей.

CREATE – для создания объектов базы данных
ALTER – для изменения объектов базы данных
DROP – для удаления объектов базы данных

### Data Manipulation Language {-}

Набор команд для работы с данными - выбор данных (из таблицы или таблиц), добавление или изменение данных, удаление данных (обычно строк из таблицы, сама таблица при этом остается на месте - для ее удаления нужно сделать DROP TABLE).

SELECT – выборка данных
INSERT – добавляет новые данные
UPDATE – изменяет существующие данные
DELETE – удаляет данные

### Data Control Language {-}

Организация и контроль над доступом к базе данных. Например, службе дашборда надо выдать права на чтение данных.

GRANT – предоставляет пользователю или группе разрешения на определённые операции с объектом
REVOKE – отзывает выданные разрешения
DENY – задаёт запрет, имеющий приоритет над разрешением

### Transaction Control Language {-}

Команды для работы с транзакциями (группами запросов, которые выполняются пакетно, чтобы не было неконсистетности в данных). Обычно аналитики с такими задачами не сталкиваются.

BEGIN – служит для определения начала транзакции
COMMIT – применяет транзакцию
ROLLBACK – откатывает все изменения, сделанные в контексте текущей транзакции
SAVEPOINT – устанавливает промежуточную точку сохранения внутри транзакции

## data types {-}

### list {-}

В PostgreSQL (как и в других диалектах) есть большой набор разных типов данных, от стандартных (целые числа, с дробной частью, с плавающей точкой, строки, даты) до экзотических типа ip-адресов. Подробный список типов можно посмотреть вот здесь: [8. Data Types](https://www.postgresql.org/docs/current/sql.html): 

8.1. Numeric Types
8.2. Monetary Types
8.3. Character Types
8.4. Binary Data Types
8.5. Date/Time Types
8.6. Boolean Type
8.7. Enumerated Types
8.8. Geometric Types
8.9. Network Address Types
8.10. Bit String Types
8.11. Text Search Types
8.12. UUID Type
8.13. XML Type
8.14. JSON Types
8.15. Arrays
8.16. Composite Types
8.17. Range Types
8.18. Domain Types
8.19. Object Identifier Types
8.20. pg_lsn Type
8.21. Pseudo-Types

### Numeric types {-}

При работе с числовыми типами надо помнить о такой особенности, что целые числа и числа с дробью - это разные типы. И, например, при делении целого числа на целое SQL вернет также целое число (в R будет неявное преобращование типа):


```sql
select 1 / 7, 7 / 3
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-1)1 records

| ?column?| ?column?|
|--------:|--------:|
|        0|        2|

</div>

Один из самых простых вариантов явного преобразования - умножить целое число с типом numeric (то есть, на 1.000):

```sql
select 1 * 1.000 / 7
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-2)1 records

|  ?column?|
|---------:|
| 0.1428571|

</div>

### Character Types {-}

Строковые типы, такие же как и в других языках программирования.


```sql
select 'abc'
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-3)1 records

|?column? |
|:--------|
|abc      |

</div>

Из полезных функций - конкатенация (слияние строк, аналог `paste0()` в R) и изменение регистра.

```sql
select 'a' || 'b', upper('abc'), lower('ABC')
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-4)1 records

|?column? |upper |lower |
|:--------|:-----|:-----|
|ab       |ABC   |abc   |

</div>

Для работы со строковыми данными есть большая группа функций, использующих регулярные выражения. Вообще, регулярные выражения - весьма часто встречающаяся в жизни аналитиков вещь и их стоит освоить.

### Date/Time Types  {-}

Даты и время. Несмотря на то, что для для людей более читабельны даты и время в ISO-представлении ('гггг-мм-дд чч:мм:сс'), лучше использовать unix-timestamp -- представление даты в виде количества секнуд с 1970-01-01. Это представление проще, удобнее для хранения, не зависит от таймзоны пользователя и базы данных.


```sql
--текущая дата
select current_date
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-5)1 records

|date       |
|:----------|
|2023-12-03 |

</div>

Для преобразования даты в unix-timestamp используют функцию `extract()` с указанием, что извлекается `epoch`.

```sql
select extract(epoch from current_date)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-6)1 records

|  date_part|
|----------:|
| 1701561600|

</div>

Обратное преобразование с помощью `to_timestamp()`:

```sql
select to_timestamp(1636156800)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-7)1 records

|to_timestamp        |
|:-------------------|
|2021-11-06 03:00:00 |

</div>

Даты вычитать достаточно просто, date - date. Но если надо из даты вычесть количество дней / месяцев / лет (или другой интервал), то можно воспользоваться следующей конструкцией:

```sql
select current_date - interval '1' day
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-8)1 records

|?column?   |
|:----------|
|2023-12-02 |

</div>

### type Conversion  {-}

Для преобразования типов в Postgresql обычно используют `::`, также есть более классическая и распространенная во всех диалектах функция `cast()`:


```sql
select 
  1 * 1.000 / 7, 
  1 :: numeric / 7,
  cast(1 as numeric) / 7
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-9)1 records

|  ?column?|  ?column?|  ?column?|
|---------:|---------:|---------:|
| 0.1428571| 0.1428571| 0.1428571|

</div>

## values {-}

Иногда бывают ситуации, когда надо создать таблицу в запросе - для этого можно с помощью команды values вычислить набор строк, в которых заданы значения (количество значений в строках должно быть одинаковыми). Названия колонок в создаваемой таблице можно задать с помощью as tablename(col1_name, col2_name...), по количеству создаваемых колонок.


```sql
select 
	* 
from (
	values 
		(1, 'a', 'grp1'),
		(2, 'b', 'grp1')
) as tbl(var1, var2, var3)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-10)2 records

|var1 |var2 |var3 |
|:----|:----|:----|
|1    |a    |grp1 |
|2    |b    |grp1 |

</div>

## subquery {-}

Нередко в запросах надо обратиться к подвыборке из другой таблицы. Например, это может быть как в разделе join:


```sql
select 
	* 
-- создаем и обращаемся к первой таблице
from (
	values 
		(1, 'a', 'grp1'),
		(2, 'b', 'grp1')
) as tbl(var1, var2, var3)
-- создаем и джойним вторую таблицу
left join (
  select * from (
    values 
		('2021-11-06', 'grp1')      
  ) as tb2(var4, var3)
) as t2 using(var3)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-11)2 records

|var3 | var1|var2 |var4       |
|:----|----:|:----|:----------|
|grp1 |    1|a    |2021-11-06 |
|grp1 |    2|b    |2021-11-06 |

</div>

Более простой пример с уже существующими таблицами:

```sql
select
	*
from chars
left join (
	select planet_name, gravity
	from planets
	where climate = 'temperate'
) as p using(planet_name)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-12)Displaying records 1 - 10

|planet_name |row.names |name               | height| mass|hair_color  |skin_color |eye_color |birth_year |gender |url                             |gravity |
|:-----------|:---------|:------------------|------:|----:|:-----------|:----------|:---------|:----------|:------|:-------------------------------|:-------|
|Tatooine    |1         |Luke Skywalker     |    172|   77|blond       |fair       |blue      |19BBY      |male   |https://swapi.co/api/people/1/  |NA      |
|Tatooine    |2         |C-3PO              |    167|   75|n/a         |gold       |yellow    |112BBY     |n/a    |https://swapi.co/api/people/2/  |NA      |
|Tatooine    |3         |Darth Vader        |    202|  136|none        |white      |yellow    |41.9BBY    |male   |https://swapi.co/api/people/4/  |NA      |
|Tatooine    |4         |Owen Lars          |    178|  120|brown, grey |light      |blue      |52BBY      |male   |https://swapi.co/api/people/6/  |NA      |
|Tatooine    |5         |Beru Whitesun lars |    165|   75|brown       |light      |blue      |47BBY      |female |https://swapi.co/api/people/7/  |NA      |
|Tatooine    |6         |R5-D4              |     97|   32|n/a         |white, red |red       |unknown    |n/a    |https://swapi.co/api/people/8/  |NA      |
|Tatooine    |7         |Biggs Darklighter  |    183|   84|black       |light      |brown     |24BBY      |male   |https://swapi.co/api/people/9/  |NA      |
|Tatooine    |8         |Anakin Skywalker   |    188|   84|blond       |fair       |blue      |41.9BBY    |male   |https://swapi.co/api/people/11/ |NA      |
|Tatooine    |9         |Shmi Skywalker     |    163|   NA|black       |fair       |brown     |72BBY      |female |https://swapi.co/api/people/43/ |NA      |
|Tatooine    |10        |Cliegg Lars        |    183|   NA|brown       |fair       |blue      |82BBY      |male   |https://swapi.co/api/people/62/ |NA      |

</div>

Также вложенные запросы могут быть в блоке `where`:

```sql
select
	*
from chars
where planet_name in (
	select planet_name from planets where climate = 'temperate'
)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-13)Displaying records 1 - 10

|row.names |name                  | height| mass|hair_color    |skin_color       |eye_color |birth_year |gender        |url                             |planet_name |
|:---------|:---------------------|------:|----:|:-------------|:----------------|:---------|:----------|:-------------|:-------------------------------|:-----------|
|11        |Boba Fett             |    183| 78.2|black         |fair             |brown     |31.5BBY    |male          |https://swapi.co/api/people/22/ |Kamino      |
|12        |Lama Su               |    229| 88.0|none          |grey             |black     |unknown    |male          |https://swapi.co/api/people/72/ |Kamino      |
|13        |Taun We               |    213|   NA|none          |grey             |black     |unknown    |female        |https://swapi.co/api/people/73/ |Kamino      |
|19        |Leia Organa           |    150| 49.0|brown         |light            |brown     |19BBY      |female        |https://swapi.co/api/people/5/  |Alderaan    |
|20        |Bail Prestor Organa   |    191|   NA|black         |tan              |brown     |67BBY      |male          |https://swapi.co/api/people/68/ |Alderaan    |
|21        |Raymus Antilles       |    188| 79.0|brown         |light            |brown     |unknown    |male          |https://swapi.co/api/people/81/ |Alderaan    |
|22        |Obi-Wan Kenobi        |    182| 77.0|auburn, white |fair             |blue-gray |57BBY      |male          |https://swapi.co/api/people/10/ |Stewjon     |
|24        |Han Solo              |    180| 80.0|brown         |fair             |brown     |29BBY      |male          |https://swapi.co/api/people/14/ |Corellia    |
|25        |Wedge Antilles        |    170| 77.0|brown         |fair             |hazel     |21BBY      |male          |https://swapi.co/api/people/18/ |Corellia    |
|27        |Jabba Desilijic Tiure |    175|   NA|n/a           |green-tan, brown |orange    |600BBY     |hermaphrodite |https://swapi.co/api/people/16/ |Nal Hutta   |

</div>


## Common tables expressions {-}

Общие таблицы или "выражения с with" -- крайне полезный инструмент, так как позволяет создавать в запросе временные таблицы (которые живут только во время запроса и нигде не созраняются) и обращаться к этим таблицам во время запроса.

Для экспериментов удобно совмещать создание таблиц из заданных значений с помощью values и операции с этими таблицами с помощью with:

```sql
-- указываем, что таблицы из запросов ниже будут временными и общими для всего запроса
with 
  -- создаем первую таблицу
	tmp1 as (
		select 
			* 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1')
		) as tbl(var1, var2, var3)
	),
	-- создаем вторую таблицу
	tmp2 as (
    select * from (
      values 
  		('2021-11-06', 'grp1')      
    ) as tb2(var4, var3)
	)
	
--основная часть - пишем запрос к созданным таблицам
select * 
from tmp1
left join tmp2 using(var3)
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-14)2 records

|var3 | var1|var2 |var4       |
|:----|----:|:----|:----------|
|grp1 |    1|a    |2021-11-06 |
|grp1 |    2|b    |2021-11-06 |

</div>


## select experiments {-}

### functions {-}

В блоке select можно использовать разные, временами сложные конструкции. Самое простое - какая-то операция с колонкой, например, вычисление среднего (для среднего в sql-диалектах используется функция `avg()`) или максимума. 


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1')
		) as tbl(v1, v2, v3)
	)
select
	count(*) as n_rows,
	count(distinct v3) as n_groups,
	avg(v1) as v2_avg
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-15)1 records

| n_rows| n_groups| v2_avg|
|------:|--------:|------:|
|      2|        1|    1.5|

</div>


### case {-}

Немного более сложный, но очень полезный инструмент - оператор логического ветвления. В R это аналог switch или вложенных ifelse. 


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
	*,
	-- открываем логическое ветвление
	case 
	  -- первое условие
		when v1 < 3 then 'g1'
		-- второе условие
		when v1 = 3 then 'g2'
		-- третье условие - "все прочее"
		else 'g3'
	-- закрываем ветвление и указываем, как назвать колонку
	end as grp2
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-16)5 records

|v1 |v2 |v3   |grp2 |
|:--|:--|:----|:----|
|1  |a  |grp1 |g1   |
|2  |b  |grp1 |g1   |
|3  |NA |grp1 |g2   |
|4  |d  |grp2 |g3   |
|5  |e  |grp2 |g3   |

</div>

### filter {-}

Полезная, но достаточно малоизвестная конструкция - значения в колонках можно фильтровать по значениям других колонок. 


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
  -- считаем количество строк, в которых в v3 есть значение grp1
	count(*) filter(where v3 = 'grp1'),
	-- одновременно считаем количество значений в колонке v1, для которых в v3 есть значение grp2
	count(v1) filter(where v3 = 'grp2')
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-17)1 records

| count| count|
|-----:|-----:|
|     3|     2|

</div>

## Window functions {-}

### row_number() over () {-}

Select-запросы в SQL предназначены в первую очередь для извлечения подвыборок (из одной или нескольких таблиц, с определенным составом колонок). Поэтому какие-то более сложные операции бывает достаточно сложно сделать. Одними из таких операций являются действия с колонками, в которых учитываются значения колонки в предыдущих строках - например, кумулятивная сумма или сумма в определенном окне (количестве строк до текущей) и тому подобные.

Такие операции делаются в SQL с помощью оконных функций, где под окном понимается определенный набор строк колонки, с которыми надо выполнить какие-то операции. Один из самых простых видов оконных функций - нумерация строк:


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
	*,
	-- row_number() - функция определения номера, over() - определение окна. 
	-- так как в over() ничего не указано, под окном понимаются все строки таблицы
	row_number() over() as counter
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-18)5 records

|v1 |v2 |v3   | counter|
|:--|:--|:----|-------:|
|1  |a  |grp1 |       1|
|2  |b  |grp1 |       2|
|3  |NA |grp1 |       3|
|4  |d  |grp2 |       4|
|5  |e  |grp2 |       5|

</div>

### over (partition by) {-}

Оконные операции можно выполнять в группах по значениям какой-то колонки, так же при этом можно сортировать строки по другим колонкам:

```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
	*,
	-- указываем, что окно бьется на группы в зависимости от значений v3
	row_number() over(partition by v3) as counter,
	-- указываем, что окно бьется на группы в зависимости от значений v3
	-- и одновременно сортируем значения по убыванию в зависимости от колонки v1
	row_number() over(partition by v3 order by v1 desc) as counter_rev
from tmp
order by v1
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-19)5 records

|v1 |v2 |v3   | counter| counter_rev|
|:--|:--|:----|-------:|-----------:|
|1  |a  |grp1 |       1|           3|
|2  |b  |grp1 |       2|           2|
|3  |NA |grp1 |       3|           1|
|4  |d  |grp2 |       1|           2|
|5  |e  |grp2 |       2|           1|

</div>

### total sum {-}

Другой пример запроса с оконной функцией -- считаем общую сумму по колонке по всей таблице и записываем ее в отдельную колонку (значение суммы одно, просто размножается по количеству строк).


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
	*,
	-- считаем сумму v1 по всем строкам таблицы
	sum(v1) over() as total_sum
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-20)5 records

|v1 |v2 |v3   | total_sum|
|:--|:--|:----|---------:|
|1  |a  |grp1 |        15|
|2  |b  |grp1 |        15|
|3  |NA |grp1 |        15|
|4  |d  |grp2 |        15|
|5  |e  |grp2 |        15|

</div>


### cumulative sum {-}

Более сложная конструкция для вычисления кумулятивной суммы. Здесь мы указываем, что хотим посчитать не просто сумму, а кумулятивную сумму. Кумулятивная сумма представляется как сумма всех значений колонки от начала и до текущей строки -- окно, в котором считается сумма, с каждой строкой расширяется. Такое поведение задается аргументом range, в котором указывем границы (можно и другие границы указать):


```sql
with 
	tmp as (
		select * 
		from (
			values 
				(1, 'a', 'grp1'),
				(2, 'b', 'grp1'),
				(3, NULL, 'grp1'),
				(4, 'd', 'grp2'),
				(5, 'e', 'grp2')
		) as tbl(v1, v2, v3)
	)
select
	*,
	-- для каждой строки считаем сумму v1 от начала до текущей строки
	sum(v1) over(order by v1 range between unbounded preceding and current row) as cum_sum
from tmp
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-21)5 records

|v1 |v2 |v3   | cum_sum|
|:--|:--|:----|-------:|
|1  |a  |grp1 |       1|
|2  |b  |grp1 |       3|
|3  |NA |grp1 |       6|
|4  |d  |grp2 |      10|
|5  |e  |grp2 |      15|

</div>

## explain {-}

### plan {-}

Для оптимизации можно посмотреть план запроса, который составляет оптимизатор. Умение читать и интерпретировать подобные планы приходит с опытом, чем больше - тем лучше, я не настолько хорошо знаю эту область, чтобы полноценно про нее рассказывать. Здесь просто для иллюстрации, что такое вообще есть.


```sql
explain
	select * 
	from chars
	where planet_name = 'Naboo'
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-22)2 records

|QUERY PLAN                                            |
|:-----------------------------------------------------|
|Seq Scan on chars  (cost=0.00..2.96 rows=11 width=94) |
|Filter: (planet_name = 'Naboo'::text)                 |

</div>

### analyze {-}

Когда мы явно указываем `analyze`, оптимизатор не просто создает план запроса, а реально выполняет запрос и выводит, сколько времени потребовалось выполнение того или иного этапа запроса.


```sql
explain analyze
	select * 
	from chars
	where planet_name = 'Naboo'
```


<div class="knitsql-table">


Table: (\#tab:unnamed-chunk-23)5 records

|QUERY PLAN                                                                                       |
|:------------------------------------------------------------------------------------------------|
|Seq Scan on chars  (cost=0.00..2.96 rows=11 width=94) (actual time=0.023..0.025 rows=11 loops=1) |
|Filter: (planet_name = 'Naboo'::text)                                                            |
|Rows Removed by Filter: 66                                                                       |
|Planning time: 0.076 ms                                                                          |
|Execution time: 0.048 ms                                                                         |

</div>




