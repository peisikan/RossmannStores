library(XLConnect) # read xlsx
library(sqldf) # SQL
library(ggplot2)
library(dplyr)

#######################################
wb <- loadWorkbook("Flatiron_Quant_Data_Analyst_Tech_Test_Tables.xlsx")
lst = readWorksheet(wb, sheet = getSheets(wb))

for(i in 1:length(lst))
  assign(names(lst[i]),lst[[i]])

## Q1
###### Obtain the names of all physicians that have performed a medical procedure
###### they have never been certified to perform.

Phys=sqldf(
  'select name 
  from Physician a
  join Undergoes b
  on a.EmployeID = b.Physician
  left join Trained_in c
  on b.Physician = c.Physician
  where b.Date < c.CertificationDate or b.Date > c.CertificationExpires'
  )


##### Also obtain the name of the procedure, 
##### the date when the procedure was carried out, 
##### and the name of the patient on whom the procedure was performed.
names(Procedure)[1:2]=c('Code','Name')
names(Undergoes)[6]=c('AssisstingNurse')

info=sqldf(
  'select d.Name ProcedureName, a.Date, e.Name PatientName
   from Undergoes a
   left join Trained_in b
   on a.Physician = b.Physician
   left join Procedure d
   on a.Procedure = d.Code
   left join Patient e
   on a.Patient = e.SSN
   where a.Date < b.CertificationDate or a.Date > b.CertificationExpires
  ')

info


## Q2
##### Identify the most commonly performed procedures, report on their frequency,
##### and calculate the total revenue by procedure. Comment on your findings.

Q2=sqldf(
  'select a.Code ProcedureCode, a.Name ProcedureName, count(b.Procedure) freq, sum(a.cost) Total_Revenue
  from Procedure a
  left join Undergoes b
  on b.Procedure = a.Code
  group by a.Code
  order by freq desc, Total_Revenue desc
  ')
Q2

ggplot(Q2, aes(x= ProcedureName, y= freq))+
  geom_bar(aes(reorder(ProcedureName, freq)), stat = 'identity', fill = 'blue', alpha = 1/2) +
  coord_flip() + 
  theme_bw() 

ggplot(Q2, aes(x= ProcedureName, y= Total_Revenue))+
  geom_bar(aes(reorder(ProcedureName, Total_Revenue)), stat = 'identity', fill = 'blue', alpha = 1/2) +
  coord_flip() + 
  theme_bw() 


## Q3
##### The hospital is looking to understand the working patterns of the nurses.
##### Identify the nurses that are most frequently on call, for which rooms, 
##### and which procedures they most often assist in.

oncall=sqldf(
  'select a.EmployeeID ID, a.Name NurseName, c.Floor, c.Code, count(*) freq
  from Nurse a
  left join On_call b
  on a.EmployeeID = b.Nurse
  left join Block c
  on b.BlockFloor = c.Floor and b.BlockCode = c.Code
  group by a.EmployeeID, c.Floor, c.Code
  order by a.EmployeeID, c.Floor, c.Code
  ')

onproc=sqldf(
  'select a.EmployeeID ID, a.Name NurseName, e.BlockFloor Floor, e.BlockCode Code, d.Room, c.Name ProcedureName, count(*) freq
  from Nurse a
  left join Undergoes b
  on a.EmployeeID = b.AssisstingNurse
  left join Procedure c
  on b.Procedure = c.Code
  left join Stay d
  on b.Stay = d.StayID
  left join Room e
  on d.Room = e.Number
  group by a.EmployeeID, c.Name, e.BlockFloor, e.BlockCode
  order by a.EmployeeID, e.BlockFloor, e.BlockCode')

# Frequency considering the block floor, block code, and procedure name at the same time
table1=sqldf(
  'select *
  from onproc a
  union 
  select ID, NurseName, Floor, Code, Null Room, Null ProcedureName,freq
  from oncall b
  ')
table1

# Frequency with calls
table2=sqldf(
  'select  ID, NurseName, count(*) freq
   from table1
   group by ID
   order by freq desc
   ')
table2

# Frequency with calls for block floor and block code
table3=sqldf(
  'select ID, NurseName, Floor, Code, count(*) freq
   from table1
   group by ID, Floor, Code
   order by freq desc')
table3




