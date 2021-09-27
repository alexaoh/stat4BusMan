* Written by R;
*  write.foreign(df = bike.test, datafile = "gitRepos/stat4BusMan/Practice_week3/BICI_for_students/bike_test.csv",  ;

PROC FORMAT;
value isweekdy 
     1 = "0" 
     2 = "1" 
;

value isholidy 
     1 = "0" 
     2 = "1" 
;

value wethrtyp 
     1 = "1" 
     2 = "2" 
     3 = "3" 
     4 = "4" 
;

DATA  rdata ;
INFILE  "gitRepos/stat4BusMan/Practice_week3/BICI_for_students/bike_test.csv" 
     DSD 
     LRECL= 41 ;
INPUT
 season
 month
 weekday
 hour
 isweekday
 isholiday
 weathertype
 temperature
 humidity
 windspeed
 count
;
FORMAT isweekday isweekdy. ;
FORMAT isholiday isholidy. ;
FORMAT weathertype wethrtyp. ;
RUN;
