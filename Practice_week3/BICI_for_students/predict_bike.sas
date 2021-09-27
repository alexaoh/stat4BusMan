
data work.bike_model;
  set work.Rdata;
  %include "bike.sas";
  run;

  proc print data=work.bike_model;
  run;
