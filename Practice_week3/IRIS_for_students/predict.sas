
data work.Rdata2;
  set work.Rdata;
  format species best8.;
  species2="          ";
  format species $10.;
if species=1 then species2="setosa";
if species=2 then species2="versicolor";
if species=3 then species2="virginica";
drop species;
rename species2=species;
run;
data work.iris_model;
  set work.Rdata2;
  %include "iris.sas";
  run;

  proc print data=work.iris_model;
  run;
