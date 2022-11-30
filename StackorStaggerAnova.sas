proc import datafile = 'C:/Users/Austin Hartman/Documents/CS 497 Basketball Sim/basketball-reference-data/StackorStaggerAnovaData.csv'
	out = a1
	dbms = csv
	replace;
run;

ods html path = "" file='C:/Users/Austin Hartman/Documents/CS 497 Basketball Sim/basketball-reference-data/StackorStaggerAnovaOutput.html' style = htmlblue;

proc glm;
   class NewDouble_Arch;
   model SuggProp = NewDouble_Arch;
   means NewDouble_Arch / bon CLdiff linestable;
   lsmeans NewDouble_Arch / adjust=bon CL pdiff linestable;
run;

proc glm;
   class NewDouble_Arch;
   model ActualProp = NewDouble_Arch;
   means NewDouble_Arch / bon CLdiff linestable;
   lsmeans NewDouble_Arch / adjust=bon CL pdiff linestable;
run;

quit;
ods html close;
