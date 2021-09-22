
proc format ; 
value fsurv  0 ="Décédé" 1="Vivant" ; 
value fage   0 ="Enfant" 1="Adulte" ; 
value fsexe  0 ="Homme"  1="Femme" ;
value fclass 0 ="Equipage" 1="1e classe" 2="2eme classe" 3="3eme classe" ; 
run ; 

/* Question 1 */

proc freq data=tmp1.titanic;
table age*surv;
format surv fsurv. age fage.  ; run;

 /* QUESTION 2  */
proc logistic data=tmp1.titanic;
class sexe(ref="1") age(ref="0") classe(ref="0") / param=ref;
model surv(ref="0")=age sexe classe / rsquare;
run;

proc logistic data=tmp1.titanic;
class sexe(ref="1") age(ref="0") classe(ref="0") / param=ref;
model surv(ref="0")=age sexe / rsquare;
run;

proc logistic data=tmp1.titanic;
class sexe(ref="Homme") age(ref="Enfant") classe(ref="Equipage") / param=ref;
model surv(ref="Décédé")=age sexe classe / rsquare ;
format surv fsurv. classe fclass. age fage. sexe fsexe. ; 
run;


/* QUESTION 3 */
proc logistic data=tmp1.titanic;
class sexe(ref="Homme") age(ref="Enfant") classe(ref="Equipage") / param=ref;
model surv(ref="Décédé")=age sexe classe / rsquare;
contrast "proba garçon seconde classe" intercept 1 classe 0 1 0 / est=prob; 
contrast "proba garçon équipage" intercept 1 classe 0 0 0 / est=all ;
contrast "proba garçon première classe" intercept 1 classe 1 0 0 / est=prob; 
contrast "proba garçon troisième classe" intercept 1 classe 0 0 1 / est=prob; 
format surv fsurv. classe fclass. age fage. sexe fsexe. ; 
run;

proc logistic data=tmp1.titanic outest=B ;
class sexe(ref="Homme") age(ref="Enfant") classe(ref="Equipage") / param=ref;
model surv(ref="Décédé")=age sexe classe / rsquare ;
format surv fsurv. classe fclass. age fage. sexe fsexe. ; 
run;

proc logistic data=tmp1.titanic outest=B ;
class sexe(ref="Homme") age(ref="Enfant") classe(ref="Equipage") / param=ref;
model surv(ref="Décédé")=age sexe classe sexe*classe ;
format surv fsurv. classe fclass. age fage. sexe fsexe. ; 
run;

