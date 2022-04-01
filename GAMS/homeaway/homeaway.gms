sets

i teams / FNB, RMA, PAO/
k days / d1*d6/

;

alias(i,j);

Table

W(i,j,k)
$call =xls2gms r=values!coefficients i=values.xlsx o=table1.inc
$include table1.inc

Scalar

M max budget for a single game /750/
B total budget /2500/
;


Variables

x(i,j,k)  gameday variable
y(i,j,k)  budget

z objective function value;


binary variables x;
nonnegative variables y;

Equations

obj objective function
teamCons(i,j) team consistency
dayCons(i,k) day cons
advert(i,j,k) advert cons
unique(i,j) home away
budgetCons budget cons
;

obj.. z =e= sum((i,j,k)$(ord(i) <> ord(j)), W(i,j,k)*y(i,j,k));

teamCons(i,j)$(ord(i) <> ord(j))..  sum((k),x(i,j,k) +x(j,i,k)) =e= 2;
dayCons(i,k).. sum((j)$(ord(i) <> ord(j)), x(i,j,k) + x(j,i,k)) =l= 1;
advert(i,j,k)$(ord(i) <> ord(j)).. y(i,j,k) =l= M*x(i,j,k);
unique(i,j)$(ord(i) <> ord(j)).. sum((k),x(i,j,k)) =e= 1;
budgetCons.. sum((i,j,k)$(ord(i) <> ord(j)),y(i,j,k)) =l= B;


Model schedule  /all/ ;

option limrow  =  115;
option limcol = 115;

Solve schedule using mip maximizing z;

Display z.m, x.l, y.l;




