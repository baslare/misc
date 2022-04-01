sets

i rows  /
$call =xls2gms r=rows!row i=nonogram.xls o=set1.inc
$include set1.inc
/

j columns /
$call =xls2gms r=columns!column i=nonogram.xls o=set2.inc
$include set2.inc

/

k order of the shape k in a row or column /
$call =xls2gms r=shapes!shape i=nonogram.xls o=set3.inc
$include set3.inc
/
;


alias(i,g);
alias(j,y);
alias(k,p);



Table
W(i,k) row values

$call =xls2gms r=rows!rowtable i=nonogram.xls o=table1.inc
$include table1.inc
;


Table
N(j,k) column values

$call =xls2gms r=columns!columntable i=nonogram.xls o=table2.inc
$include table2.inc
;

Scalar
M a very big number /1000/
;




Variables
x(i,j) ij boyalı mı değil mi
r(i,j,k) r satırındaki kncı cisim ij'den başlıyor mu
c(i,j,k) c sütunundaki kncı cisim ij'den başlıyor mu
er(i,j,k) r satırındaki kncı cisim ij'de bitiyor mu
ec(i,j,k) c sütunundaki kncı cisim ij'de bitiyor mu
z objective function value;

binary variables x, r, c, er, ec;

Equations

obj defines obj function
rowcon(i,k) ensures row consistency and eliminates the need to start shapes of zero length
colcon(j,k) ensures column consistency and eliminates the need to start shapes of zero length
rowtotal(i) ensures total number of colored cells in row i equals to the sum of total shape length
columntotal(j) ensures total number of colored cells in column j equals to the sum of total shape length
rowstart(i,j,k,y) ensures there's a gap to the left of a placed shape
columnstart(i,j,k,g) ensures there's a gap to the top of a placed shape
rowsx(i,j,k) work in progress (aiming to get the behemoth coefficient)
colsx(i,j,k) same as above

endrow(i,j,k,y) must end enough blocks farther
endcol(i,j,k,g) same as above


startuniquer(i,j) part 2 ensures that there are no "rogue" starting variables
startuniquec(i,j) part 2

rendcon(i,k) ensures row cons
cendcon(j,k) ensures columns cons

rendunique(i,j)  adsda
cendunique(i,j)  asda


startnozoner(i,k) asda
startnozonec(j,k) asdda

noendzoner(i,k) asddasd
noendzonec(j,k) asddsad

roworder(i,j,k)
colorder(i,j,k)
;


obj..          z =e= sum((i,j,k), c(i,j,k) + r(i,j,k));

rowtotal(i)..  sum(j, x(i,j)) =e= sum(k, W(i,k));
columntotal(j).. sum(i, x(i,j)) =e= sum(k, N(j,k));

rowcon(i,k)$(W(i,k)>0)..  sum((j), r(i,j,k)) =e= 1;
colcon(j,k)$(N(j,k)>0)..  sum((i), c(i,j,k)) =e= 1;

rendcon(i,k)$(W(i,k)>0).. sum(j, er(i,j,k)) =e= 1;
cendcon(j,k)$(N(j,k)>0).. sum(i, ec(i,j,k)) =e= 1;

startuniquer(i,j).. sum(k, r(i,j,k)) =l= x(i,j);
startuniquec(i,j).. sum(k, c(i,j,k)) =l= x(i,j);

rendunique(i,j).. sum(k, er(i,j,k)) =l= x(i,j);
cendunique(i,j).. sum(k, ec(i,j,k)) =l= x(i,j);

rowstart(i,j,k,y)$(ord(j)-1 = ord(y)).. r(i,j,k) =l= M*(1-x(i,y));
columnstart(i,j,k,g)$(ord(i)-1 = ord(g)).. c(i,j,k) =l= M*(1-x(g,j));

endrow(i,j,k,y)$((W(i,k)>0) and ((ord(j) + W(i,k)-1 = ord(y)) and (ord(y) le card(j)))).. r(i,j,k)=e= er(i,y,k);
endcol(i,j,k,g)$((N(j,k)>0) and ((ord(i) + N(j,k)-1 = ord(g)) and (ord(g) le card(i)))).. c(i,j,k)=e= ec(g,j,k);

rowsx(i,j,k)$((W(i,k)>0) and ((ord(j)+W(i,k)-1) le card(j))).. W(i,k)*r(i,j,k)=l=sum(y$((ord(j) le ord(y)) and (ord(y) le (ord(j) + W(i,k) -1))), x(i,y));
colsx(i,j,k)$((N(j,k)>0) and ((ord(i)+N(j,k)-1) le card(i))).. N(j,k)*c(i,j,k)=l=sum(g$((ord(i) le ord(g)) and (ord(g) le (ord(i) + N(j,k) -1))), x(g,j));

startnozoner(i,k)$(W(i,k)>0).. sum(j$(((card(j)-W(i,k) +1) < ord(j)) and (ord(j) le card(j))),r(i,j,k)) =e= 0;
startnozonec(j,k)$(N(j,k)>0).. sum(i$(((card(i)-N(j,k) +1) < ord(i)) and (ord(i) le card(i))),c(i,j,k)) =e= 0;

noendzoner(i,k)$((W(i,k)>1)).. sum(j$(ord(j) < W(i,k)), er(i,j,k)) =e= 0;
noendzonec(j,k)$((N(j,k)>1)).. sum(i$(ord(i) < N(j,k)), ec(i,j,k)) =e= 0;

roworder(i,j,k).. sum((y,p)$( (ord(y) < ord(j)) and (ord(k) < ord(p)) ), r(i,y,p)) =l= M*(1 - r(i,j,k));
colorder(i,j,k).. sum((g,p)$( (ord(g) < ord(i)) and (ord(k) < ord(p)) ), c(g,j,p)) =l= M*(1 - c(i,j,k));









Model nonogram  /all/ ;

option limrow  =  115;
option limcol = 115;

Solve nonogram using mip minimizing z;

Display z.m, c.l, r.l, x.l, rowsx.m, er.l, ec.l;

execute_unload "results.gdx" x.l
execute "gdxxrw.exe results.gdx o=nonogram.xls var=x.L rng=Results!a1:AY51"
