/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token <string> TID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token PT
%token MULT
%token NULL
%token PLUSEQUAL
%token NEW
%token ADRESSE
%token TYPEDEF
%token STRUCT
%token INF
%token EOF

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e 
%type <expression list> cp
%type <affectable> aff

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| tdI = td   lfi = prog   {let (Programme (tdI2,lf,li))=lfi in (Programme (tdI::tdI2,lf,li))}
| lf = fonc  lfi = prog   {let (Programme (tdI,lf2,li))=lfi in (Programme (tdI,lf::lf2,li))}
| ID li = bloc            {Programme ([],[],li)}

td : TYPEDEF n=TID EQUAL t=typ PV {Typenomme(n,t)}
fonc : t=typ n=ID PO p=dp PF AO li=is AF {Fonction(t,n,p,li)}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| a=aff EQUAL e1=e PV                {Affectation (a,e1)}
| a=aff PLUSEQUAL e1=e PV            {Assignation(a,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| TYPEDEF n=TID EQUAL t=typ PV      {DeclTypeNomme(n,t)}

aff:
    |n=ID         {Variable n}
    |PO MULT a=aff PF {Deref a}
    |PO a=aff PT n=ID PF {Acces(a,n)}

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

typ :
| BOOL    {Bool}
| INT     {Int}
| RAT     {Rat}
| n = TID {TypeNomme n}
| t=typ MULT {Pointeur t}
| STRUCT AO d=dp AF {Struct d}

e : 
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Binaire(Fraction,e1,e2)}
| TRUE                    {Booleen true}
| FALSE                   {Booleen false}
| e=ENTIER                {Entier e}
| NUM e1=e                {Unaire(Numerateur,e1)}
| DENOM e1=e              {Unaire(Denominateur,e1)}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO exp=e PF             {exp}
| NULL                     {Null}
| PO NEW t=typ PF          {Initilisation(t)}
| ADRESSE n=ID             {Addresse(n)}
| a = aff                  {Val(a)}
| AO cp1=cp AF             {Listexp(cp1)}


cp :
|               {[]}
| e1=e le=cp    {e1::le}

