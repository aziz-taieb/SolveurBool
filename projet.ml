type eb= V of int | VRAI | FAUX | AND of eb *eb | OR of eb * eb | XOR of eb * eb | NAND of eb * eb | NOT of eb ;;
(*type défini  pour faciliter la lecture du système d’équation*)
type eq = Equals of eb * eb ;;
(*append fonction triviale*)
let rec  concat l1 l2 = match l1 with
    []->l2
    |x1::ll1->x1::(concat ll1 l2) ;;
(*appartient fonction qui renvoie true en cas d'appartenance false sinon *)
let rec appartient x l1 = match l1 with
    []->false
    |t1::q1-> if t1 = x
        then true
        else
            appartient x q1;;
(*Fonction qui renvoie toutes les occurences de toutes les variables présentes dans une expression booléenne*)
let rec listevareb e = match e with
	V(a) -> [V(a)]
	|VRAI -> []
	|FAUX -> []
	|AND(a,b) -> concat (listevareb a) (listevareb b)
	|OR(a,b) -> concat (listevareb a) (listevareb b)
	|XOR(a,b) -> concat (listevareb a) (listevareb b)	
	|NAND(a,b) -> concat (listevareb a) (listevareb b)
	|NOT(a) -> listevareb a ;;	

(*Fonction qui renvoie toutes les occurrences de toutes les variables présentes dans une Équation booléenne*)
let listevareq e =match e with
	Equals(a,b) -> concat (listevareb a) (listevareb b) ;;  


(*Fonction qui renvoie toutes les occurrences de toutes les variables présentes dans une liste d'équations  booléenne*)
let rec listevarens e = match e with 
	[]-> []
	|a::l -> concat (listevareq a) (listevarens l) ;;
(*Fonction qui prend en argument une liste l1 de variable et une liste accumulateur l2 et qui met dans l2 au plus une occurrence de chaque variables de l1*)
let rec listevaruni l1 l2 = match l1 with 
	[]-> l2
	|a::ll1 -> if appartient a l2 =true then listevaruni ll1 l2 
			else listevaruni ll1 (a::l2);;  
(*Fonction final exercice 1 qui prend en argument une Liste d’équation et renvoie une liste de toutes les inconnue du système *)
let listevar e = listevaruni (listevarens e) [];;
(*Fonction qui prend en argument une liste de liste de couple l et un couple (Variable, valeur) et ajoute ce couple dans toutes les sous listes de l  *)
let rec ajoutercouple l (a,b) = match l with 
	[]->[]
	|x::ll1 -> ((a,b)::x)::(ajoutercouple ll1 (a,b)) ;;
 
(*Fonction finale question 2 qui prend en argument une liste de variable et renvoie une liste d'environnement pour e *)
let rec listeenv e = match e with
	[]-> [[]]
	|[V(a)] -> [[(V(a),VRAI)] ; [(V(a),FAUX)]]
	|x::ll1 -> let l1 ,l2 = listeenv ll1 , listeenv ll1
			in concat (ajoutercouple l1 (x,VRAI)) 
			          (ajoutercouple l2 (x,FAUX));;
(*Fonction qui prend un élément e de type eb et l’évalue *)
let rec equa e = match e with
	V(a) -> V(a)
        |VRAI -> VRAI
        |FAUX -> FAUX
	|NOT(a) -> if equa a = VRAI then FAUX 
			else VRAI
	|AND(a,b) -> if equa a = VRAI && equa b = VRAI then VRAI
			else FAUX
	|OR(a,b) -> if equa a = FAUX && equa b = FAUX then FAUX
			else VRAI
	|XOR(a,b) ->if equa a = equa b then FAUX
			else VRAI
	|NAND(a,b) -> if equa a = VRAI && equa b = VRAI then FAUX
                        else VRAI
(*Fonction qui prend une équation booléenne et l'évalue*)
let verifega e = match e with Equals(a,b) -> if equa a = equa b then true else false ;;
(*Fonction qui prend un système d’équation sans inconnue,l’évalue et renvoie une liste de booléen [a0,...,an] ou chaque ai correspond à l'évaluation de la ième équation dans le système *)
let rec verifens e = match e with 
	[]->[]
	|x::ll1 -> (verifega x)::(verifens ll1);;
(*Fonction qui prend une liste de booléen et renvoie true si tout les élément sont true false sinon*)
let rec veriffinal e =match e with
	[]->true
	|x::ll1 -> if x = true then veriffinal ll1 else false;;
(*Fonction qui prend un élément eb et un couple (variable , valeur) et remplace toutes les occurrences de variable dans eb par sa valeur *)
let rec remplacer eb (var,vale) = match eb with
	V(a) ->if V(a) = var then vale else V(a)
	|VRAI -> VRAI
        	|FAUX -> FAUX
        	|NOT(a) -> NOT(remplacer a (var,vale))	
        |AND(a,b) ->AND(remplacer a (var,vale),remplacer b (var,vale))
        |OR(a,b) ->OR(remplacer a (var,vale),remplacer b (var,vale))
        |XOR(a,b) ->XOR(remplacer a (var,vale),remplacer b (var,vale))
        |NAND(a,b) ->NAND(remplacer a (var,vale),remplacer b (var,vale));;
(*Fonction qui prend une liste d'équation de type eb et un couple (variable, valeur) et remplace toutes les occurences de la variable par valeur dans le système syseq*)
let rec remplacer2 syseq (var,vale) = match syseq with
	[]->[]
	|Equals(a,b)::ll1->Equals(remplacer a (var,vale),remplacer b (var,vale))::(remplacer2 ll1 (var,vale));;	
(*Fonction qui prend une liste d’équation et une liste de couple (var,val) et remplace toutes les variables par leur valeur dans le système d'équation *)
let rec remplacer3 syseq l = match l with
	[]->syseq
	|x::ll1 ->remplacer3 (remplacer2 syseq x) ll1 ;; 
(*Fonction qui prend un système d’équation et une liste d'environnement et renvoie une liste de toutes les solutions possibles *)
let rec f lens lenv = match lenv with
        []->[] 
	|x::ll1 -> if veriffinal (verifens (remplacer3 lens x))= true
				then x::f lens ll1 else f lens ll1;;
(*Fonnction finale*)
let solveur e = f e (listeenv (listevar e)) ;;
