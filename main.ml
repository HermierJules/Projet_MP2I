open Curses


type skill = {
	name : string;
	description: string;
	mutable range : (int*int) list;
	mutable dmg : int;
	cost : int
	}
type attaque = Nulle | Existe of skill

type entite = {
    mutable mpmax : int;
    mutable hpmax : int;
    mutable hp :  int;
    mutable mp: int;
    mutable x: int;
	mutable y: int;
    mutable skills: attaque * attaque * attaque * attaque;
	mutable moves: int;
	mutable can_move: bool;
	mutable can_attack: bool;
}

type tile = Bord | Mur | Vide | Allie of entite | Ennemi of entite 




(*definitions*)
let dist x1 y1 x2 y2 =
	let distance = Float.sqrt ((float_of_int ((x1-x2)*(x1-x2)))+.(float_of_int ((y1-y2)*(y1-y2)))) in
	distance

let pathfinder map x1 y1 x2 y2 =
    let x = ref x1 and y = ref y1 in
	let flag = ref (false,(0,0)) in
	for i=1 to 10 do
		if i=i then begin (* obligé sinon erreur car i n'est pas utilisé*)
			x := x1;
			y := y1;
			let move = Random.int 4 in
			if move = 0 then x := !x+1
			else if move = 1 then x := !x-1
			else if move = 2 then y := !y+1
			else if move = 3 then y := !y-1;
			if map.(!y).(!x) <> Vide then begin
				x := x1;
				y := y1;
			end;
			if (dist !x !y x2 y2) < (dist x1 y1 x2 y2) then flag := (true,(!x,!y));
		end;
    done;
	match !flag with
	| (b,coords) -> if b = true then coords else (x1,y1) 

let rec in_list x l =
	match l with
	| [] -> false
	| t::q -> if t=x then true else in_list x q

let cases = "##################################-------################-----------############---------------#########-----------------#######-------------------######-------------------#####---------------------####---------------------###-----------------------##-----------------------##-----------------------##-----------------------##-----------------------##-----------------------##-----------------------###---------------------####---------------------#####-------------------######-------------------#######-----------------#########---------------############-----------################-------##################################"

let map_create () = Array.make_matrix 25 25 Vide

let generate_walls map = 
	let check_near map x y=
		let c= ref 0 in
		for i=0 to 2 do
			for j=0 to 2 do
				if i<>1 || j<>1 then 
					match map.(y+i-1).(x+j-1) with
					| Mur -> c:= !c+1
					| Vide -> c:= !c
					| _ -> c:= 10
			done;
		done;
		!c
	in
	let walls = ref 0 in
	while !walls < 40 do
		(*let x = 6+(Random.int 13) and y = 3+(Random.int 19) in  Cela limite le spawn de murs vers le centre de la map *)
		let x = Random.int 25 and y = Random.int 25 in
		if map.(y).(x) = Vide then begin
			if check_near map x y <= 3 then map.(y).(x) <- Mur;
			walls := !walls+1;
		end
	done

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

let blast_skill = {
	name = "Explosion";
	description = "Attaque autour du personnage (Portee: 2)";
	range = [(3,3)];
	dmg = 4;
	cost = 3;
}

let ray_skill = {
	name = "Rayon";
	description = "Attaque droit devant le personnage (Portee: 5)";
	range = [(1,0);(2,0);(3,0);(4,0);(5,0)];
	dmg = 5;
	cost = 4;
}

let slash_skill = {
	name = "Taillade";
	description = "Coup devant le personnage (Portee: 1)";
	range = [(1,-1);(1,0);(1,1)];
	dmg = 6;
	cost = 0;
}

let healAura_skill = {
	name = "Aura de Soin";
	description = "Soin autour du personnage (Portee: 2)";
	range = [(-2,0);(-1,-1);(-1,0);(-1,1);(0,-2);(0,-1);(0,1);(0,2);(1,-1);(1,0);(1,1);(2,0)];
	dmg = -4;
	cost = 3;
}

let mage = {
	hpmax = 20;
	mpmax = 20;
    hp = 20;
    mp = 20;
    x = 4;
	y = 9;
    skills= (Existe blast_skill,Existe ray_skill,Existe slash_skill,Existe healAura_skill);
	moves = 0;
	can_move = true;
	can_attack = true;
}

let warrior = {
	hpmax = 30;
	mpmax = 10;
    hp = 30;
    mp = 10;
    x = 4;
	y = 15;
    skills= (Existe blast_skill,Existe ray_skill,Existe slash_skill,Existe healAura_skill);
	moves = 0;
	can_move = true;
	can_attack = true;
}

let e1 = {
	hpmax = 10;
	mpmax = 5;
    hp = 10;
    mp = 5;
    x = 20;
	y = 9;
    skills= (Existe slash_skill,Existe blast_skill,Existe ray_skill,Existe healAura_skill);
	moves = 3;
	can_move = true;
	can_attack = true;
}

let e2 = {
	hpmax = 10;
	mpmax = 5;
    hp = 10;
    mp = 5;
    x = 20;
	y = 15;
    skills= (Existe slash_skill,Existe blast_skill,Existe ray_skill,Existe healAura_skill);
	moves = 3;
	can_move = true;
	can_attack = true;
}

let e3 = {
	hpmax = 10;
	mpmax = 5;
    hp = 10;
    mp = 5;
    x = 21;
	y = 12;
    skills= (Existe slash_skill,Existe blast_skill,Existe ray_skill,Existe healAura_skill);
	moves = 3;
	can_move = true;
	can_attack = true;
}

(*définitions des couleurs*)

let noir = 0
let gris_fonce = noir+1
let gris = gris_fonce+1
let blanc = gris+1
let rouge = blanc+1
let rouge_clair = rouge+1
let vert = rouge_clair+1
let vert_clair = vert+1
let bleu = vert_clair+1
let bleu_clair = bleu+1


let cree_couleurs () =
    assert(init_color noir 0 0 0);
    assert(init_color gris_fonce 200 200 200);
    assert(init_color gris 500 500 500);
    assert(init_color blanc 1000 1000 1000);
    assert(init_color rouge 1000 0 0);
    assert(init_color vert 0 1000 0);
    assert(init_color vert_clair 500 1000 500);
    assert(init_color bleu 0 0 1000);
    assert(init_color bleu_clair 500 500 1000)

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())
let ncolors = bleu_clair + 1

let cree_paires () =
    let paires = Array.make_matrix ncolors ncolors 0 in
    let p = ref 10 in
    for i = 0 to ncolors-1 do
        for j = 0 to ncolors-1 do
            assert(init_pair !p i j);
            paires.(i).(j) <- !p;
            incr p
        done
    done;
    paires

let paires = 
    (* cree des couleurs et toutes les paires *)
    cree_couleurs ();
    cree_paires ()



let couleur texte fond =
    attron (A.color_pair paires.(texte).(fond))



(* affiche un pixel *)
let putpixel col x y =
    couleur col col;
    assert (mvaddch y x (int_of_char ' '))




let mapofstring s =
    let m = map_create () in
    for y = 0 to 24 do 
        for x = 0 to 24 do
         if s.[x + (y * 25)]  = '#' then m.(y).(x) <- Bord else m.(y).(x) <- Vide
    done
done;
m


let draw_board m h =
    let mult = h / 25 in 
    for y = 0 to 24 do
        for x = 0 to 24 do 
            for i = 0 to mult-1 do 
                     match m.(y).(x) with
					 | Bord -> putpixel gris_fonce (x+i) (y+i)
                     | Vide -> putpixel noir (x+i) (y+i)
                     | Mur -> putpixel  gris (x+i) (y+i)
                     | Ennemi _-> putpixel rouge (x+i) (y+i)
                     | Allie a-> if a.moves>0 then putpixel vert (x+i) (y+i) else putpixel vert_clair (x+i) (y+i)
         done
    done
done

(*let's draw the UI*)


let draw_UI_main ent cursor score=
	couleur blanc noir;
    ignore (mvaddstr 5 30 (Printf.sprintf "HP: %d/%d" ent.hp ent.hpmax));
    ignore (mvaddstr 0 30 (Printf.sprintf "Score: %d" !score));
    ignore (mvaddstr 5 50 (Printf.sprintf "MP: %d/%d" ent.mp ent.mpmax));
    ignore (mvaddstr 10 35 (Printf.sprintf "Que faire ? :"));
    if ent.can_move then ignore (mvaddstr 15 30 (Printf.sprintf "D : Se deplacer"));
    if ent.can_attack then ignore (mvaddstr 18 30 (Printf.sprintf "A : Attaquer"));
    ignore (mvaddstr 21 30 (Printf.sprintf "S : Changer de personnage"));
    ignore (mvaddstr 25 30 (Printf.sprintf "F : Finir le tour"));
    if cursor = 1 then putpixel rouge_clair 3 45 else putpixel rouge_clair 3 50



let draw_skill_range (n,v) skill map =
	let rec aux_draw_skill_range (n,v) l =
	match l with 
	|[] -> []
	|(x,y)::q -> begin if (v+y>=0 && v+y<=24) && (n+x>=0 && n+x<=24) then if map.(v+y).(n+x) = Vide then putpixel blanc (n+x) (v+y); aux_draw_skill_range (n,v) q end
in
	ignore (aux_draw_skill_range (n,v) skill.range)


let draw_UI_Attaques ent =
	let draw_skill x y skill letter= 
	match skill with
	|Nulle -> ignore (mvaddstr x y (Printf.sprintf "" ));
	|Existe s ->begin
			    ignore (mvaddstr x y (Printf.sprintf "%c : %s" letter s.name));
			    ignore(mvaddstr (x+2) (y+1) (Printf.sprintf "%s" s.description ))
			end
			in
	match ent.skills with
	|a,b,c,d -> begin 
					draw_skill 3 30 a 'U';
					draw_skill 10 30 b 'I';
					draw_skill 17 30 c 'O';
					draw_skill 24 30 d 'P'
				end;
	ignore(mvaddstr 30 10 (Printf.sprintf "Une fois une attaque selectionnee appuyez sur R pour la tourner"))
	
(* Enleve le Existe pour pouvoir manipuler les skills *)
let unwrap_skill s =
	match s with
	| Existe x -> x
	| Nulle -> {name = "";
				description = "";
				range = [];
				dmg = 0;
				cost = 0}
	
(* Deplacement d'une entite *)
let move_entite map ent dx dy =
	if ent.moves > 0 then
		if map.(ent.y+dy).(ent.x+dx) = Vide then begin
			ent.x <- ent.x + dx;
			ent.y <- ent.y + dy;
			ent.moves <- ent.moves - 1;
			match map.(ent.y-dy).(ent.x-dx) with
			| Allie _ -> map.(ent.y).(ent.x) <- Allie ent; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Ennemi _ -> map.(ent.y).(ent.x) <- Ennemi ent; map.(ent.y-dy).(ent.x-dx) <- Vide;
			(* Les match suivants n'ont pas de sens mais produisent une erreur s'ils n'existent pas (pattern non vérifié) *)
			| Mur -> map.(ent.y).(ent.x) <- Mur; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Vide -> map.(ent.y).(ent.x) <- Vide; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Bord -> map.(ent.y).(ent.x) <- Bord; map.(ent.y-dy).(ent.x-dx) <- Vide;
		end
	
(* Trouver un skill parmi la liste des skills avec un indice *)
let find_skill ent n =
	match ent.skills with
	| (s1,s2,s3,s4) -> if n = 1 then s1 else if n = 2 then s2 else if n = 3 then s3 else if n = 4 then s4 else Nulle


let take_dmg ent dmg map score = 
	ent.hp <- ent.hp - dmg;
	if ent.hp <= 0 then begin map.(ent.y).(ent.x) <- Vide; score:= !score + 1 end

let skill_range_circle ent s map=
	let rec aux x y range visited =
		if map.(y).(x) = Vide || !visited = [] then begin
			visited := (x,y)::(!visited);
			if range > 0 then  begin
				aux (x+1) y (range-1) visited;
				aux (x-1) y (range-1) visited;
				aux x (y+1) (range-1) visited;
				aux x (y-1) (range-1) visited;
			end;
		end;
	in
	let range = ref [] in
	match s.range with
	| [] -> [];
	| (x,y)::q -> if q=q && y=y then aux ent.x ent.y x range;
	!range

(* Rotation de la visée d'un skill*)
let rotate_skill s =
	let rec aux range =
		match range with
		| [] -> []
		| (x,y)::q -> (-y,x)::(aux q)
	in
	s.range <- aux s.range
						

(* Fonction qui fait les degats d'un skill *)
let use_skill ent s map score = 
	let rec use_skill_aux dmg range =
		match range with
		| [] -> ()
		| (x,y)::q -> match map.(ent.y+y).(ent.x+x) with
					  | Ennemi e -> begin take_dmg e dmg map score; use_skill_aux dmg q end
					  | Allie a -> begin take_dmg a dmg map score; use_skill_aux dmg q end
					  | _ -> use_skill_aux dmg q;
	in 
	let move = unwrap_skill s in
	if move = blast_skill then use_skill_aux move.dmg (skill_range_circle ent move map)
	else use_skill_aux move.dmg move.range;
	ent.mp <- ent.mp - move.cost

let activate_skill ent skill_sel atk_ready map score=
	if !ent.mp >= (unwrap_skill (find_skill !ent !skill_sel)).cost then begin
		use_skill !ent (find_skill !ent !skill_sel) map score;
		skill_sel := 0;
		!ent.can_attack <- false;
		atk_ready := false;
	end
	
let enemies_turn enemies map score =
	let enemy_turn e target=
		for i=1 to 3 do
			e.moves <- e.moves + 1;
			if i=i then 
				match (pathfinder map e.x e.y target.x target.y) with
				| (x,y) -> move_entite map e (x-e.x) (y-e.y);
			draw_board map 25;
			Unix.sleepf 0.03;
        	assert(refresh ());
		done;
		e.can_attack <- true;
		match e.skills with
		| (s1,s2,s3,s4) ->  for i=1 to 4 do
								if in_list (target.x-e.x,target.y-e.y) (unwrap_skill s1).range && e.can_attack then activate_skill (ref e) (ref 1) (ref true) map score;
						   		if in_list (target.x-e.x,target.y-e.y) (unwrap_skill s2).range && e.can_attack then activate_skill (ref e) (ref 2) (ref true) map score;
						   		if in_list (target.x-e.x,target.y-e.y) (unwrap_skill s3).range && e.can_attack then activate_skill (ref e) (ref 3) (ref true) map score;
						   		if in_list (target.x-e.x,target.y-e.y) (unwrap_skill s4).range && e.can_attack then activate_skill (ref e) (ref 4) (ref true) map score ;
								if i=i then begin
									for j=1 to 4 do
										rotate_skill (unwrap_skill (find_skill e j))
									done;
								end
						    done;
	in
	for i=0 to Array.length enemies -1 do
		if (dist enemies.(i).x enemies.(i).y mage.x mage.y) < (dist enemies.(i).x enemies.(i).y warrior.x warrior.y) then enemy_turn enemies.(i) mage
		else enemy_turn enemies.(i) warrior
	done

(*
let ennemy_spawn m turn = 
	let check = ref true in 
	for i = 0 to turn + 2 do


done
*)


let _ =
Random.self_init ();

    attroff(A.color);
    let score = ref 0 in
    let h = match get_size ()with (x,_) -> x in
    let continue = ref true in
    let frames = ref 0 in

    let m = mapofstring cases in
	m.(mage.y).(mage.x) <- Allie mage;
	m.(warrior.y).(warrior.x) <- Allie warrior;
	m.(e1.y).(e1.x) <- Ennemi e1;
	m.(e2.y).(e2.x) <- Ennemi e2;
	m.(e3.y).(e3.x) <- Ennemi e3;
	generate_walls m;
	let attack_ready = ref false and skill_selected = ref 0 in
	let a = ref mage in
	let all_enemies = [|e1;e2;e3|] in
    (* boucle principale *)
    while !continue do
        (* le clear permet de ne pas avoir de problèmes avec les animations
           mais c'est lent. Dans beaucoup d'application il vaut mieux
           réecrire par dessus ce qui a changé *)
        clear ();
        couleur rouge noir;
		
        draw_board m h;
		putpixel bleu !a.x !a.y;
		
		couleur blanc noir;
		if !attack_ready then begin 
			draw_UI_Attaques !a;
			match (find_skill !a !skill_selected) with
			| Existe s -> draw_skill_range (!a.x,!a.y) s m;
			| Nulle -> skill_selected := 0;
		end
		else if !a.moves = 0 then draw_UI_main !a 1 score;

        incr frames;

        (* on attend un peu 1/10s *)
        Unix.sleepf 0.05;
        (* on rafraichit l'écran *)
        assert(refresh ());

        (* on regarde si on a appuyé sur une touche *)
        let c = getch () in
        if c >= 0
        then begin
            (* c'est le cas on fait une action en conséquence *)
            (* attention certaines touches sont spéciales et ne
               peuvent pas être converties en caractère comme les
               touches fléchées *)
            if c = Key.down then move_entite m !a 0 1
            else if c = Key.up then move_entite m !a 0 (-1)
            else if c = Key.left then move_entite m !a (-1) 0 
            else if c = Key.right then move_entite m !a 1 0
            else (match char_of_int c with
                (* des caractères normaux *)
                | 'q' -> continue := false
				| 'd' -> if !a.moves > 0 then begin 
							!a.moves <- 0;
							!a.can_move <- false
						end
						else if !a.can_move && (not !attack_ready) then begin 
							!a.moves <- 5;
							!a.can_move <- false 
						end
				| 'a' -> if !a.moves=0 && !a.can_attack then attack_ready := true;
				| 'u' -> if !attack_ready then begin
							if !skill_selected = 1 then activate_skill a skill_selected attack_ready m score
							else skill_selected := 1;
						end
				| 'i' -> if !attack_ready then begin
							if !skill_selected = 2 then begin
								use_skill !a (find_skill !a !skill_selected) m score;
								skill_selected := 0;
								!a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 2;
						end
				| 'o' -> if !attack_ready then begin
							if !skill_selected = 3 then begin
								use_skill !a (find_skill !a !skill_selected) m score;
								skill_selected := 0;
								!a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 3;
						end
				| 'p' -> if !attack_ready then begin
							if !skill_selected = 4 then begin
								use_skill !a (find_skill !a !skill_selected) m score;
								skill_selected := 0;
								!a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 4;
						end
				| 'r' -> if !skill_selected > 0 then rotate_skill (unwrap_skill (find_skill !a !skill_selected))
				| 's' -> if !a = mage then a:=warrior else a:=mage;
				| 'f' -> mage.can_move <- true; mage.can_attack <- true; warrior.can_move <- true; warrior.can_attack <- true; enemies_turn all_enemies m score
                | _ -> ())
        end
    done;

    endwin ();