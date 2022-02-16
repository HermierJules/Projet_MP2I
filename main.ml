open Curses


type skill = {
	name : string;
	description: string;
	range : (int*int) list;
	dmg : int
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

let pathfinder map x1 y1 x2 y2 =
    let dist x1 y1 x2 y2 =
        let distance = Float.sqrt ((float_of_int ((x1-x2)*(x1-x2)))+.(float_of_int ((y1-y2)*(y1-y2)))) in
        distance
    in
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
	range = [(-2,0);(-1,-1);(-1,0);(-1,1);(0,-2);(0,-1);(0,1);(0,2);(1,-1);(1,0);(1,1);(2,0)];
	dmg = 3;
}

let ray_skill = {
	name = "Rayon";
	description = "Attaque droit devant le personnage (Portee: 5)";
	range = [(1,0);(2,0);(3,0);(4,0);(5,0)];
	dmg = 4;
}

let slash_skill = {
	name = "Taillade";
	description = "Coup devant le personnage (Portee: 1)";
	range = [(1,-1);(1,0);(1,1)];
	dmg = 6;
}

let healAura_skill = {
	name = "Aura de Soin";
	description = "Soin autour du personnage (Portee: 2)";
	range = [(-2,0);(-1,-1);(-1,0);(-1,1);(0,-2);(0,-1);(0,1);(0,2);(1,-1);(1,0);(1,1);(2,0)];
	dmg = -4;
}

let punch_skill = {
	name = "Frappe";
	description = "Coup puissant droit devant le personnage";
	range = [(1,0)];
	dmg = 4;
}

let a = {
	hpmax = 15;
	mpmax = 10;
    hp = 15;
    mp = 10;
    x = 4;
	y = 9;
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
    x = 7;
	y = 9;
    skills= (Existe punch_skill, Nulle,Nulle,Nulle);
	moves = 0;
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
    skills= (Existe punch_skill, Nulle,Nulle,Nulle);
	moves = 0;
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



let couleur texte  fond =
    attron (A.color_pair paires.(texte).(fond))



(* affiche un pixel *)
let putpixel col x y =
    couleur col col;
    assert (mvaddch y x (int_of_char ' '))

let ligne_horiz col x1 x2 y =
    for x = x1 to x2 do
        putpixel col x y
    done


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


let draw_UI_main ent cursor =
	couleur blanc noir;
    ignore (mvaddstr 5 30 (Printf.sprintf "HP: %d/%d" ent.hp ent.hpmax));
    ignore (mvaddstr 5 50 (Printf.sprintf "MP: %d/%d" ent.mp ent.mpmax));
    ignore (mvaddstr 10 35 (Printf.sprintf "Que faire ? :"));
    if ent.can_move then ignore (mvaddstr 15 30 (Printf.sprintf "Se deplacer :    D"));
    if ent.can_attack then ignore (mvaddstr 18 30 (Printf.sprintf "Attaquer :       A"));
    ignore (mvaddstr 21 30 (Printf.sprintf "Finir le tour :  F"));
    if cursor = 1 then putpixel rouge_clair 3 45 else putpixel rouge_clair 3 50



let draw_skill_range (n,v) skill map =
	let rec aux_draw_skill_range (n,v) l =
	match l with 
	|[] -> []
	|(x,y)::q -> begin if map.(v+y).(n+x) = Vide then putpixel blanc (n+x) (v+y); aux_draw_skill_range (n,v) q end
in
	ignore (aux_draw_skill_range (n,v) skill.range)


let draw_UI_Attaques ent =
	let draw_skill x y skill letter= 
	match skill with
	|Nulle -> ignore (mvaddstr x y (Printf.sprintf "" ));
	|Existe s ->begin
			    ignore (mvaddstr x y (Printf.sprintf "%c : %s" letter s.name));
			    ignore(mvaddstr (x+3) (y+1) (Printf.sprintf "%s" s.description ))
			end
			in
	match ent.skills with
	|a,b,c,d -> begin 
				draw_skill 5 30 a 'U';
				draw_skill 15 30 b 'I';
				draw_skill 25 30 c 'O';
				draw_skill 35 30 d 'P'
end
	
let move_entite map ent dx dy =
	if ent.moves > 0 then
		if map.(ent.y+dy).(ent.x+dx) = Vide then begin
			ent.x <- ent.x + dx;
			ent.y <- ent.y + dy;
			ent.moves <- ent.moves - 1;
			match map.(ent.y).(ent.x) with
			| Allie _ -> map.(ent.y).(ent.x) <- Allie ent; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Ennemi _ -> map.(ent.y).(ent.x) <- Ennemi ent; map.(ent.y-dy).(ent.x-dx) <- Vide;
			(* Les match suivants n'ont pas de sens mais produisent une erreur s'ils n'existent pas (pattern non vérifié) *)
			| Mur -> map.(ent.y).(ent.x) <- Mur; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Vide -> map.(ent.y).(ent.x) <- Vide; map.(ent.y-dy).(ent.x-dx) <- Vide;
			| Bord -> map.(ent.y).(ent.x) <- Bord; map.(ent.y-dy).(ent.x-dx) <- Vide;
		end
	
let find_skill ent n =
	match ent.skills with
	| (s1,s2,s3,s4) -> if n = 1 then s1 else if n = 2 then s2 else if n = 3 then s3 else if n = 4 then s4 else Nulle


let take_dmg ent dmg map = 
	ent.hp <- ent.hp - dmg;
	if ent.hp <= 0 then map.(ent.y).(ent.x) <- Vide

let use_skill ent s map = 
	let rec use_skill_aux dmg range =
		match range with
		| [] -> ()
		| (x,y)::q -> match map.(ent.y+y).(ent.x+x) with
					  | Ennemi e -> begin take_dmg e dmg map; use_skill_aux dmg q end
					  | Allie a -> begin take_dmg a dmg map; use_skill_aux dmg q end
					  | _ -> use_skill_aux dmg q;
	in 
	match s with
	| Existe move -> use_skill_aux move.dmg move.range
	| Nulle -> use_skill_aux 0 []



let _ =
Random.self_init ();

    attroff(A.color);
    let h = match get_size ()with (x,_) -> x in
        let continue = ref true in
    let frames = ref 0 in

    let m = mapofstring cases in
	m.(a.y).(a.x) <- Allie a;
	m.(e1.y).(e1.x) <- Ennemi e1;
	m.(e2.y).(e2.x) <- Ennemi e2;
	generate_walls m;
	let attack_ready = ref false in
	let skill_selected = ref 0 in
    (* boucle principale *)
    while !continue do
        (* le clear permet de ne pas avoir de problèmes avec les animations
           mais c'est lent. Dans beaucoup d'application il vaut mieux
           réecrire par dessus ce qui a changé *)
        clear ();
        couleur rouge noir;
		m.(a.y).(a.x) <- Allie a;
        draw_board m h;
		
		couleur blanc noir;
		if !attack_ready then begin 
			draw_UI_Attaques a;
			match (find_skill a !skill_selected) with
			| Existe s -> draw_skill_range (a.x,a.y) s m;
			| Nulle -> skill_selected := 0;
		end
		else if a.moves = 0 then draw_UI_main a 1;

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
            if c = Key.down then move_entite m a 0 1
            else if c = Key.up then move_entite m a 0 (-1)
            else if c = Key.left then move_entite m a (-1) 0 
            else if c = Key.right then move_entite m a 1 0
            else (match char_of_int c with
                (* des caractères normaux *)
                | 'q' -> continue := false
				| 'd' -> if a.can_move && (not !attack_ready) then begin a.moves <- 5; a.can_move <- false end
				| 'a' -> if a.moves=0 && a.can_attack then attack_ready := true;
				| 'u' -> if !attack_ready then begin
							if !skill_selected = 1 then begin
								use_skill a (find_skill a !skill_selected) m;
								skill_selected := 0;
								a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 1;
						end
				| 'i' -> if !attack_ready then begin
							if !skill_selected = 2 then begin
								use_skill a (find_skill a !skill_selected) m;
								skill_selected := 0;
								a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 2;
						end
				| 'o' -> if !attack_ready then begin
							if !skill_selected = 3 then begin
								use_skill a (find_skill a !skill_selected) m;
								skill_selected := 0;
								a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 3;
						end
				| 'p' -> if !attack_ready then begin
							if !skill_selected = 4 then begin
								use_skill a (find_skill a !skill_selected) m;
								skill_selected := 0;
								a.can_attack <- false;
								attack_ready := false;
							end
							else skill_selected := 4;
						end
				| 'f' -> a.can_move <- true;a.can_attack <- true; 
                | _ -> ())
        end
    done;

    endwin ();