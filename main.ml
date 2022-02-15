open Curses


type skill = {
	name : string;
	description: string;
	range : (int*int) list
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
	mutable can_move: bool
}

type tile = Mur |  Vide| Allie of entite | Ennemi of entite 




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
	range = [(-2,0);(-1,-1);(-1,0);(-1,1);(0,-2);(0,-1);(0,1);(0,2);(1,-1);(1,0);(1,1);(2,0)]
}

let ray_skill = {
	name = "Rayon";
	description = "Attaque droit devant le personnage (Portee: 5)";
	range = [(1,0);(2,0);(3,0);(4,0);(5,0)]
}

let slash_skill = {
	name = "Taillade";
	description = "Coup devant le personnage (Portee: 1)";
	range = [(1,-1);(1,0);(1,1)]
}

let healAura_skill = {
	name = "Aura de Soin";
	description = "Soin autour du personnage (Portee: 2)";
	range = [(-2,0);(-1,-1);(-1,0);(-1,1);(0,-2);(0,-1);(0,1);(0,2);(1,-1);(1,0);(1,1);(2,0)]
}

let a   = {
	hpmax = 15;
	mpmax = 10;
    hp = 5;
    mp = 2;
    x = 9;
	y = 9;
    skills= (Existe blast_skill,Existe ray_skill,Existe slash_skill,Existe healAura_skill);
	can_move = false;
}

(*définitions des couleurs*)

let noir = 0
let gris = noir+1
let blanc = gris+1
let rouge = blanc+1
let rouge_clair = rouge+1
let vert = rouge_clair+1
let vert_clair = vert+1
let bleu = vert_clair+1
let bleu_clair = bleu+1


let cree_couleurs () =
    assert(init_color noir 0 0 0);
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
         if s.[x + (y * 25)]  = '#' then m.(y).(x) <- Mur else m.(y).(x) <- Vide
    done
done;
m


let draw_board m h =
    let mult = h / 25 in 
    for y = 0 to 24 do
        for x = 0 to 24 do 
            for i = 0 to mult-1 do 
                     match m.(y).(x) with
                     |Vide -> putpixel noir (x+i) (y+i)
                     |Mur -> putpixel  gris (x+i) (y+i)
                     | Ennemi _-> putpixel rouge (x+i) (y+i)
                     |Allie a-> if a.can_move then putpixel vert (x+i) (y+i) else putpixel vert_clair (x+i) (y+i)
         done
    done
done

(*let's draw the UI*)


let draw_UI_main ent cursor =
	couleur blanc noir;
    ignore (mvaddstr 5 30 (Printf.sprintf "HP: %d/%d" ent.hp ent.hpmax));
    ignore (mvaddstr 5 50 (Printf.sprintf "MP: %d/%d" ent.mp ent.mpmax));
    ignore (mvaddstr 10 35 (Printf.sprintf "Que faire ? :"));
    ignore (mvaddstr 15 30 (Printf.sprintf "Attaquer :    A"));
    ignore (mvaddstr 18 30 (Printf.sprintf "Se deplacer : M"));
    if cursor = 1 then putpixel rouge_clair 3 45 else putpixel rouge_clair 3 50



let draw_skill_range (n,v)  skill=
	let rec aux_draw_skill_range (n,v) l =
	match l with 
	|[] -> []
	|(x,y)::q -> begin putpixel blanc (n+x) (v+y); aux_draw_skill_range (n,v) q end
in
	ignore (aux_draw_skill_range (n,v) skill.range)


let draw_UI_Attaques ent =
	let draw_skill x y skill = 
	match skill with
	|Nulle -> ignore (mvaddstr x y (Printf.sprintf "" ));
	|Existe s ->begin
			    ignore (mvaddstr x y (Printf.sprintf "%s" s.name));
			    ignore(mvaddstr (x+3) (y+1) (Printf.sprintf "%s" s.description ))
			end
			in
	match ent.skills with
	|a,b,c,d -> begin 
				draw_skill 5 30 a;
				draw_skill 15 30 b;
				draw_skill 25 30 c;
				draw_skill 35 30 d
end
	
let move_entite map ent dx dy =
	if ent.can_move then
		if map.(ent.y+dy).(ent.x+dx) = Vide then begin
			ent.x <- ent.x + dx;
			ent.y <- ent.y + dy;
		end
	




    

let _ =
Random.self_init ();

    attroff(A.color);
    let h = match get_size ()with (x,_) -> x in
        let continue = ref true in
    let frames = ref 0 in
    (*let w = h in
    let t_x = ref (w/2) in
    let t_y = ref (h/2) in*)

    (* boucle principale *)
    while !continue do
        (* le clear permet de ne pas avoir de problèmes avec les animations
           mais c'est lent. Dans beaucoup d'application il vaut mieux
           réecrire par dessus ce qui a changé *)
        clear ();
        couleur rouge noir;
        let m = mapofstring cases in
		m.(a.y).(a.x) <- Allie a;
        draw_board m h;

		draw_skill_range (7,6) blast_skill;
		draw_skill_range (7,10) ray_skill;
		draw_skill_range (15,15) slash_skill;
		draw_skill_range (13,6) healAura_skill;
		
		couleur blanc noir;
		draw_UI_main a 1;
        (* on écrit un texte qui peut se déplacer avec
           les fléches *)
        (*couleur blanc noir;
        ignore (mvaddstr !t_y !t_x (Printf.sprintf "Texte en %dx%d a deplacer avec les fleches" !t_x !t_y));*)

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
				| 'm' -> a.can_move <- (not a.can_move)
                | _ -> ())
        end
    done;

    endwin ()

