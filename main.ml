open Curses


type attaque = Nulle | Existe of string * (int*int) list

type entite = {
    mutable mpmax : int;
    mutable hpmax : int;
    mutable hp :  int;
    mutable mp: int;
    mutable pos: int * int;
    mutable skills: attaque * attaque * attaque * attaque
}

type tile = Mur |  Vide| Allie of entite | Ennemi of entite 




(*definitions*)

let cases = "##################################-------################-----------############---------------#########-----------------#######-------------------######-------------------#####---------------------####---------------------###-----------------------##-----------------------##-----------------------##-----------------------##-----------------------##-----------------------##-----------------------###---------------------####---------------------#####-------------------######-------------------#######-----------------#########---------------############-----------################-------##################################"

let map_create () = Array.make_matrix 25 25 Vide

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

(*
let a   = {
    hp = 5;
    mp = 2;
    pos =(0,0);
    skills= (b,b,b,b)
}
*)
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



let couleur texte fond =
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
                     |Allie _-> putpixel vert_clair (x+i) (y+i) 
         done
    done
done



    

let _ =
    attroff(A.color);
    let h = match get_size ()with (x,_) -> x in
        let continue = ref true in
    let frames = ref 0 in
    let w = h in
    let t_x = ref (w/2) in
    let t_y = ref (h/2) in

    (* boucle principale *)
    while !continue do
        (* le clear permet de ne pas avoir de problèmes avec les animations
           mais c'est lent. Dans beaucoup d'application il vaut mieux
           réecrire par dessus ce qui a changé *)
        clear ();
        let m = mapofstring cases in
        draw_board m h;



        (* on écrit un texte qui peut se déplacer avec
           les fléches *)
        couleur blanc noir;
        ignore (mvaddstr !t_y !t_x (Printf.sprintf "Texte en %dx%d a deplacer avec les fleches" !t_x !t_y));

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
            if c = Key.down then t_y := min (!t_y+1) (h-2)
            else if c = Key.up then t_y := max (!t_y-1) 1
            else if c = Key.left then t_x := max (!t_x-1) 1 
            else if c = Key.right then t_x := min (!t_x+1) (w-2)
            else (match char_of_int c with
                (* des caractères normaux *)
                | 'q' -> continue := false
                | _ -> ())
        end
    done;

    endwin ()
