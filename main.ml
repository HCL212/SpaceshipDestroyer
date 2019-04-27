 (* TODO: if enemies reach bottom game over
 * TODO: if enemy is hit, remove enemy and bullet (List.filter)
 * TODO: if all enemies killed, win
 *)

open Tsdl

(* module of game *)
module State = struct

    (* default screen dimensions *)
    let screen_width = 1200
    let screen_height = 900

    (* sprite *)
    type sprite = {
        x : float;
        y : float;
        rect : Tsdl.Sdl.rect;
    }
    
    (* game state *)
    type t = {
        screen_w : int;
        screen_h : int;
        player : sprite;
        enemies : sprite list;
        enemy_offset : (float * float);
        enemy_forward: bool;
        bullets : sprite list;
    }

    (* create list of coordinates for the "grid" of enemies *)
    (* float -> float -> float -> float -> int -> int -> (float * float) list *)
    let grid_coords x0 y0 dx dy cols rows =
        let rec row_of_coords x y curr_col acc =
            if curr_col > 0 then
                row_of_coords (x +. dx) y (curr_col - 1) ((x,y) :: acc)
            else acc
        in        
        let rec init_down y remaining_rows acc =
            if remaining_rows > 0 then begin
                let acc = row_of_coords x0 y cols acc in
                init_down (y +. dy) (remaining_rows - 1) acc;
            end
            else acc
        in
        init_down y0 rows []

    (* check collision and remove from bullet + enemy lists 
     * returns sprites for sprite_list1 with removed elements *)
    let rec check_bullets bullets enemies offset_x offset_y =
        let ox,oy = offset_x, offset_y in
        match bullets, enemies with
        | [], [] -> []
        | [], _ -> []
        | _, [] -> bullets
        | (h::t), (hh::tt)->
                if (h.x > hh.x +. ox) && (h.x < hh.x +. ox +. 50.0) && (h.y < hh.y +. oy +. 50.0) 
                    then check_bullets t enemies ox oy
                else h :: check_bullets t enemies ox oy

    let rec check_enemies enemies bullets offset_x offset_y =
        let ox,oy = offset_x, offset_y in
        match enemies, bullets with
        | [], [] -> []
        | [], _ -> []
        | _, [] -> enemies
        | (h::t), (hh::tt)->
                if (h.x +. ox < hh.x) && (h.x +. ox +. 50.0 > hh.x) && (h.y +. oy +. 50.0 > hh.y) 
                    then check_enemies t bullets ox oy
                else h :: check_enemies t bullets ox oy
   
    (* make initial state of the game *)
    (* int -> int -> State.t *)
    let make () =
        (* source image has the player ship in the top-left 150x200
        * and the enemy is in another 150x200 region adjacent to the right. *)
        let player_rect = Sdl.Rect.create 0 0 150 200 in
        let enemy_rect = {
            x = 0.0;
            y = 0.0;
            rect = Sdl.Rect.create 150 0 150 200;
            } 
        in

        (* starting x-coord, starting y-coord, x increase of next, y increase of next, columns, rows *)
        let enemy_coords = grid_coords 0.0 0.0 55.0 55.0 10 5 in

        (* initial game state.t values *)
        { 
            screen_w = screen_width;
            screen_h = screen_height;
            player = {x = (float_of_int screen_width) /. 2.0 -. 27.0; y = (float_of_int screen_height) -. 50.0; rect = player_rect; }; 
            enemies = enemy_coords |> List.map (fun (x,y) -> {enemy_rect with x;y});
            enemy_offset = 0.0, 0.0;
            enemy_forward = true;
            bullets = [];
        }        

    (* move the ship left and right 
     * return State.t *)
    let push movement state =
        let new_player_sprite = 
            {
                state.player with
                x = state.player.x +. movement
            } in

        (* player boundary + movement *)
        if state.player.x +. movement > (float_of_int screen_width) -. 55.0 || state.player.x +. movement < 0.0 then
            state
        else
            {
                state with
                player = new_player_sprite; 
            }

    (* shoot bullets
     * return State.t *)
    let shoot state =
        (* add bullet at player position *)
        let bullet_rect = {
            x = state.player.x +. 12.5;
            y = state.player.y;
            rect = Sdl.Rect.create 300 0 160 200
            } 
        in

        let new_bullet_list = (bullet_rect :: state.bullets) in
        {
            state with
            bullets = new_bullet_list;
        }
                
    (* update player/enemy/bullets per loop *)
    let update state =
        
        (* enemy movement
         * goes back and forth from left to right
         * goes down after it "hits a wall" 
         * enemy_forw : true = go right, false = go left *)
        let x,y = state.enemy_offset in
        let forward_limit = (float_of_int screen_width) /. 2.0 in
        let enem_travel_speed = 7.0 in
        let enem_down_speed = 30.0 in

        let enemy_forw =
            if x < 0.1 then true
            else if x > forward_limit then false
            else state.enemy_forward in
        let x = 
            if enemy_forw = true then x +. enem_travel_speed 
            else x -. enem_travel_speed in
        let y = 
            if x > forward_limit then y +. enem_down_speed
            else if x < 0.1 then y +. enem_down_speed
            else y +. 0.0 in

        (* bullet movement
         * bullets1 = y-coord update (moving bullets up) 
         * bullets2 = remove bullets that go off top of the screen *)
        let bullets1 = List.map (fun s -> {s with y = s.y -. 4.0}) state.bullets in
        let bullets2 = List.filter (fun s -> s.y > 0.0) bullets1 in
        let bullets3 = check_bullets bullets2 state.enemies x y in
        let enemies1 = check_enemies state.enemies bullets2 x y in
        
        (* new state t *)
        {
            state with
            player = state.player;
            enemy_forward = enemy_forw;
            enemies = enemies1;
            enemy_offset = x, y;
            bullets = bullets3;
        }
end

(* user key presses *)
type event = 
    Left | Right | Shoot | Exit
   
let rec get_event () =
  
    let e = Sdl.Event.create () in
  
    if Sdl.poll_event(Some e) then
        match Sdl.Event.get e Sdl.Event.typ |> Sdl.Event.enum with
        | `Quit -> Some Exit 
        | `Key_down -> 
        
            let keycode = Sdl.Event.get e Sdl.Event.keyboard_keycode in
            let repeat = Sdl.Event.get e Sdl.Event.keyboard_repeat in

            if repeat = 0 then (* 0 = not a repeat, 1 = repeat *)
            begin
                if keycode = Sdl.K.q then Some Exit
                else if keycode = Sdl.K.left || keycode = Sdl.K.a then Some Left
                else if keycode = Sdl.K.right || keycode = Sdl.K.d then Some Right
                else if keycode = Sdl.K.space then Some Shoot
                else None
            end
            else None
        | _ -> get_event () (* if it's an event of another type, get the next event *)
    else
        None

(* function that rounds number down less than or equal to x *)
let round num = int_of_float (floor (0.5 +. num))

let draw win rend tex state =
    (* open module for use *)
    let open State in

    (* draw the background 
     * ignore has signature "'a -> unit" since we only want graphical results, not return value *)
    ignore (Sdl.set_render_draw_color rend 32 32 32 255);
    ignore (Sdl.render_clear rend);

    (* draw sprite generic function 
     * "?" makes the argument OPTIONAL, 
     * but we can add defaults to offset value if none provided (no longer optional since it always has a value) *)
    let draw_sprite ?(offset=0.0,0.0) width height sprite =
        let dx,dy = offset in
        let x = round (sprite.x +. dx) in
        let y = round (sprite.y +. dy) in
        let dst = Sdl.Rect.create ~x ~y ~w:width ~h:height in
        Sdl.render_copy ~src:sprite.rect ~dst rend tex |> ignore
    in

    (* draw the player *)
    draw_sprite 50 50 state.player;

    (* draw the enemy *)
    List.iter (draw_sprite ~offset:state.enemy_offset 50 50) state.enemies;

    (* draw the bullets *)
    List.iter (draw_sprite 20 20) state.bullets;

    Sdl.render_present rend

(* creates the windows and textures *)
let run win rend tex =
  
    (* loop takes the state of the game *)
    let rec loop time_prev st =
        let open State in

        Sdl.delay 10l; (* in milliseconds *)
        
    let time_cur = Int32.to_int (Sdl.get_ticks()) in
        (* elapsed time in seconds *)
        let dt = float (time_cur - time_prev) *. 0.001 in
        (* print frame rate *)
        Printf.printf "FPS: %g\n%!" (1.0 /. dt);
    
        match get_event () with
        | Some Exit -> ()
        | opt ->
            (* process one key pressed, if needed *)
            let new_state = 
                let force = 30.0 in
                match opt with
                | None -> st
                | Some Left -> State.push (-.force) st
                | Some Right -> State.push (force) st
                | Some Shoot -> State.shoot st
                | Some _ -> st
                in

            (* if the game state should update with time, update it *)
            let st3 = State.update new_state in

            (* draw *)
            draw win rend tex st3;

            (* call the loop again *)
            loop time_cur st3
            in

            (* create initial state with screen width + height and loop for next state/frame *)
            let initial_state = State.make () in
            loop (Int32.to_int (Sdl.get_ticks())) initial_state;
  
  Sdl.destroy_texture tex;
  Sdl.destroy_renderer rend;
  Sdl.destroy_window win;
  Sdl.quit (); 
  exit 0


(* main *)
let () =
  
    (* init SDL *)
    match Sdl.init Sdl.Init.video with 
    | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () -> 

        ( match Sdl.create_window ~w:1200 ~h:900 "Spaceship Destroyer" Sdl.Window.(shown + input_focus) with 
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok win -> 

            ( match Sdl.create_renderer win ~index:(-1) ~flags:Sdl.Renderer.(accelerated + presentvsync) with
            | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
            | Ok rend ->

                ( match Sdl.load_bmp "img/sprites.bmp" with
                | Error (`Msg e)  -> Sdl.log "Load bmp error: %s" e; exit 1
                | Ok surf ->

                    let win_px_fmt = Sdl.get_window_pixel_format win in

                        (* convert surface pixel format *)
                        let surf = match Sdl.convert_surface_format surf win_px_fmt with
                        | Ok s -> s
                        | _ -> surf
                        in
                
                            (* set the transparent color (0,255,255) *)
                            begin match Sdl.alloc_format win_px_fmt with
                            | Ok fmt -> 
                                let bg_color_uint = Sdl.map_rgb fmt 0 0 0 in
                                ignore(Sdl.set_color_key surf true bg_color_uint);
                                Sdl.free_format fmt
                            | _ -> ()
                            end;

                            (* create texture out of the surface and start main loop *)
                            begin match Sdl.create_texture_from_surface rend surf with
                            | Ok tex -> 
                                (* all is good, run the program *)
                                run win rend tex

                  | Error (`Msg e) -> Sdl.log "Creating texture error: %s" e; exit 1
                end
                )
            )
        )


