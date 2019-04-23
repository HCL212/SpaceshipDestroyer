(* TODO: enemies grid coordinates has to be updated each loop
 * TOOD: player type, enemy type, passed into a state of the game
 * TODO: player ship lasers in list
 * TODO: enemy lasers shooting back (maybe)
 * TODO: if player is hit game over
 * TODO: if enemy is hit, remove
 * TODO: if all enemies killed, win
 *)

open Tsdl

(* module of game *)
module State = struct

    (* default screen dimensions *)
    let screen_width = 900
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
        enemies : sprite list list;
        (* bullets: sprite list; *)
    }

    (* row coordinates *)
    let row_of_coords y x0 dx n =
        let rec next x remaining acc =
            if remaining > 0 then
                next (x+.dx) (remaining-1) ((x,y) :: acc)
            else acc
        in
        next x0 n []

    (* column coordinates *)
    let col_of_coords y0 x dx n =
        let rec next y remaining acc =
            if remaining > 0 then
                next (y+.dx) (remaining-1) ((x,y) :: acc)
            else acc
        in
        next y0 n []

    (* make initial state of the game *)
    let make screen_w screen_h =
        (* source image has the player ship in the top-left 150x200
        * and the enemy is in another 150x200 region adjacent to the right. *)
        let player_rect = Sdl.Rect.create 0 0 150 200 in
        let enemy_rect = Sdl.Rect.create 150 0 150 200 in
        let create_enemies_in_grid rect columns rows =
            List.iter rect in 
        { 
            screen_w = screen_width;
            screen_h = screen_height;
            player = { 
                x = screen_w /. 2.0; 
                y = screen_h -. 16.0; 
                rect = player_rect
            }
            enemies = create_enemies_in_grid enemy_rect 11 5
        }
end

(* user key presses *)
type event = 
    Left | Right | Exit
   
let rec get_event () =
  
    let e = Sdl.Event.create () in
  
    if Sdl.poll_event(Some e) then
        match Sdl.Event.get e Sdl.Event.typ |> Sdl.Event.enum with
        | `Quit -> Some Exit 
        | `Key_down -> 
        
            let keycode = Sdl.Event.get e Sdl.Event.keyboard_keycode in
            let repeat = Sdl.Event.get e Sdl.Event.keyboard_repeat in

            if repeat = 0 then (* not a repeat *)
            begin
                if keycode = Sdl.K.q then Some Exit
                else if keycode = Sdl.K.left || keycode = Sdl.K.a then Some Left
                else if keycode = Sdl.K.right || keycode = Sdl.K.d then Some Right
                else None
            end
            else
                None
        | _ -> get_event () (* if it's an event of another type, get the next event *)
    else
        None

let round x = int_of_float (floor (0.5 +. x))

let draw win rend tex state =
    (* draw the background *)
    ignore (Sdl.set_render_draw_color rend 32 32 32 255);
    ignore (Sdl.render_clear rend);

    (* draw the enemy *)
    let tex_rect = State.t.enemies in        

    (* draw the ship *)
    let tex_rect = State.t.player in
    let dst_rect = Sdl.Rect.create (round x - 10) (round y - 50) 70 70 in
    ignore (Sdl.render_copy ~src:tex_rect ~dst:dst_rect rend tex);
    
    Sdl.render_present rend

(* creates the windows and textures *)
let run win rend tex =
  
    (* loop takes the state of the game *)
    let rec loop time_prev st =

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
            let st2 = 
                let force = 200.0 in
                match opt with
                | None -> st
                | Some Left -> st (* State.push (-.force, 0.0) 2.0 st *)
                | Some Right -> st (* State.push (force, 0.0) 2.0 st *)
                | Some _ -> st
                in

                (* if the game state should update with time, update it *)
                let st3 = State.update (w, h) dt st2 in

                    (* draw *)
                    draw win rend tex st3;

                    (* call the loop again *)
                    loop time_cur st3
                in

                (* create initial state with screen width + height and loop for next state/frame *)
                let initial_state = State.make (screen_width, screen_height) in
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

        ( match Sdl.create_window ~w:screen_width ~h:screen_height "Spaceship Destroyer" Sdl.Window.(shown + input_focus) with 
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

