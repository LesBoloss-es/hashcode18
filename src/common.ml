
type intersection = int * int

let distance (x1, y1) (x2, y2) =
  abs (x2 - x1) + abs (y2 - y1)

type time = int

type ride =
  { start : intersection ;
    finish : intersection ;
    earliest_start : time ;
    latest_finish : time }

(* Problem *)

type problem =
  { rows : int ;
    columns : int ;
    vehicles : int ;
    rides : ride array ;
    steps : int }

let problem_of_file filename =
  let ic = open_in filename in
  match String.split_on_char ' ' (input_line ic) with
  | [rows; columns; vehicles; number_of_rides; bonus; steps] ->
     (
       let number_of_rides = int_of_string number_of_rides in
       let rides = Array.make number_of_rides (Obj.magic ()) in
       for ride = 0 to number_of_rides - 1 do
         match String.split_on_char ' ' (input_line ic) with
         | [row_start; column_start; row_finish; column_finish; earliest_start; latest_finish] ->
            (
              rides.(ride) <-
                { start = (int_of_string row_start, int_of_string column_start) ;
                  finish = (int_of_string row_finish, int_of_string column_finish) ;
                  earliest_start = int_of_string earliest_start;
                  latest_finish = int_of_string latest_finish }
            )
         | _ -> assert false
       done;
       { rows = int_of_string rows ;
         columns = int_of_string columns ;
         vehicles = int_of_string vehicles ;
         rides ;
         steps = int_of_string steps }
     )
  | _ -> assert false

(* Solution *)

type solution
