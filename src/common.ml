
type intersection = int * int

let distance (x1, y1) (x2, y2) =
  abs (x2 - x1) + abs (y2 - y1)

type time = int

type ride =
  { start : intersection ;
    finish : intersection ;
    duration : int ;
    earliest_start : time ;
    latest_finish : time }

(* Problem *)

type problem =
  { rows : int ;
    columns : int ;
    vehicles : int ;
    rides : ride array ;
    bonus : int ;
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
              let start = (int_of_string row_start, int_of_string column_start) in
              let finish = (int_of_string row_finish, int_of_string column_finish) in
              rides.(ride) <-
                { start ; finish ;
                  duration = distance start finish ;
                  earliest_start = int_of_string earliest_start;
                  latest_finish = int_of_string latest_finish }
            )
         | _ -> assert false
       done;
       close_in ic;
       { rows = int_of_string rows ;
         columns = int_of_string columns ;
         vehicles = int_of_string vehicles ;
         rides ;
         bonus = int_of_string bonus ;
         steps = int_of_string steps }
     )
  | _ -> assert false

(* Solution *)

type solution =
  int list array
(* Array associating a vehicle number to the list of its rides *)

let solution_to_file filename solution =
  let oc = open_out filename in
  for vehicle = 0 to Array.length solution - 1 do
    output_string oc
      (
        (string_of_int (List.length solution.(vehicle)))
        ^ " "
        ^ (String.concat " " (List.map string_of_int solution.(vehicle)))
        ^ "\n"
      )
  done;
  close_out oc

let score problem solution =
  let score = ref 0 in
  let ride_assigned = Array.make (Array.length problem.rides) (-1) in
  for vehicle = 0 to problem.vehicles - 1 do
    let position = ref (0, 0) in
    let time = ref 0 in
    List.iter
      (fun ride_number ->
        if ride_assigned.(ride_number) <> -1 then
          Format.eprintf "ERROR: trying to assign ride %d to vehicles %d and %d@." ride_number vehicle ride_assigned.(ride_number)
        else
          ride_assigned.(ride_number) <- vehicle;
        
        let ride = problem.rides.(ride_number) in

        (* Go to the starting point *)
        time := !time + (distance !position ride.start);

        (* Are we there on time? If yes, wait for the earliest start *)
        let bonus_deserved =
          if !time <= ride.earliest_start then
            (
              time := ride.earliest_start;
              true
            )
          else
            false
        in

        (* Do the ride *)
        time := !time + ride.duration ;

        (* Check that we are there in time *)
        if !time <(*FIXME:strict?*) ride.latest_finish && !time <=(*FIXME:large?*) problem.steps then
          score := !score + (if bonus_deserved then problem.bonus else 0) + ride.duration
        else
          Format.eprintf "WARNING: vehicle %d finished ride %d too late@." vehicle ride_number
      )
      solution.(vehicle)
  done;
  !score
