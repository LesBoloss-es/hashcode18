open Common


let find_bonus t cur_pos p rides_booked =
  let r = ref (-1) in
  let end_t = ref t in
  let find i ride =
    if !r = -1 &&
       (not rides_booked.(i)) &&
       t + distance ride.start cur_pos <= ride.earliest_start then begin
      r := i;
      end_t := ride.earliest_start + ride.duration
    end
  in
  Array.iteri find p.rides ;
  !r, !end_t


let find_any t cur_pos p rides_booked =
  let r = ref (-1) in
  let end_t = ref t in
  let find i ride =
    if !r = -1 &&
       (not rides_booked.(i)) &&
       t + distance ride.start cur_pos + ride.duration < ride.latest_finish then begin
      r := i;
      end_t := t + distance ride.start cur_pos + ride.duration
    end
  in
  Array.iteri find p.rides ;
  !r, !end_t


let to_solution cars : solution =
  Array.map (fun (_, _, q, _) -> List.rev q) cars


let i_like_bonuses (p : problem) : solution =
  let cars = Array.make p.vehicles (0, (0, 0), [], false) in
  let rides_booked = Array.make (Array.length p.rides) false in
  while Array.exists (fun (_, _, _, finished) -> not finished) cars do
    for i = 0 to p.vehicles - 1 do
      let t, pos, queue, finished = cars.(i) in
      if not finished then begin
        let r, end_t = find_bonus t pos p rides_booked in
        let r, end_t =
          if r = -1
          then find_any t pos p rides_booked
          else r, end_t
        in
        if r >= 0 then begin
          rides_booked.(r) <- true;
          let end_pos = p.rides.(r).finish in
          let queue = r :: queue in
          cars.(i) <- (end_t, end_pos, queue, false) ;
        end else begin
          cars.(i) <- t, pos, queue, true
        end
      end
    done;
  done;
  to_solution cars
