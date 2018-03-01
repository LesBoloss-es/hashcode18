
open Common

let one_bonus multiplier time pos problem ride =
  (* time where we can be at start *)
  let time_at_start = time + distance pos ride.start in

  (* consider the ride only if we can make it in time *)
  if time_at_start + ride.duration <= ride.latest_finish then
    (
      (* what we earn *)
      (
        (* bonus (or 0) *)
        (multiplier * if time_at_start <= ride.earliest_start then problem.bonus else 0)
        (* duration *)
        + ride.duration
      )
      -
        (* what we loose *)
        (
          (* the time it takes to get at start *)
          distance pos ride.start
          (* the time that we wait at start *)
          + (max 0 (ride.earliest_start - time_at_start))
          (* the time it takes *)
          + ride.duration
        )
    )
  else
    min_int
